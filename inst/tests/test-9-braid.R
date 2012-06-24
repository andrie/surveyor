# 
# Author: Andrie
#------------------------------------------------------------------------------


path <- tempdir()
latexPath <- file.path(path, "latex")
dir.create(latexPath, recursive=TRUE)
graphPath <- file.path(latexPath, "graphics")
dir.create(graphPath, recursive=TRUE)

sinkfile   <- "surveyor_test.tex"


q_data <- data.frame(
    Q1=c("Yes", "No", "Yes", "Yes"),
    Q4_1 = c(1, 2, 1, 2), 
    Q4_3=c(3, 4, 4, 3), 
    Q4_2=c(5, 5, 6, 6), 
    crossbreak=c("A", "A", "B", "B"), 
    crossbreak2=c("D", "E", "D", "E"),
    weight=c(0.9, 1.1, 0.8, 1.2)
)
varlabels(q_data) <- c("Question 1", 
    "Question 4: red", "Question 4: blue", "Question 4: green", 
    "crossbreak",
    "crossbreak2",
    "weight")

q_data <- as.surveydata(q_data)

names_cqrw <- c("cbreak", "question", "response", "weight")

sbraid <- as.braid(path = latexPath)

context("Test output to Latex")

test_that("working folders are empty", {
      if(!identical(list.files(graphPath), "")){
        lapply(list.files(graphPath), function(x)file.remove(file.path(graphPath, x)))
      }          
      if (file.exists(file.path(latexPath, sinkfile))){
        file.remove(file.path(latexPath, sinkfile))
      }
      expect_false(file.exists(file.path(graphPath, "fig0001.pdf")))
      expect_false(file.exists(file.path(graphPath, "fig0002.pdf")))
      
    })

test_that("surveyorPlot works in Latex", {
      tbraid <- as.braid(path = latexPath, fileInner=sinkfile)
      
      t_defaults <- surveyorDefaults(outputType = "latex")
      
      t <- as.surveyor(q_data, q_data$crossbreak, q_data$weight, t_defaults, braid=tbraid)
      
      braidHeading(tbraid, "Test")
      surveyorPlot(t, "Q1", codeSingle, statsBin, plotBar)
      surveyorPlot(t, "Q4", codeArray, statsBin, plotBar)
      braidSave(tbraid)
      
      expect_true(file.exists(file.path(latexPath, sinkfile)))
      expect_true(file.exists(file.path(graphPath, "Q1_.pdf")))
      expect_true(file.exists(file.path(graphPath, "Q4_.pdf")))
      
    })

unlink(path, recursive=TRUE)