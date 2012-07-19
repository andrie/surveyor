# Test functionality using testthat library
# 
# Author: Andrie
#------------------------------------------------------------------------------

{
  path <- tempdir()
  latexPath <- file.path(path, "latex")
  dir.create(latexPath, recursive=TRUE, showWarnings=FALSE)
  graphPath <- file.path(latexPath, "graphics")
  dir.create(graphPath, recursive=TRUE, showWarnings=FALSE)
  
  q_data <- data.frame(
      Q1=c("Yes", "No", "Yes", "Yes"),
      Q4_1 = c(1, 2, 1, 2), 
      Q4_3=c(3, 4, 4, 3), 
      Q4_2=c(5, 5, 6, 6), 
      crossbreak=c("A", "A", "B", "B"), 
      crossbreak2=c("D", "E", "D", "E"),
      weight=c(0.9, 1.1, 0.8, 1.2)
  )
  q_text <- c("Question 1", 
      "Question 4: red", "Question 4: blue", "Question 4: green", 
      "crossbreak",
      "crossbreak2",
      "weight")
  names(q_text) <- names(q_data)
  
  names_cqrw <- c("cbreak", "question", "response", "weight")
  
  varlabels(q_data) <- q_text
  q_data <- as.surveydata(q_data, renameVarlabels=FALSE)
  s <- as.surveyor(q_data, q_data$crossbreak, q_data$weight)
}

#------------------------------------------------------------------------------

context("Other")

test_that("surveyPlot returns NULL", {
			
			r <- surveyPlot(s, "Q1", statsBin, plotBar)
      expect_is(r, "list")
      expect_is(r[[1]], "surveyorPlot")
      
      r <- surveyPlot(s, "Q4", statsBin, plotBar)
      expect_is(r, "list")
      expect_is(r[[1]], "surveyorPlot")
      
		})

#------------------------------------------------------------------------------



test_that("surveyPlot works with multiple crossbreaks", {
      s2 <- as.surveyor(q_data, list(q_data$crossbreak, q_data$crossbreak2), q_data$weight)
      
			r <- surveyPlot(s2, "Q1", statsBin, plotBar)
      expect_is(r, "list")
      expect_is(r[[1]], "surveyorPlot")
      
		})


