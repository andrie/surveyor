# Test functionality using testthat library
# 
# Author: Andrie
#------------------------------------------------------------------------------

{
  path <- tempdir()
  latexPath <- file.path(path, "latex")
  dir.create(latexPath, recursive=TRUE)
  graphPath <- file.path(latexPath, "graphics")
  dir.create(graphPath, recursive=TRUE)  
  
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
  
  sbraid <- as.braid(path = latexPath)
  q_data <- as.surveydata(q_data)
  varlabels(q_data) <- q_text
  s <- as.surveyor(q_data, q_data$crossbreak, q_data$weight, braid=sbraid)
}


context("Plot functions - ggplot")

test_that("plotBar works as expected", {
      
      p <- plotBar(statsBin(codeQuickArray(s, "Q1")))
      expect_is(p, "surveyorPlot")
      expect_is(p$plot, "ggplot")
      expect_is(print(p$plot)$plot, "ggplot")
      
      p <- plotBar(statsBin(codeQuickArray(s, "Q4")))
      expect_is(p, "surveyorPlot")
      expect_is(p$plot, "ggplot")
      expect_is(print(p$plot)$plot, "ggplot")
      
    })

test_that("plotPoint works as expected", {
      
      p <- plotPoint(statsBin(codeQuickArray(s, "Q1")))
      expect_is(p, "surveyorPlot")
      expect_is(p$plot, "ggplot")
      expect_is(print(p$plot)$plot, "ggplot")
      
      p <- plotPoint(statsBin(codeQuickArray(s, "Q4")))
      expect_is(p, "surveyorPlot")
      expect_is(p$plot, "ggplot")
      expect_is(print(p$plot)$plot, "ggplot")
      
    })

test_that("plotColumn works as expected", {
      
      p <- plotColumn(statsBin(codeQuickArray(s, "Q1")))
      expect_is(p, "surveyorPlot")
      expect_is(p$plot, "ggplot")
      expect_is(print(p$plot)$plot, "ggplot")
      
      p <- plotColumn(statsBin(codeQuickArray(s, "Q4")))
      expect_is(p, "surveyorPlot")
      expect_is(p$plot, "ggplot")
      expect_is(print(p$plot)$plot, "ggplot")
      
    })

#------------------------------------------------------------------------------

context("Plot functions - lattice")

s <- surveyorUpdateDefaults(s, fastgraphics = TRUE)

test_that("plotBar works as expected", {
      
      p <- plotBar(statsBin(codeQuickArray(s, "Q1")))
      expect_is(p, "surveyorPlot")
      expect_is(p$plot, "trellis")
      
      p <- plotBar(statsBin(codeQuickArray(s, "Q4")))
      expect_is(p, "surveyorPlot")
      expect_is(p$plot, "trellis")
      
    })

test_that("plotColumn works as expected", {
      
      p <- plotColumn(statsBin(codeQuickArray(s, "Q1")))
      expect_is(p, "surveyorPlot")
      expect_is(p$plot, "trellis")
      
      p <- plotColumn(statsBin(codeQuickArray(s, "Q4")))
      expect_is(p, "surveyorPlot")
      expect_is(p$plot, "trellis")
      
    })

unlink(path, recursive=TRUE)

