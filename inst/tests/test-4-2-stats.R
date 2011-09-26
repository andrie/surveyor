# Test functionality using testthat library
# 
# Author: Andrie
#------------------------------------------------------------------------------

{
  path <- file.path("f:","git","surveyor","test")
  latex_path <- file.path(path, "latex")
  graph_path <- file.path(latex_path, "graphics")
  
  
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
  
  sbraid <- as.braid(
      pathLatex    = latex_path,
      pathGraphics = graph_path
  )
  q_data <- as.surveydata(q_data)
  varlabels(q_data) <- q_text
  s <- as.surveyor(q_data, q_data$crossbreak, q_data$weight, braid=sbraid)
}


#==============================================================================

context("statsBin")

test_that("statsBin works with single question", {
      
      test <- statsBin(codeQuickArray(s, "Q1"))
      rest <- structure(
          list(
              cbreak = structure(
                  1:2, 
                  .Label = c("A", "B"), 
                  class = "factor"), 
              response = structure(
                  c(1L, 1L),
                  .Label = c("Yes", "No"),
                  class = c("ordered", "factor")), 
              value = c(0.9, 2)), 
          .Names = c("cbreak", "response", "value"), 
          row.names = 1:2, 
          class = "data.frame")
      
      
      expect_is(test, "surveyorStats")
      expect_equal(test$data, rest)
    })
      
test_that("statsBin works with array question", {
      
      test <- statsBin(codeQuickArray(s, "Q4"))
      rest <- structure(
          list(
              cbreak = structure(
                  c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L), 
                  .Label = c("A", "B"), 
                  class = "factor"), 
              question = structure(c(2L,2L, 3L, 1L, 1L, 2L, 2L, 3L, 1L, 1L), 
                  .Label = c("red", "blue", "green"),
                  class = c("ordered", "factor")), 
              response = c(3, 4, 5, 1, 2, 3, 4, 6, 1, 2), 
              value = c(0.9, 1.1, 2, 0.9, 1.1, 1.2, 0.8, 2, 0.8, 1.2)), 
          .Names = c("cbreak", "question", "response", "value"), 
          row.names = c(NA, 10L), 
          class = "data.frame")
      
      expect_is(test, "surveyorStats")
      expect_equal(test$data, rest)
      
    })

#==============================================================================

context("statsCentral")

test_that("statsCentral works with single question", {
      
      test <- statsCentral(codeQuickArray(s, "Q4_1"))
      rest <- structure(
          list(
              cbreak = structure(
                  1:2, 
                  .Label = c("A", "B"), 
                  class = "factor"), 
              value = c(1.55, 1.60)), 
          row.names = 1:2, 
          class = "data.frame")
      
      
      expect_is(test, "surveyorStats")
      expect_equal(test$data, rest)
    })

test_that("statsCentral works with array question", {
      
      test <- statsCentral(codeQuickArray(s, "Q4"))
      rest <- structure(
          list(
              cbreak = structure(
                  c(1L, 1L, 1L, 2L, 2L, 2L), 
                  .Label = c("A", "B"), 
                  class = "factor"), 
              question = structure(c(1L, 2L, 3L, 1L, 2L, 3L), 
                  .Label = c("blue", "green", "red"),
                  class = c("factor")), 
              value = c(3.55, 5, 1.55, 3.4, 6, 1.6)), 
          .Names = c("cbreak", "question", "value"), 
          row.names = c(NA, 6L), 
          class = "data.frame")
      
      expect_is(test, "surveyorStats")
      expect_equal(test$data, rest)
      
    })



#==============================================================================

context("statsMean")

test_that("statsMean works with single question", {
      
      test <- statsMean(codeQuickArray(s, "Q4_1"))
      rest <- structure(
          list(
              cbreak = structure(
                  1:2, 
                  .Label = c("A", "B"), 
                  class = "factor"), 
              value = c(1.55, 1.60)), 
          row.names = 1:2, 
          class = "data.frame")
      
      
      expect_is(test, "surveyorStats")
      expect_equal(test$data, rest)
    })

test_that("statsMean works with array question", {
      
      test <- statsMean(codeQuickArray(s, "Q4"))
      rest <- structure(
          list(
              cbreak = structure(
                  c(1L, 1L, 1L, 2L, 2L, 2L), 
                  .Label = c("A", "B"), 
                  class = "factor"), 
              question = structure(c(1L, 2L, 3L, 1L, 2L, 3L), 
                  .Label = c("blue", "green", "red"),
                  class = c("factor")), 
              value = c(3.55, 5, 1.55, 3.4, 6, 1.6)), 
          .Names = c("cbreak", "question", "value"), 
          row.names = c(NA, 6L), 
          class = "data.frame")
      
      expect_is(test, "surveyorStats")
      expect_equal(test$data, rest)
      
    })


#==============================================================================

context("statsMedian")

test_that("statsMedian works with single question", {
      
      test <- statsMedian(codeQuickArray(s, "Q1"))
      rest <- structure(
          list(
              cbreak = structure(
                  1:2, 
                  .Label = c("A", "B"), 
                  class = "factor"), 
              value = c(1.45, 2.00)), 
          row.names = 1:2, 
          class = "data.frame")
      
      
      expect_is(test, "surveyorStats")
      expect_equal(test$data, rest)
    })

test_that("statsMedian works with array question", {
      
      test <- statsMedian(codeQuickArray(s, "Q4"))
      rest <- structure(
          list(
              cbreak = structure(
                  c(1L, 1L, 1L, 2L, 2L, 2L), 
                  .Label = c("A", "B"), 
                  class = "factor"), 
              question = structure(c(1L, 2L, 3L, 1L, 2L, 3L), 
                  .Label = c("blue", "green", "red"),
                  class = c("factor")), 
              value = c(3.55, 5, 1.55, 3.4, 6, 1.6)), 
          .Names = c("cbreak", "question", "value"), 
          row.names = c(NA, 6L), 
          class = "data.frame")
      
      expect_is(test, "surveyorStats")
      expect_equal(test$data, rest)
      
    })

