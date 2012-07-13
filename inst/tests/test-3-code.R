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
  
  q_data <- as.surveydata(q_data)
  varlabels(q_data) <- q_text
  s <- as.surveyor(q_data, q_data$crossbreak, q_data$weight)
}


context("Code functions")

#test_that("codeSingle works", {
#      scode <- codeSingle(s, "Q1")
#      expect_is(scode, "surveyorCode")
#      expect_is(scode$data, "data.frame")
#      expect_equal(names(scode$data), names_cqrw)
#      expect_equal(nrow(scode$data), 4)
#      
#    })
#
#test_that("codeArray works", {
#      scode <- codeArray(s, "Q4")
#      expect_is(scode, "surveyorCode")
#      expect_is(scode$data, "data.frame")
#      expect_equal(names(scode$data), names_cqrw)
#      expect_equal(nrow(scode$data), 12)
#      
#    })


test_that("codeGuess works", {
      test <- codeGuess(s, "Q1")
      rest <- codeQuickArray(s, "Q1")
      expect_equal(test, rest)

      test <- codeGuess(s, "Q4")
      rest <- codeQuickArray(s, "Q4")
      expect_equal(test, rest)
    })


test_that("codeQuickArray works", {
      test <- codeQuickArray(s, "Q1")
      rest <- structure(
          list(
              cbreak = structure(
                  c(1L, 1L, 2L, 2L), 
                  .Label = c("A", "B"), 
                  class = "factor"), 
              question = structure(c(1L, 1L, 1L, 1L), 
                  .Label = "1", 
                  class = c("ordered", "factor")), 
              response = structure(c(2L,1L, 2L, 2L), 
                  .Label = c("No", "Yes"), 
                  class = "factor"), 
              weight = c(0.9, 1.1, 0.8, 1.2)
          ), 
        .Names = c("cbreak", "question", "response", "weight"), 
        row.names = c(NA, 4L), 
        class = "data.frame")
      
      expect_is(test, "surveyorCode")
      expect_equal(test$data, rest)
      
      test <- codeQuickArray(s, "Q4")
      rest <- structure(
          list(
              cbreak = structure(
                  c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L), 
                  .Label = c("A", "B"), 
                  class = "factor"), 
              question = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L), 
                  .Label = c("red", "blue", "green"), class = c("ordered", "factor")), 
              response = c(1, 2, 1, 2, 3, 4, 4, 3, 5, 5, 6, 6), 
              weight = c(0.9, 1.1, 0.8, 1.2, 0.9, 1.1, 0.8, 1.2, 0.9, 1.1, 0.8, 1.2)
          ), 
        .Names = c("cbreak", "question", "response", "weight"), 
        row.names = c(NA, 12L), 
        class = "data.frame")
      
      expect_is(test, "surveyorCode")
      expect_equal(test$data, rest)
      
      expect_error(is.null(codeQuickArray(s, "Q999")))
      
    })

unlink(path, recursive=TRUE)




