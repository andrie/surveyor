# Test functionality using testthat library
# 
# Author: Andrie
#------------------------------------------------------------------------------


context("Code functions")


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
      crossbreak=factor(c("A", "A", "B", "B")), 
      crossbreak2=factor(c("D", "E", "D", "E")),
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
  s <- as.surveyor(q_data, crossbreak=list(breaks=q_data$crossbreak), q_data$weight)
}

test_that("codeGuess works", {
      test1 <- codeGuess(s, "Q1")
      rest1 <- codeQuickArray(s, "Q1")
      expect_equal(test1, rest1)

      test2 <- codeGuess(s, "Q4")
      rest2 <- codeQuickArray(s, "Q4")
      expect_equal(test2, rest2)
    })


test_that("codeQuickArray works", {
      test1 <- codeQuickArray(s, "Q1")
      rest1 <- structure(
          list(
              cbreak = structure(
                  c(1L, 1L, 2L, 2L), 
                  .Label = c("A", "B"), 
                  class = "factor"), 
              question = structure(c(1L, 1L, 1L, 1L), 
                  .Label = "1", 
                  #class = c("ordered", "factor")),
                  class = "factor"),
              response = structure(c(2L,1L, 2L, 2L), 
                  .Label = c("No", "Yes"), 
                  class = "factor"), 
              weight = c(0.9, 1.1, 0.8, 1.2)
          ), 
        .Names = c("cbreak", "question", "response", "weight"), 
        row.names = c(NA, 4L), 
        class = "data.frame")
      
      expect_is(test1, "surveyorCode")
      expect_equal(test1$data, rest1)
      
      test2 <- codeQuickArray(s, "Q4")
      rest2 <- structure(
          list(
              cbreak = structure(
                  c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L), 
                  .Label = c("A", "B"), 
                  class = "factor"), 
              question = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L), 
                  .Label = c("red", "blue", "green"), 
                  #class = c("ordered", "factor")), 
                  class = "factor"),
              response = c(1, 2, 1, 2, 3, 4, 4, 3, 5, 5, 6, 6), 
              weight = c(0.9, 1.1, 0.8, 1.2, 0.9, 1.1, 0.8, 1.2, 0.9, 1.1, 0.8, 1.2)
          ), 
        .Names = c("cbreak", "question", "response", "weight"), 
        row.names = c(NA, 12L), 
        class = "data.frame")
      
      expect_is(test2, "surveyorCode")
      expect_equal(test2$data, rest2)
      
      
      test3 <- codeQuickArray(s, "Q4_1")
      rest3 <- structure(
          list(
              cbreak = structure(c(1L, 1L, 2L, 2L), 
                  .Label = c("A", "B"), 
                  class = "factor"), 
              question = structure(c(1L, 1L, 1L, 1L), 
                  .Label = "1", 
                  class = "factor"), 
              response = c(1, 2, 1, 2), 
              weight = c(0.9, 1.1, 0.8, 1.2)), 
          .Names = c("cbreak", "question", "response", "weight"), 
          row.names = c(NA, 4L), class = "data.frame")
      
      expect_is(test3, "surveyorCode")
      expect_equal(test3$data, rest3)
      
      
      expect_error(is.null(codeQuickArray(s, "Q999")))
      
    })





