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


#------------------------------------------------------------------------------

context("Stats functions")

test_that("statBin works as expected", {
      
      test <- statsBin(codeQuickArray(s, "Q1"))
      rest <- structure(list(cbreak = structure(1:2, .Label = c("A", "B"), class = "factor"), 
              response = structure(c(1L, 1L), .Label = c("Yes", "No"), class = c("ordered", 
                      "factor")), value = c(0.9, 2)), .Names = c("cbreak", "response", 
              "value"), row.names = 1:2, class = "data.frame")
      
      expect_is(test, "surveyorStats")
      expect_equal(test$data, rest)
      
      
      test <- statsBin(codeQuickArray(s, "Q4"))
      rest <- structure(list(cbreak = structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 
                      2L, 2L, 2L), .Label = c("A", "B"), class = "factor"), question = structure(c(2L, 
                      2L, 3L, 1L, 1L, 2L, 2L, 3L, 1L, 1L), .Label = c("red", "blue", 
                      "green"), class = c("ordered", "factor")), response = c(3, 4, 
                  5, 1, 2, 3, 4, 6, 1, 2), value = c(0.9, 1.1, 2, 0.9, 1.1, 1.2, 
                  0.8, 2, 0.8, 1.2)), .Names = c("cbreak", "question", "response", 
              "value"), row.names = c(NA, 10L), class = "data.frame")
      
      expect_is(test, "surveyorStats")
      expect_equal(test$data, rest)
      
    })

