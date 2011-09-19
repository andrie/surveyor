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


context("Stats functions")

test_that("stat functions work with codeSingle", {
      
      scode <- codeSingle(s, "Q1")
      
      p <- statsBin(scode)
      expect_is(p, "surveyorStats")

    })

test_that("stat functions work with codeArray", {
			
      scode <- codeArray(s, "Q4")
      
      p <- statsBin(scode)
      expect_is(p, "surveyorStats")
      
		})

