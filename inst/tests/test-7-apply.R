# Test functionality using testthat library
# 
# Author: Andrie
#------------------------------------------------------------------------------

context("Apply")


{
  path <- tempdir()
  latexPath <- file.path(path, "latex")
  dir.create(latexPath, recursive=TRUE, showWarnings=FALSE)
  graphPath <- file.path(latexPath, "graphics")
  dir.create(graphPath, recursive=TRUE, showWarnings=FALSE)
  
  sats <- c("Very dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Very satisfied")
    
  q_data <- data.frame(
      Q1=c("Yes", "No", "Yes", "Yes"),
      Q2_1=c("Yes", "Yes", "Yes", "Yes"),
      Q2_2=c("Yes", "Yes", "Yes", "No"),
      Q2_3=c("Yes", "Yes", "No", "No"),
      Q2_4=c("Yes", "No", "No", "No"),
      Q3_1=factor(c("Very dissatisfied", "Dissatisfied", "Very satisfied", "Neutral"), levels=sats),
      Q3_2=factor(c("Very satisfied", "Ssatisfied", "Very dissatisfied", "Neutral"), levels=sats),
      Q4_1 = c(1, 2, 1, 2), 
      Q4_3=c(3, 4, 4, 3), 
      Q4_2=c(5, 5, 6, 6), 
      crossbreak=c("A", "A", "B", "B"), 
      crossbreak2=c("D", "E", "D", "E"),
      Respondents=c(1, 1, 1, 1),
      weight=c(0.9, 1.1, 0.8, 1.2)
  )
  q_text <- c("Question 1 has a really very long title", 
      "Question 2 - Multiple yes/no : Sub_q1",
      "Question 2 - Multiple yes/no : Sub_q2",
      "Question 2 - Multiple yes/no : Sub_q3",
      "Question 2 - Multiple yes/no : Sub_q4",
      "Question 3 - Satisfaction: Sub_q1",
      "Question 3 - Satisfaction: Sub_q2",
      "Question 4 also has a long title: lovely red colour, the shade of sunset", "Question 4 also has a long title: blue", "Question 4 also has a long title: green", 
      "crossbreak",
      "crossbreak2",
      "Respondents",
      "weight")
  names(q_text) <- names(q_data)
  
  names_cqrw <- c("cbreak", "question", "response", "weight")
  
  varlabels(q_data) <- q_text
  q_data <- as.surveydata(q_data, renameVarlabels=FALSE)
  s <- as.surveyor(q_data, crossbreak=list(breaks=q_data$crossbreak), q_data$weight)
}

#------------------------------------------------------------------------------


test_that("surveyPlot returns NULL", {
      
      defs <- list(
          Q1 = list(x=s, qid="Q1"),
          Q2 = list(x=s, qid="Q2"),
          Q3 = list(x=s, qid="Q3"),
          Q4 = list(x=s, qid="Q3")
      )
      
      
      
      x <- surveyPlotApply(defs, s)
      #browser()
      sapply(x, function(xx)class(xx[[1]]) , USE.NAMES=FALSE)
      sapply(x, function(xx)class(xx[[1]]) %in% "ggplotmod", USE.NAMES=FALSE)
      
      expect_is(x, "list")
      expect_true(all(sapply(x, function(xx)class(xx) =="surveyorPlot", USE.NAMES=FALSE)))
      lapply(x, print)
      
		})

#------------------------------------------------------------------------------

