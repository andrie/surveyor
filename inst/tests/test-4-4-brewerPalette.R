# Test functionality using testthat library
# 
# Author: Andrie
#------------------------------------------------------------------------------

context("brewerPalette")

{
  
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
  
  #names_cqrw <- c("cbreak", "question", "response", "weight")
  
  varlabels(q_data) <- q_text
  q_data <- as.surveydata(q_data, renameVarlabels=TRUE)
  s <- as.surveyor(q_data, crossbreak=list(group=q_data$crossbreak), q_data$weight)
}

# Helper function to extract expected colours
expectedColours <- function(x, n=3, brewerPalette=x$defaults$brewerPalette, keep=n){
  sort(brewer.pal(n=n, name=brewerPalette)[seq_len(keep)])
}

# Helper function to get actual plot colours
getFillColours <- function(p){
  pp <- ggplot_build(p)
  sort(unique(pp$data[[1]][, "fill"]))
}


test_that("default brewerPalette works with default palette", {
      
      x1 <- surveyPlot(s, "Q1")[[1]]$plot
      x2 <- surveyPlot(s, "Q2")[[1]]$plot
      x3 <- surveyPlot(s, "Q3")[[1]]$plot
      x4 <- surveyPlot(s, "Q4")[[1]]$plot
      
      expColours <- expectedColours(s, keep=2)
      
      expect_equal(getFillColours(x1), expColours)
      expect_equal(getFillColours(x2), expColours)
      expect_equal(getFillColours(x3), expColours)
      expect_equal(getFillColours(x4), expColours)
      
    })

test_that("default brewerPalette works with different plot types", {
      
      x1 <- surveyPlot(s, "Q3", plotFunction=plotBar, brewerPalette="BuPu")[[1]]$plot
      x2 <- surveyPlot(s, "Q3", plotFunction=plotColumn, brewerPalette="BuPu")[[1]]$plot
      x3 <- surveyPlot(s, "Q3", plotFunction=plotNetScore, brewerPalette="BuPu")[[1]]$plot
      x4 <- surveyPlot(s, "Q3", plotFunction=plotPoint, brewerPalette="BuPu")[[1]]$plot
      
      expColours <- expectedColours(brewerPalette="BuPu", n=2, keep=2)
      expColours
      x1
      x2
      x3
      x4
      getFillColours(x1)
      getFillColours(x2)
      getFillColours(x3)
      getFillColours(x4)
      
      expect_equal(getFillColours(x1), expColours)
      expect_equal(getFillColours(x2), expColours)
      expect_equal(getFillColours(x3), expColours)
      expect_equal(getFillColours(x4), expColours)
      
    })


test_that("default brewerPalette works with custom palette", {
      
      x1 <- surveyPlot(s, "Q1", brewerPalette="BuPu")[[1]]$plot
      x2 <- surveyPlot(s, "Q2", brewerPalette="BuPu")[[1]]$plot
      x3 <- surveyPlot(s, "Q3", brewerPalette="BuPu")[[1]]$plot
      x4 <- surveyPlot(s, "Q4", brewerPalette="BuPu")[[1]]$plot
      
      expColours <- expectedColours(brewerPalette="BuPu", n=3, keep=2)
      expColours
      x1
      x2
      x3
      x4
      getFillColours(x1)
      getFillColours(x2)
      getFillColours(x3)
      getFillColours(x4)
      
      expect_equal(getFillColours(x1), expColours)
      expect_equal(getFillColours(x2), expColours)
      expect_equal(getFillColours(x3), expColours)
      expect_equal(getFillColours(x4), expColours)
      
    })



      