# Test functionality using testthat library
# 
# Author: Andrie
#------------------------------------------------------------------------------

context("statsHelpers")


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

#==============================================================================


values1 <- c(1:5, 10)
weights1 <- rep(1, length(values1))

values2 <- c(1:5, 10)
weights2 <- seq(1, by=0.2, length.out=length(values2))

values3 <- c(1:2, NA, 4:5, 10)
weights3 <- seq(1, by=0.2, length.out=length(values2))


test_that("weightedMean works", {
      expect_equal(weightedMean(values1, weights1), 4.1666667)
      expect_equal(weightedMean(values2, weights2), 4.7777778)
      expect_equal(weightedMean(values3, weights3), 5.1052632)
    })
      
test_that("weightedMedian works", {
      expect_equal(weightedMedian(values1, weights1), 3.5)
      expect_equal(weightedMedian(values2, weights2), 4.058823548)
      expect_equal(weightedMedian(values3, weights3), 4.4705882)
    })

test_that("weightedSum works", {
      expect_equal(weightedSum(values1, weights1), 25)
      expect_equal(weightedSum(values2, weights2), 43)
      expect_equal(weightedSum(values3, weights3), 38.8)
    })

test_that("weightedCount works", {
      expect_equal(weightedCount(values1, weights1), 6)
      expect_equal(weightedCount(values2, weights2), 9)
      expect_equal(weightedCount(values3, weights3), 7.6)
    })


#context("split, apply and combine")

test_that("splitMeanCombine gives results identical to ddply",{
      
      test <- splitMeanCombine(codeGuess(s, "Q4_1")$data)
      rest <- rename(
              ddply(
                  codeGuess(s, "Q4_1")$data, 
                  .(cbreak), 
                  function(i)value=weightedMean(i$response, i$weight)),
              c("V1"="value")
          )
      expect_equal(test, rest)
      
      test <- splitMeanCombine(codeGuess(s, "Q4")$data)
      rest <- rename(
          ddply(
              codeGuess(s, "Q4")$data, 
              .(cbreak, question), 
              function(i)value=weightedMean(i$response, i$weight)),
          c("V1"="value")
      )
      expect_equal(test, rest)
    
    })


test_that("splitBinCombine gives results identical to ddply",{
      test1 <- splitBinCombine(codeGuess(s, "Q4_1")$data)
      rest1 <- ddply(
          codeGuess(s, "Q4_1")$data, 
          .(cbreak, response), 
          summarise,
          value=weightedCount(response, weight))
      
      expect_equal(test1, rest1)
      
      test2 <- splitBinCombine(codeGuess(s, "Q4")$data)
      rest2 <- ddply(
              codeGuess(s, "Q4")$data, 
              .(cbreak, question, response),
              summarise,
              value=weightedCount(response, weight))
      expect_equal(test2, rest2)
      
      
    })

test_that("splitPercentCombine gives results identical to ddply",{
      test <- splitPercentCombine(codeGuess(s, "Q4_1")$data)
      rest <- ddply(
              codeGuess(s, "Q4_1")$data, 
              .(cbreak, question), 
              summarise,
              weight=sum(weight))
      expect_equal(test, rest)
      
      test <- splitPercentCombine(codeGuess(s, "Q4")$data)
      rest <- ddply(
              codeGuess(s, "Q4")$data, 
              .(cbreak, question), 
              summarise,
              weight=sum(weight))
      expect_equal(test, rest)
      
      
    })

