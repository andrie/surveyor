# Test functionality using testthat library
# 
# Author: Andrie
#------------------------------------------------------------------------------

context("Weighted Median")

# Unit tests copied from examples in  aroma.light::weightedMedian
x <- 1:10

n <- length(x)
w <- rep(1, n)

test_that("Unweighted median is calculated correctly", {

      m1 <- median(x) # 5.5
      m2 <- weightedMedian(x) # 5.5
      
      expect_equal(m1, 5.5)
      expect_equal(m2, 5.5)
      expect_identical(m1, m2)
    
    })


test_that("Weighted median is calculated correctly", {
      
      m1 <- weightedMedian(x, w) # 5.5 (default)
      m2 <- weightedMedian(x, ties="weighted") # 5.5 (default)
      m3 <- weightedMedian(x, ties="min") # 5
      m4 <- weightedMedian(x, ties="max") # 6
      
      expect_equal(m1, 5.5)
      expect_equal(m2, 5.5)
      expect_equal(m3, 5)
      expect_equal(m4, 6)
      expect_identical(m1, m2)
      
    })
      

test_that("Adjust weights", {
      
      w[1] <- 5
      m1 <- weightedMedian(x, w) # 3.5
      y <- c(rep(0,w[1]), x[-1]) # Only possible for integer weights
      m2 <- median(y) # 3.5
      
      expect_equal(m1, 3.5)
      expect_equal(m2, 3.5)
      expect_identical(m1, m2)
      
      w[1] <- 8
      expect_equal(weightedMedian(x, w), 2) # 2

      w[1] <- Inf
      expect_equal(weightedMedian(x, w), 1) # 1

      w[1] <- 1
      w[n] <- Inf
      expect_equal(weightedMedian(x, w), 10) # 10
      
      w <- rep(0, n)
      expect_true(is.na(weightedMedian(x, w))) # NA
    })

