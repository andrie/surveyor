# 
# Author: Andrie
#------------------------------------------------------------------------------






context("Validation of allNA and allNull functions")
xl_allNA <- list(
    a = c(NA, NA),
    b = c(NA, NA)
)

xdf_allNA <- data.frame(
    a = c(NA, NA),
    b = c(NA, NA)
)

x_allNA <- c(NA, NA)

#----------

xl_not_allNA <- list(
    a = c(1, NA),
    b = c(NA, 2)
)

xdf_not_allNA <- data.frame(
    a = c(1, NA),
    b = c(NA, 2)
)

x_not_allNA <- c(1, NA)


#------------------------------------------------------------------------------

xl_allNull <- list(
    a = c(NULL, NULL),
    b = c(NULL, NULL)
)

xdf_allNull <- data.frame(
    a = c(NULL, NULL),
    b = c(NULL, NULL)
)

x_allNull <- c(NULL, NULL)

#----------

xl_not_allNull <- list(
    a = c(1, NULL),
    b = c(NULL, 2)
)

xdf_not_allNull <- data.frame(
    a = c(1, NULL),
    b = c(NULL, 1)
)

x_not_allNull <- c(1, NA)


test_that("allNA passes correct value", {
      
      expect_that(allNA(x_allNA), equals(TRUE))
      expect_that(allNA(xl_allNA), equals(TRUE))
      expect_that(allNA(xdf_allNA), equals(TRUE))
      
      expect_that(allNA(x_not_allNA), equals(FALSE))
      expect_that(allNA(xl_not_allNA), equals(FALSE))
      expect_that(allNA(xdf_not_allNA), equals(FALSE))
    })

test_that("allNull passes correct value", {
      
      expect_that(allNull(x_allNull), equals(TRUE))
      expect_that(allNull(xl_allNull), equals(TRUE))
      expect_that(allNull(xdf_allNull), equals(TRUE))
      
      expect_that(allNull(x_not_allNull), equals(FALSE))
      expect_that(allNull(xl_not_allNull), equals(FALSE))
      expect_that(allNull(xdf_not_allNull), equals(FALSE))
    })


