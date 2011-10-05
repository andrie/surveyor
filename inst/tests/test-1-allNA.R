# 
# Author: Andrie
#------------------------------------------------------------------------------






context("Validation of all_NA and all_NULL functions")
xl_all_na <- list(
    a = c(NA, NA),
    b = c(NA, NA)
)

xdf_all_na <- data.frame(
    a = c(NA, NA),
    b = c(NA, NA)
)

x_all_na <- c(NA, NA)

#----------

xl_not_all_na <- list(
    a = c(1, NA),
    b = c(NA, 2)
)

xdf_not_all_na <- data.frame(
    a = c(1, NA),
    b = c(NA, 2)
)

x_not_all_na <- c(1, NA)


#------------------------------------------------------------------------------

xl_all_null <- list(
    a = c(NULL, NULL),
    b = c(NULL, NULL)
)

xdf_all_null <- data.frame(
    a = c(NULL, NULL),
    b = c(NULL, NULL)
)

x_all_null <- c(NULL, NULL)

#----------

xl_not_all_null <- list(
    a = c(1, NULL),
    b = c(NULL, 2)
)

xdf_not_all_null <- data.frame(
    a = c(1, NULL),
    b = c(NULL, 1)
)

x_not_all_null <- c(1, NA)


test_that("all_na passes correct value", {
      
      expect_that(all_na(x_all_na), equals(TRUE))
      expect_that(all_na(xl_all_na), equals(TRUE))
      expect_that(all_na(xdf_all_na), equals(TRUE))
      
      expect_that(all_na(x_not_all_na), equals(FALSE))
      expect_that(all_na(xl_not_all_na), equals(FALSE))
      expect_that(all_na(xdf_not_all_na), equals(FALSE))
    })

test_that("all_null passes correct value", {
      
      expect_that(all_null(x_all_null), equals(TRUE))
      expect_that(all_null(xl_all_null), equals(TRUE))
      expect_that(all_null(xdf_all_null), equals(TRUE))
      
      expect_that(all_null(x_not_all_null), equals(FALSE))
      expect_that(all_null(xl_not_all_null), equals(FALSE))
      expect_that(all_null(xdf_not_all_null), equals(FALSE))
    })


