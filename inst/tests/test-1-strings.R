context("String functions: wrap and reverse")

test_that("string wrap and reverse works", {
      long_string <- "the quick brown fox jumps over the lazy dog"
      long_string_2 <- c("the quick brown fox jumps over the lazy dog", 
          "Why is the sentence the quick brown fox jumps over the lazy dog so unique? ")
      
      expect_that(str_wrap(long_string, 10), 
          equals("the quick\nbrown fox\njumps\nover the\nlazy dog"))
      
      expect_that(str_wrap(long_string_2, 20), 
          equals(c("the quick brown fox\njumps over the lazy\ndog",
                  "Why is the sentence\nthe quick brown fox\njumps over the lazy\ndog so unique?")))
      
      expect_that(str_reverse(long_string), 
          equals("god yzal eht revo spmuj xof nworb kciuq eht"))
    })


#context("String functions: common and unique")
#
#qs0 <- "Q"
#qs01 <- c("Q", "Q1")
#qs1 <- c("Q1", "Q1")
#qs2 <- c("Q1", "Q2")
#qs3 <- c("1", "2", "3")
#qs4 <- c("Q_1", "Q_2", "Q_3") 
#
#test_that("str_common_unique works", {
#     
#     str_common <- function(x) str_common_unique(x)$common
#     str_unique <- function(x) str_common_unique(x)$unique
#     
#     expect_that(str_common(qs1), is_a("character"))
#     expect_that(str_unique(qs1), is_a("character"))
#
#     expect_that(str_common(qs0), equals("Q"))
#     expect_that(str_unique(qs0), equals(""))
#     
#     expect_that(str_common(qs01), equals("Q"))
#     expect_that(str_unique(qs01), equals(c("", "1")))
#     
#     expect_that(str_common(qs1), equals("Q1"))
#     expect_that(str_unique(qs1), equals(c("", "")))
#     
#     expect_that(str_common(qs2), equals("Q"))
#     expect_that(str_unique(qs2), equals(c("1", "2")))
#     
#     expect_that(str_common(qs3), equals(""))
#     expect_that(str_unique(qs3), equals(c("1", "2", "3")))
#     
#     expect_that(str_common(qs4), equals("Q_"))
#     expect_that(str_unique(qs4), equals(c("1", "2", "3")))
#
#   })

