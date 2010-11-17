# This file contains test scripts, using the testthat package, developed by 
# Hadley Wickham:
# https://github.com/hadley/devtools/wiki/Testing
# 
# Author: Andrie
###############################################################################

q_string <- c("Q_1", "Q_2", "Q_3") 
q_common <- str_common_unique(q_string)$common
q_unique <- str_common_unique(q_string)$unique

long_string <- "the quick brown fox jumps over the lazy dog"
long_string_2 <- c("the quick brown fox jumps over the lazy dog", 
		"Why is the sentence the quick brown fox jumps over the lazy dog so unique? ")

qdata <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_3=c(3,4), Q4_2=c(5,6), crossbreak=c("A", "B"), weight=c(0.9, 1.1))
qtext <- c("Question 1", "Question 4: red", "Question 4: blue", "Question 4: green", "crossbreak", "weight")
names(qtext) <- c("Q1", "Q4_1", "Q4_3", "Q4_2", "crossbreak", "weight")

names_vvcw <- c("variable", "value", "crossbreak", "weight")

s_defaults <- surveyor_defaults()
s <- surveyor(qdata, qtext, s_defaults)

surveyor_initialise()


###############################################################################

context("String functions")

test_that("string functions work", {
			
	expect_that(q_common, is_a("character"))
	expect_that(q_unique, is_a("character"))
	expect_that(q_common, equals("Q_"))
	expect_that(q_unique, equals(c("1", "2", "3")))
	
	expect_that(str_wrap(long_string, 10), 
					equals("the quick\nbrown fox\njumps\nover the\nlazy dog"))
	
	expect_that(str_wrap(long_string_2, 20), 
			equals(c("the quick brown fox\njumps over the lazy\ndog",
							"Why is the sentence\nthe quick brown fox\njumps over the lazy\ndog so unique?")))
	
	expect_that(str_reverse(long_string), 
					equals("god yzal eht revo spmuj xof nworb kciuq eht"))		
})

###############################################################################

context("Question handling")

test_that("Question handling functions work", {
			
	expect_that(get_q_subquestions(qdata, "Q1"), is_a("NULL"))
	expect_that(get_q_subquestions(qdata, "Q4"), equals(c("Q4_1","Q4_3","Q4_2")))
	
	expect_that(get_qtext_unique(qdata, "Q4", qtext), equals(c("red", "blue", "green")))
	expect_that(get_qtext_common(qdata, "Q4", qtext), equals("Question 4: "))
	
	expect_that(get_qtext(s, "Q1"), equals("Question 1"))
	expect_that(get_qtext(s, "Q4"), equals("Question 4: "))
	
})
	
###############################################################################

context("Surveyor class objects")

test_that("Surveyor objects are defined properly", {
	
	expect_that(s, is_a("surveyor"))
	expect_that(s$qdata, is_a("data.frame"))
	expect_that(s$qdata, equals(qdata))
	expect_that(s$qtext, equals(qtext))
	expect_that(is.surveyor(s), equals(TRUE))
	expect_that(is.surveyor(qdata), equals(FALSE))
	expect_that(surveyor(qdata, qtext[-1]), throws_error())
	
})

###############################################################################

context("Code functions")

code <- code_array(s, "Q4")

test_that("code_array works", {
			
	expect_that(code, is_a("data.frame"))
	expect_that(names(code)[1:4], equals(names_vvcw))
	expect_that(nrow(code), equals(6))
	
})

code <- code_single(s, "Q1")

test_that("code_array works", {
			
			expect_that(code, is_a("data.frame"))
			expect_that(names(code)[1:4], equals(names_vvcw))
			expect_that(nrow(code), equals(2))
			
		})

###############################################################################

context("Plot functions")

test_that("plot functions have proper type", {

			expect_that(plot_bar_stacked(code), is_a("ggplot"))
			
		}
)

###############################################################################

rm(q_string, q_common, q_unique, long_string, long_string_2, qdata, qtext, s,
		names_vvcw, code)
