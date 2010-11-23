# This file contains test scripts, using the testthat package, developed by 
# Hadley Wickham:
# https://github.com/hadley/devtools/wiki/Testing
# 
# Author: Andrie
###############################################################################

q_string <- c("Q_1", "Q_2", "Q_3") 
q_common <- surveyor:::str_common_unique(q_string)$common
q_unique <- surveyor:::str_common_unique(q_string)$unique

long_string <- "the quick brown fox jumps over the lazy dog"
long_string_2 <- c("the quick brown fox jumps over the lazy dog", 
		"Why is the sentence the quick brown fox jumps over the lazy dog so unique? ")

qdata <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_3=c(3,4), Q4_2=c(5,6), crossbreak=c("A", "B"), weight=c(0.9, 1.1))
qtext <- c("Question 1", "Question 4: red", "Question 4: blue", "Question 4: green", "crossbreak", "weight")
names(qtext) <- c("Q1", "Q4_1", "Q4_3", "Q4_2", "crossbreak", "weight")

names_vvcw <- c("variable", "value", "crossbreak", "weight")

# ---

path <- file.path("f:","git","surveyor","test")
latex_path <- file.path(path, "latex")
graph_path <- file.path(latex_path, "graphics")
sinkfile   <- file.path(latex_path, "surveyor_test.tex")

###############################################################################

file.remove(list.files(graph_path))
if (file.exists(file.path(latex_path, sinkfile))){
	file.remove(file.path(latex_path, sinkfile))
}


s_defaults <- surveyor_defaults(
		path_latex    = latex_path,
		path_graphics = graph_path,
		output_to_latex = FALSE
)

s <- surveyor(qdata, qtext, qdata$crossbreak, qdata$weight, s_defaults)
s_count <- new_counter()

t_defaults <- surveyor_defaults(
		path_latex    = latex_path,
		path_graphics = graph_path,
		output_to_latex = TRUE,
		output_filename = sinkfile
)

t <- surveyor(qdata, qtext, qdata$crossbreak, qdata$weight, t_defaults)
t_count <- new_counter()


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
	expect_that(s$crossbreak, equals(qdata$crossbreak))
	expect_that(s$weight, equals(qdata$weight))
	expect_that(is.surveyor(s), equals(TRUE))
	expect_that(is.surveyor(qdata), equals(FALSE))
	expect_that(surveyor(qdata, qtext[-1]), throws_error())
	expect_that(surveyor_defaults(path_latex=file.path("random")), throws_error())
	expect_that(surveyor_defaults(path_graph=file.path("random")), throws_error())
	
})

###############################################################################

context("Test plumbing of plot_q")


test_that("plot_q works", {
			
	expect_that(plot_q(s, "Q1", s_count(), code_single, plot_bar), shows_message("Q1"))
	expect_that(plot_q(s, "Q1", s_count(), code_single, plot_bar), is_a("NULL"))
	expect_that(plot_q(s, "Q4", s_count(), code_array, plot_bar), shows_message("Q4"))
	expect_that(plot_q(s, "Q4", s_count(), code_array, plot_point), is_a("NULL"))
	
})

###############################################################################

context("Code as array question and plot")

code <- code_array(s, "Q4")

test_that("code_array works", {
			
	expect_that(code, is_a("data.frame"))
	expect_that(names(code)[1:4], equals(names_vvcw))
	expect_that(nrow(code), equals(6))
	
})

test_that("plot functions work with code_array", {
			
	expect_that(plot_bar_stacked(code), is_a("ggplot"))
	expect_that(plot_bar(code), is_a("ggplot"))
	expect_that(plot_point(code), is_a("ggplot"))
	
})


###############################################################################

context("Code as single question and plot")

code <- code_single(s, "Q1")

test_that("code_single works", {
			
	expect_that(code, is_a("data.frame"))
	expect_that(names(code)[1:4], equals(names_vvcw))
	expect_that(nrow(code), equals(2))
			
})

test_that("plot functions work with code_single", {
			
	expect_that(plot_bar_stacked(code), is_a("ggplot"))
	expect_that(plot_bar(code), is_a("ggplot"))
	expect_that(plot_point(code), is_a("ggplot"))
			
})

###############################################################################

context("Test output to Latex")

file.remove(list.files(graph_path))
if (file.exists(file.path(latex_path, sinkfile))){
	file.remove(file.path(latex_path, sinkfile))
}


test_that("plot_q works", {
			
			plot_q(t, "Q1", t_count(), code_single, plot_bar)
			plot_q(t, "Q4", t_count(), code_array, plot_bar_stacked)
			expect_that(file.exists(sinkfile), equals(TRUE))
			expect_that(file.exists(file.path(graph_path, "fig1.eps")), equals(TRUE))
			expect_that(file.exists(file.path(graph_path, "fig2.eps")), equals(TRUE))
			
		})

###############################################################################

rm(q_string, q_common, q_unique, long_string, long_string_2, qdata, qtext, s,
		names_vvcw, code)

#file.remove(list.files(graph_path))
#if (file.exists(file.path(latex_path, sinkfile))){
#	file.remove(file.path(latex_path, sinkfile))
#}

