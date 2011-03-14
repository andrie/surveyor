# Test functionality using testthat library
# 
# Author: Andrie
###############################################################################



long_string <- "the quick brown fox jumps over the lazy dog"
long_string_2 <- c("the quick brown fox jumps over the lazy dog", 
		"Why is the sentence the quick brown fox jumps over the lazy dog so unique? ")


###############################################################################

context("String functions: wrap and reverse")

test_that("string wrap and reverse works", {
			
			expect_that(str_wrap(long_string, 10), 
					equals("the quick\nbrown fox\njumps\nover the\nlazy dog"))
			
			expect_that(str_wrap(long_string_2, 20), 
					equals(c("the quick brown fox\njumps over the lazy\ndog",
									"Why is the sentence\nthe quick brown fox\njumps over the lazy\ndog so unique?")))
			
			expect_that(str_reverse(long_string), 
					equals("god yzal eht revo spmuj xof nworb kciuq eht"))
		})


context("String functions: common and unique")

qs0 <- "Q"
qs01 <- c("Q", "Q1")
qs1 <- c("Q1", "Q1")
qs2 <- c("Q1", "Q2")
qs3 <- c("1", "2", "3")
qs4 <- c("Q_1", "Q_2", "Q_3") 

test_that("str_common_unique works", {
			
			str_common <- function(x) str_common_unique(x)$common
			str_unique <- function(x) str_common_unique(x)$unique
			
			expect_that(str_common(qs1), is_a("character"))
			expect_that(str_unique(qs1), is_a("character"))

			expect_that(str_common(qs0), equals("Q"))
			expect_that(str_unique(qs0), equals(""))
			
			expect_that(str_common(qs01), equals("Q"))
			expect_that(str_unique(qs01), equals(c("", "1")))
			
			expect_that(str_common(qs1), equals("Q1"))
			expect_that(str_unique(qs1), equals(c("", "")))
			
			expect_that(str_common(qs2), equals("Q"))
			expect_that(str_unique(qs2), equals(c("1", "2")))
			
			expect_that(str_common(qs3), equals(""))
			expect_that(str_unique(qs3), equals(c("1", "2", "3")))
			
			expect_that(str_common(qs4), equals("Q_"))
			expect_that(str_unique(qs4), equals(c("1", "2", "3")))

		})


###############################################################################
###############################################################################
###############################################################################

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


###############################################################################

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


###############################################################################


context("Validation of all_NA and all_NULL functions")

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



# This file contains test scripts, using the testthat package 
# 
# Author: Andrie
###############################################################################

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

# ---

path <- file.path("f:","git","surveyor","test")
latex_path <- file.path(path, "latex")
graph_path <- file.path(latex_path, "graphics")
sinkfile   <- file.path(latex_path, "surveyor_test.tex")

###############################################################################

file.remove(list.files(graph_path, full.names=TRUE))
if (file.exists(file.path(latex_path, sinkfile))){
	file.remove(file.path(latex_path, sinkfile))
}

###############################################################################

s_defaults <- surveyor_defaults(
		path_latex    = latex_path,
		path_graphics = graph_path,
		output_to_latex = FALSE
)

s <- as.surveyor(q_data, q_text, q_data$crossbreak, q_data$weight, s_defaults)
#s_count <- new_counter()

t_defaults <- surveyor_defaults(
		path_latex    = latex_path,
		path_graphics = graph_path,
		output_to_latex = TRUE,
		output_filename = sinkfile
)

t <- as.surveyor(q_data, q_text, q_data$crossbreak, q_data$weight, t_defaults)
#t_count <- new_counter()

s2 <- as.surveyor(q_data, q_text, list(q_data$crossbreak, q_data$crossbreak2), q_data$weight, s_defaults)
#s2_count <- new_counter()



###############################################################################

context("Question handling")

test_that("Question handling functions work", {
			
			expect_that(get_q_subquestions(q_data, "Q1"), is_a("NULL"))
			expect_that(get_q_subquestions(q_data, "Q4"), equals(c("Q4_1","Q4_3","Q4_2")))
			
			expect_that(get_q_text_unique(q_data, "Q4", q_text), equals(c("red", "blue", "green")))
			expect_that(get_q_text_common(q_data, "Q4", q_text), equals("Question 4: "))
			
			expect_that(get_q_text(s, "Q1"), equals("Question 1"))
			expect_that(get_q_text(s, "Q4"), equals("Question 4: "))
			
		})


###############################################################################

context("Surveyor class objects")

test_that("Surveyor objects are defined properly", {
			
			expect_that(s, is_a("surveyor"))
			expect_that(s$q_data, is_a("data.frame"))
			expect_that(s$q_data, equals(q_data))
			expect_that(s$q_text, equals(q_text))
			expect_that(s$crossbreak, equals(q_data$crossbreak))
			expect_that(s$weight, equals(q_data$weight))
			expect_that(is.surveyor(s), equals(TRUE))
			expect_that(is.surveyor(q_data), equals(FALSE))
			expect_that(as.surveyor(q_data, q_text[-1]), throws_error())
			expect_that(surveyor_defaults(path_latex=file.path("random")), throws_error())
			expect_that(surveyor_defaults(path_graph=file.path("random")), throws_error())
			
		})

###############################################################################

context("Code as single question and plot")

code <- code_single(s, "Q1")
plotcode <- stats_bin(code)

test_that("code_single works", {
			
			expect_that(code, is_a("data.frame"))
			expect_that(names(code), equals(names_cqrw))
			expect_that(nrow(code), equals(4))
			
		})

test_that("plot functions work with code_single", {
			
			expect_that(plot_bar(plotcode, s), is_a("ggplot"))
			expect_that(plot_point(plotcode, s), is_a("ggplot"))
			
		})

###############################################################################

context("Code as array question and plot")

code <- code_array(s, "Q4")
plotcode <- stats_bin(code)

test_that("code_array works", {
			
			expect_that(code, is_a("data.frame"))
			expect_that(names(code), equals(names_cqrw))
			expect_that(nrow(code), equals(12))
			
		})

test_that("plot functions work with code_array", {
			
			expect_that(plot_bar(plotcode), is_a("ggplot"))
			expect_that(plot_point(plotcode), is_a("ggplot"))
			
		})

###############################################################################

context("Test plumbing of surveyor_plot")

test_that("surveyor_plot works", {
			
			expect_that(surveyor_plot(s, "Q1", code_single, stats_bin, plot_bar), shows_message("Q1"))
			expect_that(surveyor_plot(s, "Q1", code_single, stats_bin, plot_bar), is_a("NULL"))
			
			expect_that(surveyor_plot(s, "Q4", code_array, stats_bin, plot_point), is_a("NULL"))
			
		})

###############################################################################

context("Test that multiple crossbreaks work")


test_that("surveyor_plot works with multiple crossbreaks", {
			
			expect_that(surveyor_plot(s2, "Q1", code_single, stats_bin, plot_bar), shows_message("Q1"))
			
		})

###############################################################################



context("Test output to Latex")

file.remove(list.files(graph_path))
if (file.exists(file.path(latex_path, sinkfile))){
	file.remove(file.path(latex_path, sinkfile))
}


test_that("surveyor_plot works in Latex", {
			
			surveyor_plot(t, "Q1", code_single, stats_bin, plot_bar)
			surveyor_plot(t, "Q4", code_array, stats_bin, plot_bar)
			expect_that(file.exists(sinkfile), equals(TRUE))
			expect_that(file.exists(file.path(graph_path, "fig1.eps")), equals(TRUE))
			expect_that(file.exists(file.path(graph_path, "fig2.eps")), equals(TRUE))
			
		})

###############################################################################

rm(list=ls())

#file.remove(list.files(graph_path))
#if (file.exists(file.path(latex_path, sinkfile))){
#	file.remove(file.path(latex_path, sinkfile))
#}

