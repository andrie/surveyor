# Test functionality using testthat library
# 
# Author: Andrie
#------------------------------------------------------------------------------


path <- file.path("f:","git","surveyor","test")
latex_path <- file.path(path, "latex")
graph_path <- file.path(latex_path, "graphics")


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

sbraid <- as.braid(
    path_latex    = latex_path,
    path_graphics = graph_path
)
s <- as.surveyor(q_data, q_text, q_data$crossbreak, q_data$weight, braid=sbraid)






#------------------------------------------------------------------------------

context("Question handling")

test_that("Question handling functions work", {
			
			expect_that(get_q_subquestions(q_data, "Q1"), is_a("NULL"))
			expect_that(get_q_subquestions(q_data, "Q4"), equals(c("Q4_1","Q4_3","Q4_2")))
			
      r <- get_q_text_unique(q_data, "Q4", q_text)
      
      #print(r)
			expect_that(r, equals(c("red", "blue", "green")))
      r <- get_q_text_common(q_data, "Q4", q_text)
      #print(r)
			expect_that(r, equals("Question 4"))
			
			expect_that(get_q_text(s, "Q1"), equals("Question 1"))
      r <- get_q_text(s, "Q4")
      #print(r)
			expect_that(r, equals("Question 4"))
			
		})


#------------------------------------------------------------------------------

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
			#expect_that(surveyor_defaults(path_latex=file.path("random")), throws_error())
			#expect_that(surveyor_defaults(path_graph=file.path("random")), throws_error())
      
			
		})

#------------------------------------------------------------------------------

context("Code as single question and plot")

test_that("code_single works", {
      scode <- code_single(s, "Q1")
      expect_that(scode, is_a("surveyor_code"))
      expect_that(scode$data, is_a("data.frame"))
#      expect_that(code, is_a("data.frame"))
			expect_that(names(scode$data), equals(names_cqrw))
			expect_that(nrow(scode$data), equals(4))
      
      plotcode <- stats_bin(scode)
      expect_that(plotcode, is_a("surveyor_stats"))
      pb <- plot_bar(plotcode)
      ps <- plot_point(plotcode)
      expect_that(pb, is_a("surveyor_plot"))
      expect_that(pb$plot, is_a("ggplot"))
      expect_that(ps, is_a("surveyor_plot"))
      expect_that(ps$plot, is_a("ggplot"))
      
		})

#------------------------------------------------------------------------------

context("Code as array question and plot")


test_that("code_array works", {
			
      scode <- code_array(s, "Q4")
      plotcode <- stats_bin(scode)
      expect_that(scode, is_a("surveyor_code"))
      expect_that(scode$data, is_a("data.frame"))
      expect_that(names(scode$data), equals(names_cqrw))
			expect_that(nrow(scode$data), equals(12))
      
		})

test_that("plot functions work with code_array", {
			
      scode <- code_array(s, "Q4")
      plotcode <- stats_bin(scode)
      pb <- plot_bar(plotcode)
      ps <- plot_point(plotcode)
      expect_that(pb, is_a("surveyor_plot"))
      expect_that(pb$plot, is_a("ggplot"))
      expect_that(ps, is_a("surveyor_plot"))
      expect_that(ps$plot, is_a("ggplot"))
      
		})

#------------------------------------------------------------------------------

context("Test plumbing of surveyor_plot")

test_that("surveyor_plot works", {
			
			expect_that(surveyor_plot(s, "Q1", code_single, stats_bin, plot_bar), shows_message("Q1"))
			expect_that(surveyor_plot(s, "Q4", code_array, stats_bin, plot_point), is_a("NULL"))
			
		})

#------------------------------------------------------------------------------

context("Test that multiple crossbreaks work")


test_that("surveyor_plot works with multiple crossbreaks", {
      s2 <- as.surveyor(q_data, q_text, list(q_data$crossbreak, q_data$crossbreak2), q_data$weight, braid=sbraid)
      
			expect_that(surveyor_plot(s2, "Q1", code_single, stats_bin, plot_bar), shows_message("Q1"))
      expect_that(surveyor_plot(s2, "Q1", code_single, stats_bin, plot_bar), is_a("NULL"))
      
		})

