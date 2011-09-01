# TODO: Add comment
# 
# Author: Andrie
###############################################################################


path <- file.path("f:","git","surveyor","test")
latex_path <- file.path(path, "latex")
graph_path <- file.path(latex_path, "graphics")
sinkfile   <- file.path(latex_path, "surveyor_test.tex")


#file.remove(list.files(graph_path, full.names=TRUE))
#if (file.exists(file.path(latex_path, sinkfile))){
#  file.remove(file.path(latex_path, sinkfile))
#}

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


context("Test output to Latex")

test_that("surveyor_plot works in Latex", {
      sbraid <- as.braid(
          path_latex    = latex_path,
          path_graphics = graph_path
      )
      
      tbraid <- as.braid(
          path_latex    = latex_path,
          path_graphics = graph_path
      )
      
      s <- as.surveyor(q_data, q_text, q_data$crossbreak, q_data$weight, braid=sbraid)
      #s_count <- new_counter()
      
      t_defaults <- surveyor_defaults(
          output_to_latex = TRUE
      )
      
      t <- as.surveyor(q_data, q_text, q_data$crossbreak, q_data$weight, t_defaults, braid=tbraid)
      #t_count <- new_counter()
      
      s2 <- as.surveyor(q_data, q_text, list(q_data$crossbreak, q_data$crossbreak2), q_data$weight, braid=sbraid)
      #s2_count <- new_counter()
      
      
      if(!identical(list.files(graph_path), "")){
        lapply(list.files(graph_path), function(x)file.remove(file.path(graph_path, x)))
      }          
      if (file.exists(file.path(latex_path, sinkfile))){
        file.remove(file.path(latex_path, sinkfile))
      }
      expect_that(file.exists(file.path(graph_path, "fig0001.pdf")), equals(FALSE))
      expect_that(file.exists(file.path(graph_path, "fig0002.pdf")), equals(FALSE))
      
      surveyor_plot(t, "Q1", code_single, stats_bin, plot_bar)
      surveyor_plot(t, "Q4", code_array, stats_bin, plot_bar)
      #expect_that(file.exists(sinkfile), equals(TRUE))
      expect_that(file.exists(file.path(graph_path, "fig0001.pdf")), equals(TRUE))
      expect_that(file.exists(file.path(graph_path, "fig0002.pdf")), equals(TRUE))
      
    })

