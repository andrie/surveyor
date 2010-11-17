# TODO: Add comment
# 
# Author: Andrie
###############################################################################


qdata <- data.frame(
		Q1=c(11, 12), 
		Q4_1 = c(1,2), 
		Q4_3=c(3,4), 
		Q4_2=c(5,6), 
		crossbreak=c("A", "B"), 
		weight=c(0.9, 1.1))

qtext <- c(
		"Question 1", 
		"Question 4: red", 
		"Question 4: blue", 
		"Question 4: yellow", 
		"crossbreak", 
		"weight")

names(qtext) <- c("Q1", "Q4_1", "Q4_3", "Q4_2", "crossbreak", "weight")

s_defaults <- surveyor_defaults()
s <- surveyor(qdata, qtext, s_defaults)

#surveyor_initialise()
save_print <- save_print_function()

plot_q(s, "Q4", code_array, plot_bar_stacked)
