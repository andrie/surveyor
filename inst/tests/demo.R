# Demo
# TODO: Surveyor: Ensure demonstration file runs without errors

q_string <- c("Q_1", "Q_2", "Q_3") 
q_common <- surveyor:::str_common_unique(q_string)$common
q_unique <- surveyor:::str_common_unique(q_string)$unique

long_string <- "the quick brown fox jumps over the lazy dog"
long_string_2 <- c("the quick brown fox jumps over the lazy dog", 
		"Why is the sentence the quick brown fox jumps over the lazy dog so unique? ")

q_data <- data.frame(
		Q1=c("Yes", "No", "Yes", "Yes"),
		Q4_1 = c(1, 2, 1, 2), 
		Q4_3=c(5, 4, 4, 3), 
		Q4_2=c(5, 5, 6, 6),
		Q5=c(1, 2, 2, 3),
		crossbreak=c("A", "A", "B", "B"), 
		weight=c(0.9, 1.1, 0.8, 1.2)
)
q_text <- c("Question 1", 
		"Question 4: red", "Question 4: blue", "Question 4: green", 
		"Question 5",
		"crossbreak", "weight")
names(q_text) <- c("Q1", "Q4_1", "Q4_3", "Q4_2", "Q5", "crossbreak", "weight")

names_vcw <- c("variable", "value", "crossbreak")

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


s_defaults <- surveyor_defaults(
		path_latex    = latex_path,
		path_graphics = graph_path,
		output_to_latex = FALSE
)

s <- surveyor(q_data, q_text, q_data$crossbreak, q_data$weight, s_defaults)
s_count <- new_counter()

print(s)


###############################################################################


r <- code_single(s, "Q1")
print(r)

f <- code_single(s, "Q5")
print(f)

ddply(f, .(crossbreak, variable), value=net_score(value))



