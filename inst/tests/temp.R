q_data <- data.frame(
		Q1   = c("Yes", "No", "Yes", "Yes"),
		Q2_1 = c(1, 2, 1, 2), 
		Q2_2 = c(3, 4, 4, 3), 
		Q2_3 = c(5, 5, 6, 6), 
		Q4_1 = c(1, 2, 1, 2), 
		Q4_3 = c(3, 4, 4, 3), 
		Q4_2 = c(5, 5, 6, 6), 
		crossbreak = c("A", "A", "B", "B"), 
		weight     = c(0.9, 1.1, 0.8, 1.2)
)
q_text <- c(
		"Question 1", 
		"Question 2", "Question 2", "Question 2",
		"Question 4: 1", "Question 4: 3", "Question 4: 2",
		"crossbreak",
		"weight")

names(q_text) <- names(q_data)

qTextUnique(q_data, "Q4", q_text)