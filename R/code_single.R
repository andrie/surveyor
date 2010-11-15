code_single <- function(Qnr,
		data=kd,
		Qtext=Qs,
		weight=TRUE,
		...){
	x <- data[, Qnr]
	x <- factor(x)
	levels(x) <- wordwrap(levels(x), 30)
	cb(x)
}

