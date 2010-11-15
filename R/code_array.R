code_array <- function(Qnr="Q1",
		data=kd,
		Qtext=Qs,
		multicode=FALSE,
		remove.other=FALSE,
		index="crossbreak",
		weight=TRUE,
		...){
	# Melt multicoded question in data.frame, and code question text to variable
	
	# data is a data frame
	# Qnumber is the number of the question, e.g. "Q4" (text)
	# Qtext is a list of question text (character vector)
	# n is the number of characters in Qtext to discard (numeric)
	# index is the crosstab variable
	
#  data <- kd
#  Qnr <- "Q22"
#  Qtext <- Qs
#  multicode <- TRUE
#  index <- "crossbreak"
#  remove.other <- FALSE
#  weight=TRUE
	
	
	r <- getQunique(data, Qnr, Qtext)
	names(r) <- idQuestionGroup(data, Qnr, Qtext)
	
	if (remove.other==TRUE) r <- r[-length(r)]
	
	x <- as.list(data[names(r)])
	x[x=="NA"] <- NA
	if (multicode==TRUE) x <- llply(x, as.numeric)
	if (!is.null(weight)) x$weight <- data$weight
	
	# Scale to 100%
	x$weight <- x$weight / sum(x$weight)
	
	x <- as.data.frame(x, stringsAsFactors=TRUE)
	x$crossbreak <- kd[,index]
	if (weight==FALSE){
		x <- melt(x, id.vars=c("crossbreak",), na.rm=TRUE)
	} else {
		x <- melt(x, id.vars=c("crossbreak", "weight"), na.rm=TRUE)
	}
	if (multicode==TRUE){
		x <- subset(x, value!=max(value))
	}
	x$variable <- r[as.character(x$variable)]
	x$variable <- wordwrap(x$variable, 50)
	x
}

