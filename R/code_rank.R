#code_rank <- function(Qid="Q1",
#		data=kd,
#		Qtext=Qs,
#		multicode=FALSE,
#		remove.other=FALSE,
#		index=NULL,
#		weight=TRUE,
#		...){
#	# Melt multicoded question in data.frame, and code question text to variable
#	
#	# data is a data frame
#	# Qnumber is the number of the question, e.g. "Q4" (text)
#	# Qtext is a list of question text (character vector)
#	# n is the number of characters in Qtext to discard (numeric)
#	# index is the crosstab variable
#	
##  Qid <- "Q14"
##  data <- kd
##  Qtext <- Qs
#	##  multicode <- FALSE
##  remove.other <- FALSE
##  index <- NULL
##  weight=TRUE
##
#	
#	r <- get_qtext_unique(data, Qid, Qtext)
#	if (remove.other==TRUE) r <- r[-length(r)]
#	names(r) <- paste(Qid, "_", 1:length(r), sep="")
##  x <- as.list(data[names(r)])
#	x <- data[, names(r)]
#	x[x=="NA"] <- NA
##  if (multicode) x <- llply(x, as.numeric)
#	if (!is.null(weight)) x$weight <- data$weight
#	
#	if (is.null(index) || index==FALSE){
#		if (is.null(weight)){
#			x <- melt(as.data.frame(x), id.vars=NULL, na.rm=TRUE)
#		} else {
#			x <- melt(as.data.frame(x), id.vars="weight", na.rm=TRUE)
#		}
#	} else{
#		x <- data.frame(crossbreak=data[,index], as.data.frame(x))
#		if (is.null(weight)){
#			x <- melt(x, id.vars=c("crossbreak",), na.rm=TRUE)
#		} else {
#			x <- melt(x, id.vars=c("crossbreak", "weight"), na.rm=TRUE)
#		}
#	}
#	##  if (multicode)  x <- subset(x, value!=0)
#	
#	#### Now variable contains rank, and value contains subquestion
#	x$variable <- r[x$variable]
#	x$variable <- wordwrap(x$variable, 50)
#	xtemp <- x
#	x$variable <- xtemp$value
#	x$value <- xtemp$variable
#	rm(xtemp)
##  x <- x[x$value==1, ]
#	x
#}

