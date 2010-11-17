#################################################################################
####  net score calculation and graphing                                      ###
#################################################################################
#
#netScore <- function(x){
#	
##  x <- kd$Q22_1
#	x <- x[!is.na(x)]
#	
#	xu <- unclass(x)
#	m <- ceiling(length(levels(x))/2)
#	below <- length(xu[xu < m])
#	above <- length(xu[xu > m])
#	net <- above - below
#	netScore <- net / length(x)
#	netScore
#}
#
#code_netScore <- function(Qid="Q1",
#		data=kd,
#		Qtext=Qs,
#		index="crossbreak",
#		weight=TRUE,
#		...){
##  data <- kd
##  Qid <- "Q22"
##  Qtext <- Qs
##  multicode <- TRUE
##  index <- "crossbreak"
##  remove.other <- FALSE
##  weight=TRUE
#	
#	
#	r <- get_qtext_unique(data, Qid, Qtext)
#	names(r) <- idQuestionGroup(data, Qid, Qtext)
#	
#	
#	
#	x <- as.data.frame(data[names(r)])
#	x$crossbreak <- data[, index]
#	x[x=="NA"] <- NA
#	
#	
#	segment_netScore <- function(x){
#		# make data.frame with subset net scores
#		# first drop crossbreak column
#		x <- x[, which(names(x)!="crossbreak")]
#		x <- ldply(x, netScore)
#		
#		x$variable <- r[as.character(x$.id)]
#		x <- rename(x, c(V1="value"))
#		
#		x$variable <- wordwrap(x$variable, 50)
#		x$weight <- rep(1, nrow(x))
#		x
#	}
#	
#	segment_netScore(x)
#	
#	ddply(x, "crossbreak", segment_netScore)
#}
