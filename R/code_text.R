#filter_nocomment <- function(x){
#	# This function strips out some content-free answers
#	z <- x[!is.na(x)]
#	z[grep("^(No|no|NO|Nope|None|none|n.a.|NA|n/a).?$", z, invert=TRUE)]
#}
#
#
#code_text <- function(Qnr="Q1",
#		data=kd,
#		filter_responses=FALSE,
#		...){
#	# Deals with open-ended text questions
#	
#	tmp <- data[, Qnr]
#	tmp <- subset(tmp, !is.na(tmp))
#	if (filter_responses==TRUE){
#		tmp <- filter_nocomment(tmp)
#	}
##   tmp <- laply(tmp, function(x){paste("\\cjktext{", x, "}\\\\ \n\n", sep="")})
##   tmp <- laply(tmp, function(x){paste(x, "\n\n", sep="")})
#	tmp <- laply(tmp, function(x)
#			{paste("\\item", latexTranslate(x,
#								inn=c("\\",   "&",   "@"),
#								out=c("\\\\", "\\&", "\\@")))})
#	paste(tmp, collapse="\n\n")
#}

