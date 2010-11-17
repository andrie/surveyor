#' Code survey data in array question form (i.e. with subquestions)
#'
#' @param surveyor Surveyor object
#' @param Qid Question id
#' @param multicode Boolean
#' @param remove.other Boolean
#' @param index Crossbreak variable
#' @param weight
#' @param ...
#' @seealso code_single, code_rank
#' @export
code_array <- function(
		surveyor,
		Qid,
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
#  Qid <- "Q22"
#  Qtext <- Qs
#  multicode <- TRUE
#  index <- "crossbreak"
#  remove.other <- FALSE
#  weight=TRUE
	
	Qdata <- surveyor$qdata
	Qtext <- surveyor$qtext
	
	r <- get_qtext_unique(Qdata, Qid, Qtext)
	names(r) <- get_q_subquestions(Qdata, Qid)
	
	if (remove.other==TRUE) r <- r[-length(r)]
	
	x <- as.list(Qdata[names(r)])
	x[x=="NA"] <- NA
	if (multicode==TRUE) x <- llply(x, as.numeric)
	if (!is.null(weight)) x$weight <- Qdata$weight
	
	# Scale to 100%
	x$weight <- x$weight / sum(x$weight)
	
	x <- as.data.frame(x, stringsAsFactors=TRUE)
	x$crossbreak <- Qdata[,index]
	if (weight==FALSE){
		x <- melt(x, id.vars=c("crossbreak",), na.rm=TRUE)
	} else {
		x <- melt(x, id.vars=c("crossbreak", "weight"), na.rm=TRUE)
	}
	if (multicode==TRUE){
		x <- subset(x, value!=max(value))
	}
	x$variable <- r[as.character(x$variable)]
	x$variable <- str_wrap(x$variable, 50)
	data.frame(
			variable=x$variable,
			value=x$value,
			crossbreak=x$crossbreak,
			weight=x$weight
	)
}

