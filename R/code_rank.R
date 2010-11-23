#' Code survey data in rank question form (i.e. with subquestions)
#'
#' @param surveyor Surveyor object
#' @param Qid Question id
#' @param remove.other Boolean
#' @seealso code_single, code_array
#' @export

code_rank <- function(
		surveyor,
		Qid,
		remove.other=FALSE
	){
	
	Qdata <- surveyor$qdata
	Qtext <- surveyor$qtext
	
	r <- get_qtext_unique(Qdata, Qid, Qtext)
	if (remove.other==TRUE) r <- r[-length(r)]
	names(r) <- paste(Qid, "_", 1:length(r), sep="")
#  x <- as.list(data[names(r)])
	x <- Qdata[, names(r)]
	x[x=="NA"] <- NA
#  if (multicode) x <- llply(x, as.numeric)
	if (!is.null(weight)) 	x$weight <- surveyor$weight
		
	if (is.null(index) || index==FALSE){
		if (is.null(weight)){
			x <- melt(as.data.frame(x), id.vars=NULL, na.rm=TRUE)
		} else {
			x <- melt(as.data.frame(x), id.vars="weight", na.rm=TRUE)
		}
	} else{
		x <- data.frame(crossbreak=Qdata[,index], as.data.frame(x))
		if (is.null(weight)){
			x <- melt(x, id.vars=c("crossbreak",), na.rm=TRUE)
		} else {
			x <- melt(x, id.vars=c("crossbreak", "weight"), na.rm=TRUE)
		}
	}
	##  if (multicode)  x <- subset(x, value!=0)
	
	#### Now variable contains rank, and value contains subquestion
	x$variable <- r[x$variable]
	x$variable <- wordwrap(x$variable, 50)
	xtemp <- x
	x$variable <- xtemp$value
	x$value <- xtemp$variable
	rm(xtemp)

	data.frame(
			variable=x$variable,
			value=x$value,
			crossbreak=x$crossbreak,
			weight=x$weight,
			stringsAsFactors=FALSE
	)
	
}

