#' Code survey data in rank question form
#'
#' Code survey data in rank question form (i.e. with subquestions)
#' 
#' @param surveyor Surveyor object
#' @param q_id Question id
#' @param remove.other Boolean
#' @return data frame
#' @seealso \code{\link{code_single}}, \code{\link{code_array}}
#' @export
code_rank <- function(
		surveyor,
		q_id,
		remove.other=FALSE
	){
	
	q_data <- surveyor$q_data
	q_text <- surveyor$q_text
	
	r <- get_q_text_unique(q_data, q_id, q_text)
	if (remove.other==TRUE) r <- r[-length(r)]
	names(r) <- paste(q_id, "_", 1:length(r), sep="")
#  x <- as.list(data[names(r)])
	x <- q_data[, names(r)]
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
		x <- data.frame(crossbreak=q_data[,index], as.data.frame(x))
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

