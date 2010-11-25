#TODO: Fix code_multi2

#' Code survey data in multiple question form 2
#'
#' Code survey data in multiple question form (i.e. with subquestions)
#' 
#' @param surveyor Surveyor object
#' @param q_id Question id
#' @param remove_other If true, will remove last column
#' @return data frame
#' @seealso Coding functions:
#' \code{\link{code_single}}, 
#' \code{\link{code_array}},
#' \code{\link{code_rank}},
#' \code{\link{code_text}}
#' Summarising functions:
#' \code{\link{stats_bin}}, 
#' \code{\link{stats_net_score}}
#' Plot functions: 
#' \code{\link{plot_bar}}, 
#' \code{\link{plot_point}} 
#' @export
code_multi2 <- function(
		surveyor,
		q_id,
		remove_other=FALSE
){
	# Melt multicoded question in data.frame, and code question text to variable
	
	q_data <- surveyor$q_data
	q_text <- surveyor$q_text
	
	r <- get_q_text_unique(q_data, q_id, q_text)
	names(r) <- get_q_subquestions(q_data, q_id)
	
	if (remove_other==TRUE) r <- r[-length(r)]
	
	x <- as.list(q_data[names(r)])
	x[x=="NA"] <- NA
	#x <- llply(x, as.numeric)  ### This is the line that handles multi-code
	x$weight <- surveyor$weight
	
	# Scale to 100%
	x$weight <- x$weight / sum(x$weight)
	
	x <- as.data.frame(x, stringsAsFactors=TRUE)
	x$crossbreak <- surveyor$crossbreak
#	if (weight==FALSE){
#		x <- melt(x, id.vars=c("crossbreak",), na.rm=TRUE)
#	} else {
	x <- melt(x, id.vars=c("crossbreak", "weight"), na.rm=TRUE)
#	}
	
	#x <- subset(x, value!=max(value)) ### This handles remove_other
	
	x$variable <- r[as.character(x$variable)]
	x$variable <- str_wrap(x$variable, 50)
	data.frame(
			variable=x$variable,
			value=x$value,
			crossbreak=x$crossbreak,
			weight=x$weight,
			stringsAsFactors=FALSE
	)
}


