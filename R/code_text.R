
#' Removes a selected range of 'content-free' strings from x
#' 
#' Removes all strings that removes all strings that match the regular 
#' expression "remove" from x
#' @param x A regular expression in character format
#' @param remove A character string passed to grep() 
filter_nocomment <- function(x, remove="^(No|no|NO|Nope|None|none|n.a.|NA|n/a).?$"){
	# This function strips out some content-free answers
	z <- x[!is.na(x)]
	z[grep(remove, z, invert=TRUE)]
}

#' Code survey data in text question form (i.e. open-ended text
#'
#' @param surveyor Surveyor object
#' @param q_id Question id
#' @param inn string passed to latexTranslate()
#' @param out string passed to latexTranslate()
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
#' @return data frame
#' @export
code_text <- function(
		surveyor,
		q_id,
		inn = NULL,
		out = NULL
	){
	q_data <- surveyor$q_data
	q_text <- surveyor$q_text
	inn <- c(inn, "\\",   "&",   "@")
	out <- c(out, "\\\\", "\\&", "\\@")
		
	tmp <- q_data[, q_id]
	tmp <- subset(tmp, !is.na(tmp))
	if (filter_responses==TRUE){
		tmp <- filter_nocomment(tmp)
	}
	tmp <- laply(tmp, function(x)
			{paste("\\item", latexTranslate(x,
								inn=,
								out=))})
	paste(tmp, collapse="\n\n")
}



