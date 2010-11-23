
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
#' @param Qid Question id
#' @param inn string passed to latexTranslate()
#' @param out string passed to latexTranslate()
#' @seealso code_single, code_rank
#' @export
code_text <- function(
		surveyor,
		Qid,
		inn = NULL,
		out = NULL
	){
	Qdata <- surveyor$qdata
	Qtext <- surveyor$qtext
	inn <- c(inn, "\\",   "&",   "@")
	out <- c(out, "\\\\", "\\&", "\\@")
		
	tmp <- Qdata[, Qid]
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



