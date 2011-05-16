#' Code survey data in single question form
#'
#' Code survey data in single question form (i.e. without subquestions)
#' 
#' @param surveyor Surveyor object
#' @param q_id Question id
#' @param ... Not used
#' @seealso
#' Other coding functions: 
#' \code{\link{code_single}},  
#' \code{\link{code_array}}, 
#' \code{\link{code_text}}, 
#' \code{\link{code_guess}}
#' 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' 
#' @return data frame
#' @keywords code
#' @export
code_single <- function(surveyor, q_id,	...){
	if(is.numeric(surveyor$q_data[, q_id])){
		response <- surveyor$q_data[, q_id]
	} else {
		response <- str_wrap(surveyor$q_data[, q_id], 30)
	}	
	response[response=="NA"] <- NA
	
	if (all(is.na(response))){
		return(NULL)
	}		
		
	x1 <- quickdf(list(
			cbreak = surveyor$cbreak,
			question = rep("1", nrow(surveyor$q_data)),
			response = response,
			weight = surveyor$weight
#			stringsAsFactors = FALSE
	))
	x1[!is.na(x1$response), ]
	
}

################################################################################

#' Code survey data in array question form
#'
#' Code survey data in array question form (i.e. with subquestions)
#' 
#' @param surveyor Surveyor object
#' @param q_id Question id
#' @param remove_other If true, will remove last column
#' @seealso
#' Other coding functions: 
#' \code{\link{code_single}},  
#' \code{\link{code_array}}, 
#' \code{\link{code_text}}, 
#' \code{\link{code_guess}}
#' 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' 
#' @return data frame
#' @keywords code
#' @export
code_array <- function(surveyor, q_id, remove_other=FALSE){
	# Melt multicoded question in data.frame, and code question text to variable
	
	q_data <- surveyor$q_data
	q_text <- surveyor$q_text
	
	r <- get_q_text_unique(q_data, q_id, q_text)
	names(r) <- get_q_subquestions(q_data, q_id, surveyor)
	
	if (remove_other==TRUE) r <- r[-length(r)]
	
	x <- as.list(q_data[names(r)])
	x[x=="NA"] <- NA
	
	if (all_na(x)){
		return(NULL)
	}
	
	#x <- llply(x, as.numeric)  ### This is the line that handles multi-code
	
	x$weight <- surveyor$weight
	
	# Scale to 100%
	#x$weight <- x$weight / sum(x$weight)
	
	x <- as.data.frame(x, stringsAsFactors=TRUE)
	x$cbreak <- surveyor$cbreak
	x <- melt(x, id.vars=c("cbreak", "weight"), na.rm=TRUE)
	
	x$variable <- r[as.character(x$variable)]
	x$variable <- str_wrap(x$variable, 50)
	x1 <- quickdf(list(
			cbreak=x$cbreak,
			question=x$variable,
			response=x$value,
			weight=x$weight
#			stringsAsFactors=FALSE
	))
	x1[!is.na(x1$response), ]
}

################################################################################

#' Code survey data in single or array question form
#'
#' Code survey data in array question form (i.e. with subquestions)
#' 
#' @param surveyor Surveyor object
#' @param q_id Question id
#' @param ... Other parameters passed on to downstream code_* functions
#' @seealso
#' Other coding functions: 
#' \code{\link{code_single}},  
#' \code{\link{code_array}}, 
#' \code{\link{code_text}}, 
#' \code{\link{code_guess}}
#' 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' 
#' @return data frame
#' @keywords code
#' @export
code_guess <- function(surveyor, q_id, ...){
	if (any(names(surveyor$q_text)==q_id)){
		return(code_single(surveyor, q_id, ...))
	} else {
		return(code_array(surveyor, q_id, ...))
	}
}

################################################################################

#' Removes a selected range of 'content-free' strings from x
#' 
#' Removes all strings that removes all strings that match the regular 
#' expression "remove" from x
#' @param x A regular expression in character format
#' @param remove A character string passed to grep() 
#' @export
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
#' @param filter_responses Indicates whether responses should be filtered first
#' @seealso
#' Other coding functions: 
#' \code{\link{code_single}},  
#' \code{\link{code_array}}, 
#' \code{\link{code_text}}, 
#' \code{\link{code_guess}}
#' 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' 
#' @return data frame
#' @keywords code
#' @export
code_text <- function(
		surveyor,
		q_id,
		inn = NULL,
		out = NULL,
		filter_responses=TRUE
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
	tmp <- sapply(tmp, function(x)
			{paste("\\item", latexTranslate(x,
								inn=,
								out=))})
	paste(tmp, collapse="\n\n")
}


##TODO: Fix handling of remove_other in code_multi
#
##' Code survey data in multiple question form
##'
##' Code survey data in multiple question form (i.e. with subquestions)
##' 
##' @param surveyor Surveyor object
##' @param q_id Question id
##' @param remove_other If true, will remove last column
##' @return data frame
##' @seealso Coding functions:
##' \code{\link{code_single}}, 
##' \code{\link{code_array}},
##' \code{\link{code_text}}
##' Summarising functions:
##' \code{\link{stats_bin}}, 
##' \code{\link{stats_net_score}}
##' Plot functions: 
##' \code{\link{plot_bar}}, 
##' \code{\link{plot_point}} 
##' @export
#code_multi <- function(
#		surveyor,
#		q_id,
#		remove_other=FALSE
#){
#	# Melt multicoded question in data.frame, and code question text to variable
#	
#	q_data <- surveyor$q_data
#	q_text <- surveyor$q_text
#	
#	r <- get_q_text_unique(q_data, q_id, q_text)
#	names(r) <- get_q_subquestions(q_data, q_id, surveyor)
#	
#	if (remove_other==TRUE) r <- r[-length(r)]
#	
#	x <- as.list(q_data[names(r)])
#	x[x=="NA"] <- NA
#	#x <- llply(x, as.numeric)  ### This is the line that handles multi-code
#	x$weight <- surveyor$weight
#	
#	# Scale to 100%
#	x$weight <- x$weight / sum(x$weight)
#	
#	x <- as.data.frame(x, stringsAsFactors=TRUE)
#	x$cbreak <- surveyor$cbreak
##	if (weight==FALSE){
##		x <- melt(x, id.vars=c("cbreak",), na.rm=TRUE)
##	} else {
#	x <- melt(x, id.vars=c("cbreak", "weight"), na.rm=TRUE)
##	}
#
#	#x <- subset(x, value!=max(value)) ### This handles remove_other
#	
#	x$variable <- r[as.character(x$variable)]
#	x$variable <- str_wrap(x$variable, 50)
#	data.frame(
#			variable=x$variable,
#			value=x$value,
#			cbreak=x$cbreak,
#			weight=x$weight,
#			stringsAsFactors=FALSE
#	)
#}
#


##' Code survey data in array question form
##'
##' Code survey data in array question form (i.e. with subquestions)
##' 
##' @param surveyor Surveyor object
##' @param q_id Question id
##' @param remove_other If true, will remove last column
##' @seealso
##' Other coding functions: 
##' \code{\link{code_single}},  
##' \code{\link{code_array}}, 
##' \code{\link{code_text}}, 
##' \code{\link{code_guess}}
##' 
##' For an overview of the surveyor package \code{\link{surveyor}}
##' 
##' @return data frame
##' @keywords code
##' @export
#code_array_fast <- function(
#		surveyor,
#		q_id,
#		remove_other=FALSE
#){
#	# Melt multicoded question in data.frame, and code question text to variable
#	
#	q_data <- surveyor$q_data
#	q_text <- surveyor$q_text
#	
#	r <- get_q_text_unique(q_data, q_id, q_text)
#	names(r) <- get_q_subquestions(q_data, q_id, surveyor)
#	
#	if (remove_other==TRUE) r <- r[-length(r)]
#	
#	x <- as.list(q_data[names(r)])
#	x[x=="NA"] <- NA
#	
#	if (all_na(x)){
#		return(NULL)
#	}
#	
#	#x <- llply(x, as.numeric)  ### This is the line that handles multi-code
#	
#	x$weight <- surveyor$weight
#	
#	# Scale to 100%
#	#x$weight <- x$weight / sum(x$weight)
#	
#	x <- as.data.frame(x, stringsAsFactors=TRUE)
#	x$cbreak <- surveyor$cbreak
#	x <- melt(x, id.vars=c("cbreak", "weight"), na.rm=TRUE)
#	
#	x$variable <- r[as.character(x$variable)]
#	#x$variable <- str_wrap(x$variable, 50)
#	x1 <- data.frame(
#			cbreak=x$cbreak,
#			question=x$variable,
#			response=x$value,
#			weight=x$weight,
#			stringsAsFactors=FALSE
#	)
#	x1[!is.na(x1$response), ]
#}
#

