#' Creates object of class "surveyorCode".
#'
#' Creates object of class "surveyorCode".
#' @param x Data created by surveyor code function
#' @param surveyor A surveyor object
#' @param qid Question identifier, e.g. "Q4"
#' @param ... Passed to surveyorStats
#' @export
#' @return An object of class "surveyorCode"
as.surveyorCode <- function(x, surveyor, qid, ...){
  stopifnot(is.surveyor(surveyor))
  surveyor$plot_title <- qText(surveyor$sdata, qid) 
  surveyor$sdata <- NULL
  ret <- list(
      data = x,
      surveyor = surveyor,
      qid = qid
  )
  class(ret) <- "surveyorCode"
  ret
}

#' Tests whether object is of class "surveyorCode".
#'
#' Tests whether object is of class "surveyorCode".
#' @param x Object to test
#' @export
#' @return TRUE or FALSE
is.surveyorCode <- function(x){
  inherits(x, "surveyorCode")
}

#' Code survey data in single question form
#'
#' Code survey data in single question form (i.e. without subquestions)
#' 
#' @param surveyor Surveyor object
#' @param q_id Question id
#' @param wrapWidth Position where labels will be wrapped (in character count)
#' @param ... Passed to surveyorStats
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @family codeFunctions
#' @return data frame
#' @keywords code
#' @export
codeSingle <- function(surveyor, q_id,	wrapWidth=50, ...){
	if(is.numeric(surveyor$sdata[, q_id])){
		response <- surveyor$sdata[, q_id]
	} else {
		response <- str_wrap(as.character(surveyor$sdata[, q_id]), wrapWidth)
	}	
	response[response=="NA"] <- NA
	
	if (all(is.na(response))){
		return(NULL)
	}		
		
	x1 <- data.frame(
			cbreak = surveyor$cbreak,
			question = rep("1", nrow(surveyor$sdata)),
			response = response,
			weight = surveyor$weight,
			stringsAsFactors = FALSE
	)

  as.surveyorCode(
      x1[!is.na(x1$response), ],
      surveyor,
      q_id,
      ...)
  
}

#-------------------------------------------------------------------------------

#' Code survey data in array question form
#'
#' Code survey data in array question form (i.e. with subquestions)
#' 
#' @param surveyor Surveyor object
#' @param q_id Question id
#' @param remove_other If true, will remove last column
#' @param wrapWidth Position where labels will be wrapped (in character count)
#' @param ... Passed to surveyorStats
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @family codeFunctions
#' @return data frame
#' @keywords code
#' @export
codeArray <- function(surveyor, q_id, remove_other=FALSE, wrapWidth=50, ...){
	# Melt multicoded question in data.frame, and code question text to variable
	
	q_data <- surveyor$sdata
	q_text <- qTextUnique(q_data, q_id)
	
	r <- qTextUnique(q_data, q_id)
	names(r) <- names(varlabels(q_data[, q_id]))
  
	if (remove_other==TRUE) r <- tail(r, -1)
	
	x <- as.list(q_data[, q_id])
	x[x=="NA"] <- NA
	
  if (all_na(x)){
		return(NULL)
	}
	
	x$weight <- surveyor$weight
	
	x <- quickdf(x)
	x$cbreak <- surveyor$cbreak
	x <- melt(x, id.vars=c("cbreak", "weight"), na.rm=TRUE)
	
	x$variable <- r[as.character(x$variable)]
	x$variable <- str_wrap(x$variable, wrapWidth)
	x1 <- quickdf(list(
			cbreak=x$cbreak,
			question=x$variable,
			response=x$value,
			weight=x$weight
#			stringsAsFactors=FALSE
	))

  as.surveyorCode(
      x1[!is.na(x1$response), ],
      surveyor,
      q_id,
      ...)
  
}

#-{79}

#' Code survey data in single or array question form.
#'
#' Code survey data in array question form (i.e. with subquestions)
#' 
#' @param surveyor Surveyor object
#' @param q_id Question id
#' @param ... Other parameters passed on to downstream code_* functions
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @family codeFunctions
#' @return data frame
#' @keywords code
#' @export
codeGuess <- function(surveyor, q_id, ...){
	if (length(which.q(surveyor$sdata, q_id))==1){
		dat <- codeSingle(surveyor, q_id, ...)
	} else {
		dat <- codeArray(surveyor, q_id, ...)
	}
  dat
}

#-------------------------------------------------------------------------------

#' Stacks a data frame into a single vector.
#'
#' Adapted from \code{\link{stack}}
#' @param x A data.frame
#' @return A vector
#' @keywords Internal
quickStack <- function(x) unlist(unname(x))

#' Code survey data in single or array question form.
#'
#' Code survey data in array question form (i.e. with subquestions)
#' 
#' @param surveyor Surveyor object
#' @param q_id Question id
#' @param wrapWidth Position where labels will be wrapped.  Passed to \code{\link[stringr]{str_wrap}}
#' @param ... Other parameters passed on to downstream code* functions
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @family codeFunctions
#' @return data frame
#' @keywords code
#' @export
codeQuickArray <- function(surveyor, q_id, wrapWidth=50, ...){
  dat <- surveyor$sdata
  reps <- length(which.q(dat, q_id))
  if(reps==0) return(NULL)
  question <- if(reps==1) "1" else qTextUnique(dat, q_id)
  cbreak <- rep(surveyor$cbreak, reps)
  weight <- rep(surveyor$weight, reps)
  dat <- dat[, q_id]
  nrows <- if(reps==1) length(dat) else nrow(dat)
  #browser()
  if (reps==1) {
    class(dat) <- class(dat)[-1]
    response <- dat
    if(!is.numeric(response)) response <- str_wrap(as.character(response), width=wrapWidth)
  } else {
    response <- quickStack(dat)
  }
  
  x1 <- data.frame(
      cbreak,
      question = rep(question, each=nrows), 
      response = response,
      weight
  )
  
  as.surveyorCode(
      x1[!is.na(x1$response), ],
      surveyor,
      q_id,
      ...)
  
}


#-------------------------------------------------------------------------------

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
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @family codeFunctions
#' @return data frame
#' @keywords code
#' @export
codeText <- function(
		surveyor,
		q_id,
		inn = NULL,
		out = NULL,
		filter_responses=TRUE
){
	q_data <- surveyor$sdata
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




