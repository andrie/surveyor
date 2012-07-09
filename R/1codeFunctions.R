#' Creates object of class "surveyorCode".
#'
#' Creates object of class "surveyorCode".
#' @param x Data created by surveyor code function
#' @param surveyor A surveyor object
#' @param qid Question identifier, e.g. "Q4"
#' @param ... Passed to surveyorStats
#' @export
#' @return 
#' An object of class "surveyorCode"
#' \describe{
#' \item{data}{A data.frame with four columns: cbreak, question, response and weight}
#' \item{surveyorDefaults}{A copy of the original surveyor defaults, see \code{\link{surveyorDefaults}}}
#' \item{plotTitle}{Main title of the plot: defaults to question text}
#' \item{qid}{Question id, e.g. "Q4"}
#' \item{sampleSize}{Named vector with weighted sample size for each crossbreak}
#' }
as.surveyorCode <- function(x, surveyor, qid, ...){
  stopifnot(is.surveyor(surveyor))
  plotTitle <- qTextCommon(surveyor$sdata, qid) 
  surveyor$sdata <- NULL
#  sampleSize <- vapply(
#      split(x, x$cbreak), 
#      function(x)weightedCount(x$response, x$weight),
#      FUN.VALUE=1
#  )

  ret <- list(
      data = x,
      surveyorDefaults = surveyor$defaults,
      plotTitle = plotTitle,
      qid = qid
      #sampleSize = sampleSize
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
	
  if (allNA(x)){
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

#------------------------------------------------------------------------------

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
#	if (length(which.q(surveyor$sdata, q_id))==1){
#		dat <- codeSingle(surveyor, q_id, ...)
#	} else {
#		dat <- codeArray(surveyor, q_id, ...)
#	}
  codeQuickArray(surveyor, q_id, ...)
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
#' @param crossbreak Crossbreak vector
#' @param wrapWidth Position where labels will be wrapped.  Passed to \code{\link[stringr]{str_wrap}}
#' @param autosortQuestion Logical. If TRUE, sorts questions in order of response
#' @param ... Other parameters passed on to downstream code* functions
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @family codeFunctions
#' @return data frame
#' @keywords code
#' @export
codeQuickArray <- function(surveyor, q_id, crossbreak=surveyor$cbreak, wrapWidth=50,
    autosortQuestion=FALSE, ...){
  dat <- surveyor$sdata[, q_id]
  if(is.data.frame(dat)) reps <- ncol(dat) else reps <- 1
  #reps <- length(dat)
  if(reps==0) return(NULL)
  questionText <- if(reps==1) qTextCommon(surveyor$sdata, q_id) else qTextUnique(surveyor$sdata, q_id)
  question <- if(reps==1) "1" else questionText
  #cbreak <- rep(surveyor$cbreak, reps)
  cbreak <- rep(crossbreak, reps)
  weight <- rep(surveyor$weight, reps)
  #dat <- dat[, q_id]
  
  if(reps==1){
    to.order <- is.ordered(dat)
    order.levels <- levels(dat)
  } else {
    to.order <- is.ordered(dat$response)
    order.levels <- levels(dat$response)
  }
  
  nrows <- if(reps==1) length(dat) else nrow(dat)
  if (reps==1) {
    class(dat) <- class(dat)[-1]
    response <- unlist(dat)
    #browser()
    if(!is.numeric(response)) {
      response <- str_wrap(as.character(response), width=wrapWidth)
      order.levels <- str_wrap(as.character(order.levels), width=wrapWidth)
    }
  } else {
    response <- quickStack(dat)
  }
  
#  browser()
  if(to.order) response <- factor(response, levels=order.levels, ordered=TRUE)
  
  ret <- data.frame(
      cbreak,
      question = if(!autosortQuestion){
        factor(rep(question, each=nrows), levels=question, ordered=TRUE)
      } else {
        rep(questionText, each=nrows)
      },
      response = response,
      weight
#      stringsAsFactors=FALSE
  )
  
  as.surveyorCode(
      ret[!is.na(ret$response), ],
      surveyor,
      q_id,
      ...)
  
}







