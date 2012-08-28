#' Creates object of class "surveyorCode".
#'
#' Creates object of class "surveyorCode".
#' @param x Data created by surveyor code function
#' @param surveyor A surveyor object
#' @param qid Question identifier, e.g. "Q4"
#' @param ... Passed to surveyorStats
#' @export
#' @seealso \code{\link{surveyPlot}} to plot a surveyor object
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

  ret <- list(
      data = x,
      surveyorDefaults = surveyor$defaults,
      plotTitle = plotTitle,
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


#------------------------------------------------------------------------------

#' Code survey data in single or array question form.
#'
#' Code survey data in array question form (i.e. with subquestions)
#' 
#' @param surveyor Surveyor object
#' @param q_id Question id
#' @param ... Other parameters passed on to downstream code_* functions
#' @seealso For an overview of the surveyor package \code{\link{surveyor}}
#' @family codeFunctions
#' @family surveyPlot arguments
#' @return data frame
#' @keywords code
#' @export
codeGuess <- function(surveyor, q_id, ...){
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
#' @param ... Other parameters passed on to downstream code* functions
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @family codeFunctions
#' @return data frame
#' @keywords code
#' @export
codeQuickArray <- function(surveyor, q_id, crossbreak=surveyor$crossbreak[[1]], wrapWidth=50, ...){
  dat <- surveyor$sdata[, q_id, drop=FALSE]
  if(is.data.frame(dat)) reps <- ncol(dat) else reps <- 1
  if(reps==0) return(NULL)
  questionText <- if(reps==1) qTextCommon(surveyor$sdata, q_id) else qTextUnique(surveyor$sdata, q_id)
  
  wrap <- function(x, width){
    vapply(x, 
        function(xx)paste0(strwrap(xx, width=width), collapse="\n"), 
        FUN.VALUE="character",
        USE.NAMES=FALSE)
  }
    
  questionTextWrapped <- wrap(questionText, width=wrapWidth)
  question <- if(reps==1) "1" else questionTextWrapped
  cbreak <- rep(crossbreak, reps)
  weight <- rep(surveyor$weight, reps)
  
  if(reps==1){
    to.order <- is.factor(dat[[1]])
    order.levels <- levels(dat[[1]])
  } else {
    to.order <- is.factor(dat$response)
    order.levels <- levels(dat$response)
  }
  
  #nrows <- if(reps==1) length(dat) else nrow(dat)
  nrows <- nrow(dat)
  #browser()
  if (reps==1L) {
    class(dat) <- class(dat)[-1]
    response <- unname(unlist(dat))
    if(!is.numeric(response)) {
      response <- wrap(response, width=wrapWidth)
      order.levels <- wrap(order.levels, width=wrapWidth)
    }
  } else {
    response <- quickStack(dat)
  }
  
  if(to.order) response <- factor(response, levels=order.levels)#, ordered=TRUE)
  
  #browser()
  ret <- quickdf(list(
      cbreak=cbreak,
#      question = if(!autosortQuestion){
#        factor(rep(question, each=nrows), levels=unique(question))#, ordered=TRUE)
#      } else {
#        rep(questionTextWrapped, each=nrows)
#      },
      question = factor(rep(question, each=nrows), levels=unique(question)),
      response = response,
      weight = weight
  ))
  
  as.surveyorCode(
      ret[!is.na(ret$response), ],
      surveyor,
      q_id,
      ...)
  
}







