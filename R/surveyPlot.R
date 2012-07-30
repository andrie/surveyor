#-------------------------------------------------------------------------------
#   Plugin architecture to process each question
#-------------------------------------------------------------------------------

#' Codes and plots a survey question.
#' 
#' This function takes extracts the data from a \code{surveydata} object, then analyses and plots it.
#' 
#' This is a generic function, with an implementation for class \code{surveyor}, see also \code{\link{as.surveyor}}. The package \code{surveybraid} implements additional methods for \code{surveybraid} and \code{surveybraidppt}.  See also \code{\link[surveybraid]{as.surveybraid}} and \code{\link[surveybraid]{as.surveybraidppt}}. 
#' 
#' @param x Object to plot, of class \code{surveyor}, see also \code{\link{as.surveyor}}
#' @param qid String. Unique question identifier, e.g. "Q4"

#' @param statsFunction A reference to a function that summarizes the coded data. Defaults to \code{\link{statsGuess}}
#' @param plotFunction A reference to a function that plots the summarized data. Defaults to \code{\link{plotGuess}}
#' @param codeFunction A reference to a function that processes the question data. Defaults to \code{\link{codeGuess}}
#' @param crossbreak A list of crossbreak vectors (each must be a named factor).  See also \code{\link{as.surveyor}}
#' @param onlyBreaks Numeric vector that limits crossbreak processing
#' @param ... Passed to methods
#' @export 
#' @return Depends on method, but typically a list of \code{\link{as.surveyorPlot}} objects
#' @seealso \code{\link{as.surveyor}}
surveyPlot <- function(
    x,
    qid,
    statsFunction = "statsGuess",
    plotFunction = "plotGuess",
    codeFunction = "codeQuickArray",
    crossbreak,
    onlyBreaks=seq_along(x$crossbreak),
    ...){
  UseMethod("surveyPlot")
}

#' @rdname surveyPlot
#' @method surveyPlot surveyor
#' @export
surveyPlot.surveyor <- function(
		x,
		qid,
		statsFunction = "statsGuess",
		plotFunction = "plotGuess",
    codeFunction = "codeQuickArray",
    crossbreak=x$crossbreak,
    onlyBreaks=seq_along(x$crossbreak),
    ...){
  
  surveyor <- x
  
  #-------
  
  plotQone <- function(crossbreak){
    
    stopifnot(is.surveyor(surveyor))
    
    f <- match.fun(codeFunction)(surveyor, qid, crossbreak=crossbreak, ...)
    if (is.null(f)){
      nothing_to_plot <- TRUE
      message("Nothing to plot")
    } else {
      nothing_to_plot <- FALSE
      g <- match.fun(statsFunction)(f, ...)
      g$data <- subset(g$data, subset=!is.na("value")) # Remove NA values from g
      h <- match.fun(plotFunction)(g, ...)
      h
      
      }
  }
  
  #-------
  
  if(!exists(qid, surveyor$sdata) & is.null(which.q(surveyor$sdata, qid))){
    message(paste(qid,": Question not found.  Processing aborted"))
    return(NULL)
  }

  plot_title <- qTextCommon(surveyor$sdata, qid)
  surveyor$plot_title <- plot_title
  
  #browser()
		
    ret <- lapply(onlyBreaks,
        function(i){
          plotQone(crossbreak=crossbreak[[i]])
        }
      )	
  return(ret)
}




