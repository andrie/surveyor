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
      #h <- match.fun(plotFunction)(g, plotSize=plotSize, outputType, ...)
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
		
#	if (is.list(surveyor$crossbreak)) {
    ret <- lapply(onlyBreaks,
        function(i){
    			#surveyor$cbreak <- unlist(surveyor$crossbreak[i])
          #surveyor$cbreakname <- names(surveyor$crossbreak[i])
    			plotQone(crossbreak=unlist(surveyor$crossbreak[i]))
        }
      )	
#    for (i in onlyBreaks) {
#      surveyor$cbreak <- unlist(surveyor$crossbreak[i])
#      surveyor$cbreakname <- names(surveyor$crossbreak[i])
#      plotQone()
#    }		
#  } else {
#		ret <- plotQone()
#	}
  return(ret)
}

##' Codes and plots a survey question.
##' 
##' This is the top level function that determines how a question is processed,
##' coded, printed and plotted.
##' 
##' @param surveyor Surveyor object
##' @param qid Question id
##' @param codeFunction A reference to a function that processes the question data
##' @param statsFunction A reference to a function that summarizes the coded data
##' @param plotFunction A reference to a function that plots the summarized data
##' @param plotSize Size in inches of plot output, e.g. c(4,3)
##' @param outputType "latex", "ppt" or "device": Specifies destination of ouput
##' @param onlyBreaks Numeric vector that limits crossbreak processing
##' @param plotMultiplierLimits Numeric vector of length two, indicating lower and upper limit of vertical plot resizing
##' @param ... Other parameters passed to codeFunction
##' @export
##' @seealso \code{\link{as.surveyor}}
#surveyPlot <- function(
#    surveyor,
#    qid,
#    codeFunction = "codeQuickArray",
#    statsFunction = "statsGuess",
#    plotFunction = "plotGuess",
#    plotSize = if(outputType=="device") par("din") else surveyor$braid$defaultPlotSize,
#    outputType = surveyor$defaults$outputType,
#    onlyBreaks=seq_along(surveyor$crossbreak),
#    plotMultiplierLimits = if(outputType=="ppt") c(0.8, 1.2) else c(0.8, 2.5),
#    ...){
#  
#  #-------
#  
#  plotQone <- function(){
#    
#    stopifnot(is.surveyor(surveyor))
#    
#    f <- match.fun(codeFunction)(surveyor, qid, ...)
#    if (is.null(f)){
#      nothing_to_plot <- TRUE
#      message("Nothing to plot")
#    } else {
#      nothing_to_plot <- FALSE
#      g <- match.fun(statsFunction)(f, ...)
#      g$data <- subset(g$data, subset=!is.na("value")) # Remove NA values from g
#      h <- match.fun(plotFunction)(g, plotSize=plotSize, outputType, ...)
#      
#      if(outputType %in% c("latex", "ppt")){
##        if(outputType=="ppt"){
##          braidppt::braidpptNewSlide(
##              surveyor$braid,
##              title=qid,
##              text=plot_title
##          )
##        }
#        
#        catString <- surveyorPrintQuestion(
#            surveyor,
#            qid,
#            f,
#            g,
#            h, 
#            plotSize,
#            outputType=outputType,
#            plotMultiplierLimits=plotMultiplierLimits
#        )
#      }
#      if(outputType=="latex") braidWrite(surveyor$braid, catString)
#      if(outputType=="device") print(h$plot)
#    }
#  }
#  
#  #-------
#  
#  if(outputType=="ppt") if(!require(braidppt)) stop("Unable to load package braidppt")
#  
#  if(!exists(qid, surveyor$sdata) & is.null(which.q(surveyor$sdata, qid))){
#    message(paste(qid,": Question not found.  Processing aborted"))
#    return(NULL)
#  }
#  message(qid)
#  plot_title <- qTextCommon(surveyor$sdata, qid)
#  surveyor$plot_title <- plot_title
#  if(outputType=="latex"){
#    braidHeading(
#        surveyor$braid, 
#        paste(qid, plot_title), 
#        headinglevel= "section",
#        pagebreak=FALSE)
#  }
#  
#  
#  if (is.list(surveyor$crossbreak)) {
#    for (i in onlyBreaks) {
#      surveyor$cbreak <- unlist(surveyor$crossbreak[i])
#      surveyor$cbreakname <- names(surveyor$crossbreak[i])
#      plotQone()
#    }		
#  } else {
#    plotQone()
#  }
#  return(invisible(NULL))
#}


#-------------------------------------------------------------------------------



