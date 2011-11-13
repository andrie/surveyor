#-------------------------------------------------------------------------------
#   Plugin architecture to process each question
#-------------------------------------------------------------------------------

#' Codes and plots a survey question.
#' 
#' This is the top level function that determines how a question is processed,
#' coded, printed and plotted.
#' 
#' @param surveyor Surveyor object
#' @param q_id Question id
#' @param codeFunction A reference to a function that processes the question data
#' @param statsFunction A reference to a function that summarizes the coded data
#' @param plotFunction A reference to a function that plots the summarized data
#' @param plotSize Size in inches of plot output, e.g. c(4,3)
#' @param outputType "latex", "ppt" or "device": Specifies destination of ouput
#' @param onlyBreaks Numeric vector that limits crossbreak processing
#' @param plotMultiplierLimits Numeric vector of length two, indicating lower and upper limit of vertical plot resizing
#' @param ... Other parameters passed to codeFunction
#' @export
#' @seealso \code{\link{as.surveyor}}
surveyorPlot <- function(
		surveyor,
		q_id,
		codeFunction = codeQuickArray,
		statsFunction = statsGuess,
		plotFunction = plotGuess,
		plotSize = if(outputType=="device") par("din") else surveyor$braid$defaultPlotSize,
    outputType = surveyor$defaults$outputType,
    onlyBreaks=seq_along(surveyor$crossbreak),
    plotMultiplierLimits = if(outputType=="ppt") c(0.8, 1.2) else c(0.8, 2.5),
		...){
  
  #-------
  
  plotQone <- function(){
    
    stopifnot(is.surveyor(surveyor))
    
    f <- match.fun(codeFunction)(surveyor, q_id, ...)
    if (is.null(f)){
      nothing_to_plot <- TRUE
      message("Nothing to plot")
    } else {
      nothing_to_plot <- FALSE
      g <- match.fun(statsFunction)(f, ...)
      g$data <- subset(g$data, subset=!is.na("value")) # Remove NA values from g
      h <- match.fun(plotFunction)(g, plotSize=plotSize, outputType, ...)
      
      if(outputType %in% c("latex", "ppt")){
        if(outputType=="ppt"){
          braidppt::braidpptNewSlide(
              surveyor$braid,
              title=q_id,
              text=plot_title
          )
        }
          
        catString <- surveyorPrintQuestion(
            surveyor,
            q_id,
            f,
            g,
            h, 
            plotSize,
            outputType=outputType,
            plotMultiplierLimits=plotMultiplierLimits
        )
      }
      if(outputType=="latex") braidWrite(surveyor$braid, catString)
      if(outputType=="device") print(h$plot)
    }
  }
  
  #-------
  
  if(outputType=="ppt") if(!require(braidppt)) stop("Unable to load package braidppt")
  
  if(!exists(q_id, surveyor$sdata) & is.null(which.q(surveyor$sdata, q_id))){
    message(paste(q_id,": Question not found.  Processing aborted"))
    return(NULL)
  }
  message(q_id)
  plot_title <- qTextCommon(surveyor$sdata, q_id)
  surveyor$plot_title <- plot_title
  if(outputType=="latex"){
        braidHeading(
    				surveyor$braid, 
    				paste(q_id, plot_title), 
    				headinglevel= "section",
    				pagebreak=FALSE)
      }
	
		
	if (is.list(surveyor$crossbreak)) {
		for (i in onlyBreaks) {
			surveyor$cbreak <- unlist(surveyor$crossbreak[i])
      surveyor$cbreakname <- names(surveyor$crossbreak[i])
			plotQone()
		}		
	} else {
		plotQone()
	}
  return(invisible(NULL))
}

 

#-------------------------------------------------------------------------------

#' Prints surveyor question. 
#' 
#' @inheritParams surveyorPlot  
#' @inheritParams surveyorDefaults  
#' @keywords internal
surveyorPrintQuestion <- function(surveyor, q_id, f, g, h, plotSize, outputType, 
    plotMultiplierLimits=c(1, 1)){
					
	if (is.null(f)){
		catString <- "\nNo data\n\n"
    return(catString)
	}
  
  if(class(h$plot)=="text"){
    catString <- h$plot
    return(catString)
  }
    
	# Print plot
	filename <- braidFilename(surveyor$braid)
  filename <- paste(q_id, "_", surveyor$cbreakname, ".", surveyor$defaults$graphicFormat, sep="")
  message(paste(" --", filename))

	# Adjust vertical size of plot depending on number of questions
	# Make the assumption that 7 questions can fit on a plot
	# Limit vertical size to [1, 3]*size of default
  #browser()
  plotMin <- plotMultiplierLimits[1]
  plotMax <- plotMultiplierLimits[2]
  height_multiplier <- ifelse(
			is.numeric(g$nquestion), 
			min(plotMax, max(plotMin, g$nquestion / 7)),
			plotMin
	)
	#message(paste("In surveyorPrintQuestion, height_multiplier = ", height_multiplier))
  switch(outputType,
      latex = {
      	braidPlot(surveyor$braid, h$plot, filename=filename,
            width=plotSize[1], height=(plotSize[2] * height_multiplier), Qid=q_id)
      },
      ppt = {
        braidppt::braidpptPlot(surveyor$braid, h$plot, filename=filename,
            width=plotSize[1], height=(plotSize[2] * height_multiplier), Qid=q_id)
      }
  )

	catString <- if(surveyor$defaults$printTable) tableGuess(g) else ""
	
	return(catString)
}


