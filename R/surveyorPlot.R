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
#' @param outputToLatex If TRUE, wraps output in latex code, otherwise print to console
#' @param ... Other parameters passed to codeFunction
#' @export
#' @seealso \code{\link{as.surveyor}}
surveyorPlot <- function(
		surveyor,
		q_id,
		codeFunction = codeQuickArray,
		statsFunction = statsGuess,
		plotFunction = plotGuess,
		plotSize = surveyor$braid$defaultPlotSize,
    outputToLatex = surveyor$defaults$outputToLatex,
		...){
  
  #------------------------------------
  
  plotQinternal <- function(){
    
    stopifnot(is.surveyor(surveyor))
    
    f <- match.fun(codeFunction)(surveyor, q_id, ...)
    if (is.null(f)){
      nothing_to_plot <- TRUE
      message("Nothing to plot")
    } else {
      nothing_to_plot <- FALSE
      g <- match.fun(statsFunction)(f, ...)
      g$data <- subset(g$data, subset=!is.na("value")) # Remove NA values from g
      h <- match.fun(plotFunction)(g, ...)
      if (outputToLatex){
        catString <- surveyorPrintQuestion(
            surveyor,
            q_id,
            f,
            g,
            h, 
            plotSize)
        braidWrite(surveyor$braid, catString)
      } else {
        print(h$plot)
      }
    }
  }
  
  #------------------------------------
  
  if(!exists(q_id, surveyor$sdata) & is.null(which.q(surveyor$sdata, q_id))){
    message(paste(q_id,": Question not found.  Processing aborted"))
    return(NULL)
  }
  message(q_id)
  plot_title <- qTextCommon(surveyor$sdata, q_id)
  surveyor$plot_title <- plot_title
  if (outputToLatex){
		braidHeading(
				surveyor$braid, 
				paste(q_id, plot_title), 
				headinglevel= "section",
				pagebreak=FALSE)
	}
		
	if (is.list(surveyor$crossbreak)) {
		for (i in seq_along(surveyor$crossbreak)) {
			surveyor$cbreak <- unlist(surveyor$crossbreak[i])
			plotQinternal()
		}		
	} else {
		plotQinternal()
	}
  return(invisible(NULL))
}

#-------------------------------------------------------------------------------

  

#-------------------------------------------------------------------------------

#' Prints surveyor question. 
#' 
#' @param surveyor A surveyor object
#' @param q_id The question id
#' @param f Results from code* function
#' @param g Results from stats* function
#' @param h Results from plot* function
#' @param plotSize the plot size in inches
#' @keywords internal
surveyorPrintQuestion <- function(surveyor, q_id, f, g, h, plotSize){
					
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
	message(paste("Now saving ", filename, sep=""))

	# Adjust vertical size of plot depending on number of questions
	# Make the assumption that 7 questions can fit on a plot
	# Limit vertical size to [1, 3]*size of default
	height_multiplier <- ifelse(
			is.numeric(g$nquestion), 
			min(3, max(1, g$nquestion / 7)),
			1
	)
	#message(paste("In surveyorPrintQuestion, height_multiplier = ", height_multiplier))
	braidPlot(surveyor$braid, h$plot, filename=filename,
      width=plotSize[1], height=(plotSize[2] * height_multiplier))

	catString <- ifelse(surveyor$defaults$print_table, tableGuess(g), "")
	
	return(catString)
}


