#-------------------------------------------------------------------------------
# Define surveyor class and methods
#-------------------------------------------------------------------------------

#' Creates object of class surveyor.
#' 
#' A surveyor object describes the data and meta-data in the survey that will be analysed by the analysis and reporting functions in the surveyor package.
#' 
#' @param sdata surveydata object
#' @param crossbreak List of factors that contain crossbreak data.  
#' @param weight Numeric vector with weighting data
#' @param defaults Surveyor defaults.  See also \code{\link{surveyorDefaults}}
#' @param braid A braid object
#' @return A list object of class surveyor
#' @export
#' @examples
#' sdata <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' qtext <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' varlabels(sdata) <- qtext
#' sdata <- as.surveydata(sdata)
#' b <- as.braid()
#' s <- as.surveyor(sdata, crossbreak=c("aa", "bb"), weight=c(1,1), braid=b) 					
as.surveyor <- function(
		sdata, 
		crossbreak = sdata$crossbreak,
		weight     = sdata$weight,
		defaults   = surveyorDefaults(),
    braid = as.braid()
	){
  if (!surveydata::is.surveydata(sdata)){
		stop("Surveyor: sdata must be a surveydata object")
	}
	
	if (is.list(crossbreak)) {
		if (any(lapply(crossbreak, length) != nrow(sd))) {
			stop ("Surveyor object: each element in crossbreak list must match sdata in length")
		} else {
			cbreak <- unlist(crossbreak[1])
		}
	} else {
		if (length(crossbreak) != nrow(sdata)){
			stop("Surveyor object: crossbreak must match sdata in length")
		} else {
			cbreak <- crossbreak
		}
	}

	if (length(weight) != nrow(sdata)){
		stop("Surveyor object: Weight must match sdata in length")
	}
	if (!is.numeric(weight)){
		stop("Surveyor object: Weight must be numeric")
	}
#  browser()
	structure(
			list(
					sdata      = sdata, 
					cbreak     = cbreak,
          plot_title = NULL,
					crossbreak = crossbreak,
					weight     = weight,
					defaults   = defaults,
          braid      = braid
			), 
			class = "surveyor"
	)
}


#-------------------------------------------------------------------------------

#' Tests that object is of class surveyor object.
#' 
#' @param x Object to be tested
#' @export
#' @examples 
#' sdata <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' varlabels(sdata) <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' sdata <- as.surveydata(sdata)
#' b <- as.braid()
#' s <- as.surveyor(sdata, crossbreak=c("aa", "bb"), weight=c(1,1), b)
#' is.surveyor(s) # TRUE
#' is.surveyor("String") #FALSE           
is.surveyor <- function(x){
  if (inherits(x, "surveyor")) {
    ifelse(
        all(
            is.surveydata(x$sdata),
            !is.null(x$crossbreak),
            !is.null(x$weight),
            !is.null(x$defaults)
        ), TRUE, FALSE)
  } else {
    FALSE
  }
}


#-------------------------------------------------------------------------------

#surveyorDefaults <- function(surveyor){
#  surveyor$defaults
#}
#


#' Initialises surveyor object defaults.
#' 
#' @param outputType Character string specifying the destination of output: "latex", "ppt" or "device".  If "device", graphs are sent to the default device (typically the RGgui plot terminal)
#' @param defaultThemeSize Text size in points, passed to ggplot
#' @param plotType Either \code{ggplot} or \code{lattice}
#' @param fastgraphics Uses lattice graphics if true, otherwise ggplot
#' @param graphicFormat Device type for saving graphic plots.  Currently only pdf and wmf is supported.
#' @param addPlotTitle If true, adds question text as plot title. Defaults to TRUE if \code{ouputType} is either "ppt" or "device"
#' @param defaultBrewerPal Names of ColorBrewer pallette to use 
#' @param revBrewerPal If TRUE, reverse the order of \code{defaultBrewerPal}
#' @param printTable If TRUE will print the table as part of the report
#' @seealso \code{\link{as.surveyor}}, \code{\link{surveyorUpdateDefaults}}
#' @export
#' @examples
#' s <- surveyorDefaults()
#' s <- surveyorDefaults(outputType="latex") 					
surveyorDefaults <- function(
    outputType = c("latex", "ppt", "device"),
		defaultThemeSize = 9,
		plotType = c("ggplot", "lattice"),
    fastgraphics = plotType[1]=="lattice",
    graphicFormat = c("pdf", "wmf"),
    addPlotTitle = outputType %in% c("ppt", "device"),
		defaultBrewerPal = "Set2",
    revBrewerPal = FALSE,
    printTable = TRUE
){
	
  outputType <- outputType[1]
  if(!is.null(outputType) && outputType=="ppt") graphicFormat <- "wmf"
  
	list(
      outputType         = outputType,
      defaultThemeSize   = defaultThemeSize,
			fastgraphics       = fastgraphics,
      graphicFormat      = graphicFormat[1],
      addPlotTitle       = addPlotTitle,
      brewerPalette   = defaultBrewerPal,
      revBrewerPal       = revBrewerPal,
      printTable         = printTable
	)
}

#' Selectively updates surveyor defaults.
#'
#' Selectively updates surveyor defaults.
#' 
#' @inheritParams surveyorDefaults
#' @seealso \code{\link{as.surveyor}}, \code{\link{surveyorDefaults}}
#' @export 
surveyorUpdateDefaults <- function(
    surveyor,
    outputType = NULL,
    defaultThemeSize = NULL,
    plotType = NULL,
    fastgraphics = NULL,
    graphicFormat = NULL,
    addPlotTitle = NULL,
    brewerPalette = NULL,
    revBrewerPal = NULL,
    printTable = NULL
){
  if(!is.null(outputType)){
      if(outputType=="ppt") graphicFormat <- "wmf"
      if(outputType %in% c("ppt", "device")) addPlotTitle <- TRUE
    }
  

  if(!missing(outputType))         surveyor$defaults$outputType <- outputType
  if(!missing(defaultThemeSize))   surveyor$defaults$defaultThemeSize <- defaultThemeSize
  if(!missing(plotType))           surveyor$defaults$fastgraphics <- plotType[1]=="lattice"
  if(!missing(fastgraphics))       surveyor$defaults$fastgraphics <- fastgraphics
  if(!missing(graphicFormat))      surveyor$defaults$graphicFormat <- graphicFormat
  if(!missing(addPlotTitle))       surveyor$defaults$addPlotTitle <- addPlotTitle
  if(!missing(brewerPalette))   surveyor$defaults$brewerPalette <- brewerPalette
  if(!missing(revBrewerPal))       surveyor$defaults$revBrewerPal <- revBrewerPal
  if(!missing(printTable))         surveyor$defaults$printTable <- printTable
  surveyor
}

#-------------------------------------------------------------------------------

#' Prints surveyor object.
#' 
#' Prints surveyor object
#' 
#' @param x surveyor object
#' @param ... ignored
#' @method print surveyor
print.surveyor <- function(x, ...){
	cat("Surveyor\n\n")
	print.listof(x)
}






