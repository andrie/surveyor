#-------------------------------------------------------------------------------
# Define surveyor class and methods
#-------------------------------------------------------------------------------

#' Creates object of class surveyor.
#' 
#' A surveyor object describes the data and meta-data in the survey that will be analysed by the analysis and reporting functions in the surveyor package.
#' 
#' @param sdata surveydata object. See also \code{\link[surveydata]{as.surveydata}}
#' @param crossbreak List of named factors that contain crossbreak data. Each factor must have the same length as the columns in the \code{surveydata} object  
#' @param weight Numeric vector with weighting data. Must have the same length as the columns in the \code{surveydata} object 
#' @param defaults Surveyor defaults.  See also \code{\link{surveyorDefaults}}
#' @return A list object of class \code{surveyor}:
#' \describe{
#' \item{sdata}{surveydata object. See also \code{\link[surveydata]{as.surveydata}}}
#' \item{cbreak}{}
#' \item{plot_title}{}
#' \item{weight}{}
#' \item{defaults}{}
#' }
#' @export
#' @examples
#' library(surveydata)
#' sdata <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' qtext <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' varlabels(sdata) <- qtext
#' sdata <- as.surveydata(sdata, renameVarlabels=TRUE)
#' s <- as.surveyor(sdata, crossbreak=list(breaks=factor(c("aa", "bb"))), weight=c(1,1))
as.surveyor <- function(
		sdata, 
		crossbreak = sdata$crossbreak,
		weight     = sdata$weight,
		defaults   = surveyorDefaults() #,
	){
  if (!surveydata::is.surveydata(sdata)){
		stop("Surveyor: sdata must be a surveydata object")
	}
	
#	if (is.list(crossbreak)) {
#		if (any(lapply(crossbreak, length) != nrow(sdata))) {
#			stop ("Surveyor object: each element in crossbreak list must match sdata in length")
#		} else {
#			cbreak <- unlist(crossbreak[1])
#		}
#	} else {
#		if (length(crossbreak) != nrow(sdata)){
#			stop("Surveyor object: crossbreak must match sdata in length")
#		} else {
#			cbreak <- crossbreak
#		}
#	}
  
  if(!isValidCrossbreak(crossbreak, nrow=nrow(sdata))) stop("Invalid crossbreak")

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
#					cbreak     = cbreak,
					crossbreak = crossbreak,
					weight     = weight,
					defaults   = defaults
			), 
			class = "surveyor"
	)
}

#' Tests that crossbreak is valid.
#' 
#' A crossbreak object must be a list, and each element must be a named vector
#' @param x A crossbreak object to test
#' @param surveyorObject A surveyor object
#' @param nrow The required length of the crossbreak, i.e. the number of rows in the surveydata object
#' @param throwErrors Logical. If TRUE, throws errors, else simply returns TRUE or FALSE
#' @keywords internal
isValidCrossbreak <- function(x, surveyorObject, nrow=nrow(surveyorObject$sdata), throwErrors=TRUE){
  if(!is.list(x)){
    if(throwErrors) stop("crossbreak must be a list")
    return(FALSE)
  }
  if (any(lapply(x, length) != nrow)) {
    if(throwErrors) stop ("Each element in crossbreak list must match sdata in length")
    return(FALSE)
  } 
#  if (!all(sapply(x, is.factor))) {
#    if(throwErrors) stop ("Each element in crossbreak list must be a factor")
#    return(FALSE)
#  } 
  if(any(is.null(sapply(x, names)))){
    if(throwErrors) stop ("Each element in crossbreak list must be a named vector")
    return(FALSE)
  }
  # it is a list, and all elements are the correct length, so return TRUE
  TRUE
}


#-------------------------------------------------------------------------------

#' Tests that object is of class surveyor object.
#' 
#' @param x Object to be tested
#' @export
#' @examples 
#' library(surveydata)
#' sdata <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' varlabels(sdata) <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' sdata <- as.surveydata(sdata, renameVarlabels=TRUE)
#' s <- as.surveyor(sdata, crossbreak=list(breaks=factor(c("aa", "bb"))), weight=c(1,1))
#' is.surveyor(s) # TRUE
#' is.surveyor("String") #FALSE           
is.surveyor <- function(x){
  if (!inherits(x, "surveyor")) return(FALSE)
  if(all(
      is.surveydata(x$sdata),
      #!is.null(x$crossbreak),
      isValidCrossbreak(x$crossbreak, nrow=nrow(x$sdata), throwErrors=FALSE),
      !is.null(x$weight),
      !is.null(x$defaults)
    )) TRUE else FALSE
}


#-------------------------------------------------------------------------------

#surveyorDefaults <- function(surveyor){
#  surveyor$defaults
#}
#


#' Initialises surveyor object defaults.
#' 
#' @param defaultThemeSize Text size in points, passed to ggplot
#' @param plotType Either \code{ggplot} or \code{lattice}
#' @param fastgraphics Uses lattice graphics if true, otherwise ggplot
#' @param addPlotTitle If true, adds question text as plot title. 
#' @param defaultBrewerPal Names of ColorBrewer palette to use 
#' @param revBrewerPal If TRUE, reverse the order of \code{defaultBrewerPal}
#' @param printTable If TRUE will print the table as part of the report
#' @seealso \code{\link{as.surveyor}}, \code{\link{surveyorUpdateDefaults}}
#' @export
#' @examples
#' s <- surveyorDefaults()
#' s <- surveyorDefaults(defaultThemeSize=12) 					
surveyorDefaults <- function(
		defaultThemeSize = 9,
		plotType = c("ggplot", "lattice"),
    fastgraphics = plotType[1]=="lattice",
    addPlotTitle = TRUE,
		defaultBrewerPal = "Set2",
    revBrewerPal = FALSE,
    printTable = TRUE
){
	
  
	list(
      defaultThemeSize   = defaultThemeSize,
			fastgraphics       = fastgraphics,
      addPlotTitle       = addPlotTitle,
      brewerPalette      = defaultBrewerPal,
      revBrewerPal       = revBrewerPal,
      printTable         = printTable
	)
}

#' Selectively updates surveyor defaults.
#'
#' Selectively updates surveyor defaults.
#' 
#' @inheritParams surveyorDefaults
#' @param surveyor Surveyor object
#' @param brewerPalette Names of ColorBrewer palette to use 
#' @seealso \code{\link{as.surveyor}}, \code{\link{surveyorDefaults}}
#' @export 
surveyorUpdateDefaults <- function(
    surveyor,
#    outputType = NULL,
    defaultThemeSize = NULL,
#    plotType = NULL,
    fastgraphics = NULL,
#    graphicFormat = NULL,
    addPlotTitle = NULL,
    brewerPalette = NULL,
    revBrewerPal = NULL,
    printTable = NULL
){
#  if(!is.null(outputType)){
#      if(outputType=="ppt") graphicFormat <- "wmf"
#      if(outputType %in% c("ppt", "device") & missing(addPlotTitle)) addPlotTitle <- TRUE
#    }
  

#  if(!missing(outputType))         surveyor$defaults$outputType <- outputType
  if(!missing(defaultThemeSize))   surveyor$defaults$defaultThemeSize <- defaultThemeSize
  #if(!missing(plotType))           surveyor$defaults$fastgraphics <- plotType[1]=="lattice"
  if(!missing(fastgraphics))       surveyor$defaults$fastgraphics <- fastgraphics
#  if(!missing(graphicFormat))      surveyor$defaults$graphicFormat <- graphicFormat
  if(!missing(addPlotTitle))       surveyor$defaults$addPlotTitle <- addPlotTitle
  if(!missing(brewerPalette))      surveyor$defaults$brewerPalette <- brewerPalette
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
#' @export
print.surveyor <- function(x, ...){
	cat("Surveyor\n\n")
	print.listof(x)
}


#' Subsetting surveyor objects.
#' 
#' Returns subset of surveyor object by applying the subset to the data, weight as well as crossbreak elements of the surveyor object.
#' @param x Surveyor object
#' @param subset Subset to evaluate. This is evaluated in the environment of the surveyor data, i.e. \code{x$sdata}
#' @param ... Ignored
#' @method subset surveyor
#' @export
#' @return surveyor object
subset.surveyor <- function(x, subset, ...){
  if(missing(subset))
    r <- TRUE
  else {
    e <- substitute(subset)
    r <- eval(e, x$sdata, parent.frame())
  }
  x$sdata <- x$sdata[r, ]
  x$weight <- x$weight[r]
  x$crossbreak <- lapply(x$crossbreak, function(x)x[r])
  x
}





