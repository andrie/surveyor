#-------------------------------------------------------------------------------
### Define surveyor class and methods
#-------------------------------------------------------------------------------
# TODO: Default switch to print graphics
# TODO: Modify plotq with simpler parameters
# TODO: Modify surveyor architecture to use environments


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
#' @param outputToLatex TRUE or FALSE, determines if latex commands is output
#' @param defaultThemeSize Text size in points, passed to ggplot
#' @param questionPattern A text pattern passed to grep() to distinguish 
#' between single and array questions
#' @param subquestionAppend Indicates whether subquestion text is appended to question text
#' @param subquestionPrepend Indicates whether subquestion text is prepended to question text
#' @param fastgraphics Uses lattice graphics if true, otherwise ggplot
#' @param add_plot_title If true, adds question text as plot title 
#' @param default_colour_area Default RGB colour for areas in graphs (e.g. bars)
#' @param default_colour_point Default RGB colour for points in graphs (e.g. points)
#' @param print_table If TRUE will print the table as part of the report
#' @seealso \code{\link{as.surveyor}}, \code{\link{surveyor_update_defaults}}
#' @export
#' @examples
#' s <- surveyorDefaults()
#' s <- surveyorDefaults(outputToLatex=TRUE) 					
surveyorDefaults <- function(
    outputToLatex = TRUE,
		defaultThemeSize = 9,
		questionPattern = "_[[:digit:]]*$",
		subquestionAppend = TRUE,
		subquestionPrepend = !subquestionAppend,
		fastgraphics = FALSE,
    add_plot_title = FALSE,
		default_colour_area = rgb(127,201,127, 255, maxColorValue=255),
		default_colour_point = rgb(27, 158, 119, 255, maxColorValue=255),
    print_table = TRUE
){
	
	list(
			outputToLatex      = outputToLatex,
      defaultThemeSize   = defaultThemeSize,
			questionPattern     = questionPattern,
			subquestionAppend   = subquestionAppend,
			subquestionPrepend  = subquestionPrepend,
			fastgraphics         = fastgraphics,
      add_plot_title       = add_plot_title,
      default_colour_area  = default_colour_area,
			default_colour_point = default_colour_point,
      print_table          = print_table
  
	)
}

#' Selectively updates surveyor defaults.
#'
#' Selectively updates surveyor defaults.
#' @param surveyor Surveyor object
#' @param outputToLatex TRUE or FALSE, determines if latex commands is output
#' @param defaultThemeSize Text size in points, passed to ggplot
#' @param questionPattern A text pattern passed to grep() to distinguish 
#' between single and array questions
#' @param subquestionAppend Indicates whether subquestion text is appended to question text
#' @param subquestionPrepend Indicates whether subquestion text is prepended to question text
#' @param fastgraphics Uses lattice graphics if true, otherwise ggplot 
#' @param add_plot_title If true, adds question text as plot title 
#' @param default_colour_area Default RGB colour for areas in graphs (e.g. bars)
#' @param default_colour_point Default RGB colour for points in graphs (e.g. points)
#' @param print_table If TRUE will print the table as part of the report
#' @seealso \code{\link{as.surveyor}}, \code{\link{surveyorDefaults}}
#' @export 
surveyor_update_defaults <- function(
    surveyor,
    outputToLatex = NULL,
    defaultThemeSize = NULL,
    questionPattern = NULL,
    subquestionAppend = NULL,
    subquestionPrepend = NULL,
    fastgraphics = NULL,
    add_plot_title = NULL,
    default_colour_area = NULL,
    default_colour_point = NULL,
    print_table = NULL
){
  if(!missing(outputToLatex))      surveyor$defaults$outputToLatex <- outputToLatex
  if(!missing(defaultThemeSize))   surveyor$defaults$defaultThemeSize <- defaultThemeSize
  if(!missing(questionPattern))     surveyor$defaults$questionPattern <- questionPattern
  if(!missing(subquestionAppend))   surveyor$defaults$subquestionAppend <- subquestionAppend
  if(!missing(subquestionPrepend))  surveyor$defaults$subquestionPrepend <- subquestionPrepend
  if(!missing(fastgraphics))         surveyor$defaults$fastgraphics <- fastgraphics
  if(!missing(add_plot_title))       surveyor$defaults$add_plot_title <- add_plot_title
  if(!missing(default_colour_area))  surveyor$defaults$default_colour_area <- default_colour_area
  if(!missing(default_colour_point)) surveyor$defaults$default_colour_point <- default_colour_point
  if(!missing(print_table))          surveyor$defaults$print_table <- print_table
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






