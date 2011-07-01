################################################################################
### Define surveyor class and methods
################################################################################
# TODO: Default switch to print graphics
# TODO: Modify plotq with simpler parameters
# TODO: Modify surveyor architecture to use environments


#' Creates object of class surveyor.
#' 
#' A surveyor object describes the data and meta-data in the survey that will be analysed by the analysis and reporting functions in the surveyor package.
#' 
#' @param q_data data frame with survey data
#' @param q_text The question metadata that contains the original text question
#' @param crossbreak List of factors that contain crossbreak data.  
#' @param weight Numeric vector with weighting data
#' @param defaults Surveyor defaults.  See also \code{\link{surveyor_defaults}}
#' @param braid A braid object
#' @return A list object of class surveyor
#' @export
#' @examples
#' q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
#' b <- as.braid()
#' s <- as.surveyor(q_data, q_text, crossbreak=c("aa", "bb"), weight=c(1,1), braid=b) 					
as.surveyor <- function(
		q_data, 
		q_text, 
		crossbreak = q_data$crossbreak,
		weight     = q_data$weight,
		defaults   = surveyor_defaults(),
    braid = as.braid()
	){
	if (!identical(names(q_data), names(q_text))){
		stop("Surveyor object: The names of q_data and q_text must match")
	}
	
	if (is.list(crossbreak)) {
		if (any(lapply(crossbreak, length) != nrow(sd))) {
			stop ("Surveyor object: each element in crossbreak list must match q_data in length")
		} else {
			cbreak <- unlist(crossbreak[1])
		}
	} else {
		if (length(crossbreak) != nrow(q_data)){
			stop("Surveyor object: crossbreak must match q_data in length")
		} else {
			cbreak <- crossbreak
		}
	}

	if (length(weight) != nrow(q_data)){
		stop("Surveyor object: Weight must match q_data in length")
	}
	if (!is.numeric(weight)){
		stop("Surveyor object: Weight must be numeric")
	}
	structure(
			list(
					q_data     = q_data, 
					q_text     = q_text,
					cbreak     = cbreak,
					crossbreak = crossbreak,
					weight     = weight,
					defaults   = defaults,
          braid      = braid
			), 
			class = "surveyor"
	)
}

###############################################################################


#' Initialises surveyor object defaults.
#' 
#' @param output_to_latex TRUE or FALSE, determines if latex commands is output
#' @param default_theme_size Text size in points, passed to ggplot
#' @param question_pattern A text pattern passed to grep() to distinguish 
#' between single and array questions
#' @param subquestion_append Indicates whether subquestion text is appended to question text
#' @param subquestion_prepend Indicates whether subquestion text is prepended to question text
#' @param fastgraphics Uses lattice graphics if true, otherwise ggplot
#' @param add_plot_title If true, adds question text as plot title 
#' @param default_colour_area Default RGB colour for areas in graphs (e.g. bars)
#' @param default_colour_point Default RGB colour for points in graphs (e.g. points)
#' @param print_table If TRUE will print the table as part of the report
#' @seealso \code{\link{as.surveyor}}, \code{\link{surveyor_update_defaults}}
#' @export
#' @examples
#' s <- surveyor_defaults()
#' s <- surveyor_defaults(output_to_latex=TRUE) 					
surveyor_defaults <- function(
    output_to_latex = TRUE,
		default_theme_size = 9,
		question_pattern = "_[[:digit:]]*$",
		subquestion_append = TRUE,
		subquestion_prepend = !subquestion_append,
		fastgraphics = FALSE,
    add_plot_title = FALSE,
		default_colour_area = rgb(127,201,127, 255, maxColorValue=255),
		default_colour_point = rgb(27, 158, 119, 255, maxColorValue=255),
    print_table = TRUE
){
	
	list(
			output_to_latex      = output_to_latex,
      default_theme_size   = default_theme_size,
			question_pattern     = question_pattern,
			subquestion_append   = subquestion_append,
			subquestion_prepend  = subquestion_prepend,
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
#' @param output_to_latex TRUE or FALSE, determines if latex commands is output
#' @param default_theme_size Text size in points, passed to ggplot
#' @param question_pattern A text pattern passed to grep() to distinguish 
#' between single and array questions
#' @param subquestion_append Indicates whether subquestion text is appended to question text
#' @param subquestion_prepend Indicates whether subquestion text is prepended to question text
#' @param fastgraphics Uses lattice graphics if true, otherwise ggplot 
#' @param add_plot_title If true, adds question text as plot title 
#' @param default_colour_area Default RGB colour for areas in graphs (e.g. bars)
#' @param default_colour_point Default RGB colour for points in graphs (e.g. points)
#' @param print_table If TRUE will print the table as part of the report
#' @seealso \code{\link{as.surveyor}}, \code{\link{surveyor_defaults}}
#' @export 
surveyor_update_defaults <- function(
    surveyor,
    output_to_latex = NULL,
    default_theme_size = NULL,
    question_pattern = NULL,
    subquestion_append = NULL,
    subquestion_prepend = NULL,
    fastgraphics = NULL,
    add_plot_title = NULL,
    default_colour_area = NULL,
    default_colour_point = NULL,
    print_table = NULL
){
  if(!missing(output_to_latex))      surveyor$defaults$output_to_latex <- output_to_latex
  if(!missing(default_theme_size))   surveyor$defaults$default_theme_size <- default_theme_size
  if(!missing(question_pattern))     surveyor$defaults$question_pattern <- question_pattern
  if(!missing(subquestion_append))   surveyor$defaults$subquestion_append <- subquestion_append
  if(!missing(subquestion_prepend))  surveyor$defaults$subquestion_prepend <- subquestion_prepend
  if(!missing(fastgraphics))         surveyor$defaults$fastgraphics <- fastgraphics
  if(!missing(add_plot_title))       surveyor$defaults$add_plot_title <- add_plot_title
  if(!missing(default_colour_area))  surveyor$defaults$default_colour_area <- default_colour_area
  if(!missing(default_colour_point)) surveyor$defaults$default_colour_point <- default_colour_point
  if(!missing(print_table))          surveyor$defaults$print_table <- print_table
  surveyor
}

################################################################################

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

################################################################################

#' Tests that object is of class surveyor object.
#' 
#' @param x Object to be tested
#' @method is surveyor
#' @examples 
#' q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
#' b <- as.braid()
#' s <- as.surveyor(q_data, q_text, crossbreak=c("aa", "bb"), weight=c(1,1), b)
#' is.surveyor(s) # TRUE
#' is.surveyor("String") #FALSE 					
is.surveyor <- function(x){
	if (inherits(x, "surveyor")) {
    ifelse(
        all(
          !is.null(x$q_data),
  				!is.null(x$q_text),
  				!is.null(x$crossbreak),
  				!is.null(x$weight),
  				!is.null(x$defaults)
		    ), TRUE, FALSE)
		} else {
      FALSE
    }
}





################################################################################
### Plugin architecture to process each question
################################################################################

#' Codes and plots a survey question
#' 
#' This is the top level function that determines how a question is processed,
#' coded, printed and plotted.
#' 
#' @param surveyor Surveyor object
#' @param q_id Question id
#' @param code_function A reference to a function that processes the question data
#' @param stats_function A reference to a function that summarizes the coded data
#' @param plot_function A reference to a function that plots the summarized data
#' @param plot_size Size in inches of plot output, e.g. c(4,3)
#' @param output_to_latex If TRUE, wraps output in latex code, otherwise print to console
#' @param ... Other parameters passed to code_function
#' @export
#' @seealso \code{\link{as.surveyor}}
surveyor_plot <- function(
		surveyor,
		q_id,
		code_function = code_guess,
		stats_function = stats_guess,
		plot_function = plot_guess,
		plot_size = surveyor$braid$default_plot_size,
    output_to_latex = surveyor$defaults$output_to_latex,
		...){
	
	###  plot_q internal functions
	
	plot_q_internal <- function(){
		
		if(!(is.surveyor(surveyor))){stop("You must pass a valid surveyor object to plot_q")}
    
		f <- code_function(surveyor, q_id, ...)
		if (is.null(f)){
			nothing_to_plot <- TRUE
		} else {
			g <- stats_function(f)
			g2 <- g
			g2$data <- subset(g2$data, subset=!is.na("value")) # Remove NA values from g
			h <- plot_function(g2, surveyor)
			nothing_to_plot <- FALSE
		}	
		
		if (!output_to_latex){
			ifelse(nothing_to_plot, message("Nothing to plot"), print(h$plot))
		} else {
  			cat_string <- surveyor_print_question(
  					surveyor,
  					q_id,
  					f,
  					g,
  					h, 
  					plot_size)
  			braid_write(surveyor$braid, cat_string)
		}
		
	}
	
	###  plot_q main function
	
  if(!exists(q_id, surveyor$q_data) & is.null(get_q_subquestions(surveyor$q_data, q_id, surveyor))){
    message(paste(q_id,": Question not found.  Processing aborted"))
    return(NULL)
  }
  message(q_id)
  if (surveyor$defaults$output_to_latex){
		braid_heading(
				surveyor$braid, 
				paste(q_id, get_q_text(surveyor, q_id)), 
				headinglevel= "section",
				pagebreak=FALSE)
	}
		
	if (is.list(surveyor$crossbreak)) {
		for (i in seq_along(surveyor$crossbreak)) {
			surveyor$cbreak <- unlist(surveyor$crossbreak[i])
			plot_q_internal()
		}		
	} else {
		plot_q_internal()
	}

}


################################################################################

#' Prints surveyor question. 
#' 
#' @param surveyor A surveyor object
#' @param q_id The question id
#' @param f Results from code_* function
#' @param g Results from stats_* function
#' @param h Results from plot_* function
#' @param plot_size the plot size in inches
#' @keywords internal
surveyor_print_question <- function(surveyor, q_id, f, g, h, plot_size){
	# Print question description
#	surveyor_write(surveyor, qtext) 
					
	if (is.null(f)){
		cat_string <- "\nNo data\n\n"
    return(cat_string)
	}
  
  if(class(h$plot)=="text"){
    cat_string <- h$plot
    return(cat_string)
  }
    
	# Print plot
	filename <- braid_filename(surveyor$braid)
	message(paste("Now saving ", filename, sep=""))

	# Adjust vertical size of plot depending on number of questions
	# Make the assumption that 7 questions can fit on a plot
	# Limit vertical size to [1, 3]*size of default
	height_multiplier <- ifelse(
			is.numeric(g$nquestion), 
			min(3, max(1, g$nquestion / 7)),
			1
	)
	#message(paste("In surveyor_print_question, height_multiplier = ", height_multiplier))
	braid_plot(surveyor$braid, h$plot, filename=filename,
      width=plot_size[1], height=(plot_size[2] * height_multiplier))

	cat_string <- ifelse(surveyor$defaults$print_table, table_guess(g), "")
	
	return(cat_string)
}

#' Sets up default surveyor theme for use in ggplot. 
#' 
#' @param surveyor A surveyor object
#' @param q_id The question id
#' @param counter The file number
#' @param f Results from code_* function
#' @param g Results from stats_* function
#' @param h Results from plot_* function
#' @param plot_size the plot size in inches
#' @keywords internal
theme_surveyor <- function (base_size = 12, base_family = ""){
	structure(
			list(
					axis.line = theme_blank(), 
					axis.text.x = theme_text(
							family = base_family, 
							size = base_size * 0.8, 
							lineheight = 0.9, 
							colour = "grey20", #"grey50", 
							vjust = 1), 
					axis.text.y = theme_text(
							family = base_family, 
							size = base_size * 0.8, 
							lineheight = 0.9, 
							colour = "grey20", #"grey50",
							hjust = 1), 
					axis.ticks = theme_segment(colour = "grey50"), 
					axis.title.x = theme_text(family = base_family, size = base_size, vjust = 0.5), 
					axis.title.y = theme_text(family = base_family, 
							size = base_size, angle = 90, vjust = 0.5), 
					axis.ticks.length = unit(0.15, "cm"), 
					axis.ticks.margin = unit(0.1, "cm"), legend.background = theme_rect(colour = "white"), 
					legend.key = theme_rect(fill = "grey95", colour = "white"), 
					legend.key.size = unit(1.2, "lines"), legend.key.height = NA, 
					legend.key.width = NA, 
					legend.text = theme_text(family = base_family, size = base_size * 0.8), 
					legend.text.align = NA, 
					legend.title = theme_text(family = base_family, size = base_size * 
									0.8, face = "bold", hjust = 0), 
					legend.title.align = NA, 
					legend.position = "right", 
					legend.direction = "vertical", 
					legend.box = NA, 
					panel.background = theme_rect(fill = "grey90", 
							colour = NA), 
					panel.border = theme_blank(), panel.grid.major = theme_line(colour = "white"), 
					panel.grid.minor = theme_line(colour = "grey95", size = 0.25), 
					panel.margin = unit(0.25, "lines"), 
					strip.background = theme_rect(fill = "grey80", 
							colour = NA), 
					strip.text.x = theme_text(family = base_family, size = base_size * 0.8), 
					strip.text.y = theme_text(family = base_family, size = base_size * 0.8, angle = -90), 
					plot.background = theme_rect(colour = NA, fill = "white"), 
					plot.title = theme_text(family = base_family, size = base_size * 1.2), 
					plot.margin = unit(c(1, 1, 0.5, 0.5), "lines")
			), 
			class = "options"
	)
}

################################################################################
# TODO: Surveyor: Define question class
# 
# Author: Andrie
###############################################################################



# Class question

# Attributes
# - q_id
# - q_text
# - data
# - coding (e.g. single, array, multiple, text)
# - analysis, e.g. bin, rank, net_score, maxdiff, etc.
# - plot

# Methods
# - print text
# - reshape
# - summarize
# - print data
# - crosstab
# - print plot

#question <- function(){
#	structure(
#		list(
#				surveyor,
#				q_id,
#				q_text,
#				q_data
#				),
#				class="question")
#	}









