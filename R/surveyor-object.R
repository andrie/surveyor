################################################################################
### Define surveyor class and methods
################################################################################

#' Creates object of class surveyor.
#' 
#' A surveyor object describes the data and meta-data in the survey that will be analysed by the analysis and reporting functions in the surveyor package.
#' 
#' @param q_data data frame with survey data
#' @param q_text The question metadata that contains the original text question
#' @param crossbreak List of factors that contain crossbreak data.  
#' @param weight Numeric vector with weighting data
#' @param defaults Surveyor defaults.  See also \code{\link{surveyor_defaults}}
#' @return A list object of class surveyor
#' @export
#' @examples
#' q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
#' s <- as.surveyor(q_data, q_text, crossbreak=c("aa", "bb"), weight=c(1,1)) 					
as.surveyor <- function(
		q_data, 
		q_text, 
		crossbreak=q_data$crossbreak,
		weight=q_data$weight,
		defaults=surveyor_defaults()
	){
	if (!identical(names(q_data), names(q_text))){
		stop("Surveyor object: The names of q_data and q_text must match")
	}
	
	if (is.list(crossbreak)) {
		if (any(laply(crossbreak, length) != nrow(sd))) {
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
					defaults   = defaults
			), 
			class = "surveyor"
	)
}

###############################################################################


#' Initialises surveyor object defaults.
#' 
#' @param path_latex Path where latex will be saved
#' @param path_graphics Path where graphics files will be saved
#' @param output_to_latex TRUE or FALSE, determines if latex commands is output
#' @param output_filename Filename where latex output will be saved
#' @param counter_start The starting number for a counter used to store graphs, 
#' defaults to 1
#' @param default_theme_size Text size in points, passed to ggplot
#' @param question_pattern A text pattern passed to grep() to distinguish 
#' between single and array questions
#' @param subquestion_append Indicates whether subquestion text is appended to question text
#' @param subquestion_prepend Indicates whether subquestion text is prepended to question text
#' @param fastgraphics Uses lattice graphics if true, otherwise ggplot 
#' @param default_plot_size Plot size in inches, e.g. c(4, 3)
#' @param default_colour_area Default RGB colour for areas in graphs (e.g. bars)
#' @param default_colour_point Default RGB colour for points in graphs (e.g. points)
#' @param dpi Dots per inch, passed to ggsave()
#' @seealso \code{\link{as.surveyor}}
#' @export
#' @examples
#' s <- surveyor_defaults()
#' s <- surveyor_defaults(output_to_latex=TRUE) 					
surveyor_defaults <- function(
		path_latex = Sys.getenv()["TEMP"],
		path_graphics = path_latex,
		output_to_latex = FALSE,
		output_filename = file.path(path_latex, "surveyor.tex"),
		counter_start = 1,
		default_theme_size = 9,
		question_pattern = "_[[:digit:]]*$",
		subquestion_append = TRUE,
		subquestion_prepend = !subquestion_append,
		fastgraphics = FALSE,
		default_plot_size = c(5,3),
		default_colour_area = rgb(127,201,127, 255, maxColorValue=255),
		default_colour_point = rgb(27, 158, 119, 255, maxColorValue=255),
		dpi = 600
){
	
	### test that paths exist
	if (!file_test("-d", path_latex)){
		stop(paste("The latex file path doesn't exist:", path_latex))
	}
	if (!file_test("-d", path_graphics)){
		stop(paste("The graphics file path doesn't exist:", path_latex))
	}
	
	#update_geom_defaults("bar", aes_string(fill="default_colour_area"))
	#update_geom_defaults("point", aes(fill   = default_colour_point))
	#	update_geom_defaults("point", aes(color = default_colour_point))
	
	#set_colour_area()
	#set_default_scale("fill", "discrete", "brewer", palette="Set2")
	#set_default_scale("colour", "discrete", "brewer", palette="Set2")
	
	#set_colour_point()
	#set_default_scale("fill", "discrete", "brewer", palette="Set1")
	#set_default_scale("color", "discrete", "brewer", palette="Set1")
	
	list(
			path_latex           = path_latex,
			path_graphics   		 = path_graphics,
			output_to_latex      = output_to_latex,
			output_filename      = output_filename,
			default_theme_size   = default_theme_size,
			question_pattern     = question_pattern,
			subquestion_append   = subquestion_append,
			subquestion_prepend  = subquestion_prepend,
			fastgraphics         = fastgraphics,
			default_plot_size    = default_plot_size,
			default_colour_area  = default_colour_area,
			default_colour_point = default_colour_point,
			dpi                  = dpi,
			counter              = new_counter(counter_start)	
	)
}

################################################################################

#' Prints surveyor object.
#' 
#' @name print.surveyor
#' @aliases print print.surveyor
#' @usage print(x, ...)
#' @param x surveyor object
#' @param ... ignored
#' @S3method print surveyor
print.surveyor <- function(x, ...){
	cat("Surveyor\n\n")
	print.listof(x)
}

################################################################################

#' Tests that object is of class surveyor object.
#' 
#' @param x Object to be tested
#' @export
#' @examples 
#' q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
#' s <- as.surveyor(q_data, q_text, crossbreak=c("aa", "bb"), weight=c(1,1))
#' is.surveyor(s) # TRUE
#' is.surveyor("String") #FALSE 					
is.surveyor <- function(x){
	if (class(x)=="surveyor"){
		if (all(
				!is.null(x$q_data),
				!is.null(x$q_text),
				!is.null(x$cbreak),
				!is.null(x$weight),
				!is.null(x$defaults)
		)){
			TRUE
		} else {
			FALSE
		}
			
	} else {
		FALSE
	}
}



################################################################################
### Define a class of functions with a static variable (i)
### Important: This must be initialised before use
################################################################################


#' Creates a counter 
#' 
#' Must be initialiased, e.g. surveyor_counter <- new_counter()
#' 
#' @keywords internal
#' @param start The starting value of the counter, defaults to 1
new_counter <- function(start=1) {
	i <- start - 1
#	i <- i - 1
	function() {
		# do something useful, then ...
		i <<- i + 1
		i
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
#' @param ... Other parameters passed to code_function
#' @export
#' @seealso \code{\link{as.surveyor}}
surveyor_plot <- function(
		surveyor,
		q_id,
		code_function = code_guess,
		stats_function = stats_guess,
		plot_function = plot_bar,
		plot_size = surveyor$defaults$default_plot_size,
		...){
	
	###  plot_q internal functions
	
	plot_q_internal <- function(){
		
		counter <- eval(surveyor$defaults$counter(), envir=parent.frame(n=2))
		
		if(!(is.surveyor(surveyor))){
			stop("You must pass a valid surveyor object to plot_q")
		}
		
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
		
		if (!surveyor$defaults$output_to_latex){
			if (nothing_to_plot){
				message("Nothing to plot")
			} else {
				print(h)
			}
		} else {
			cat_string <- surveyor_print_question(
					surveyor,
					q_id,
					counter,
					f,
					g,
					h, 
					plot_size)
			surveyor_write(surveyor, cat_string)
		}
		
	}
	
	###  plot_q main function
	
	message(q_id)
	if (surveyor$defaults$output_to_latex){
		surveyor_heading(
				surveyor, 
				paste(q_id, get_q_text(surveyor, q_id)), 
				headinglevel= "section",
				pagebreak=FALSE)
	}
		
	if (is.list(surveyor$crossbreak)) {
		for (i in 1:length(surveyor$crossbreak)) {
			surveyor$cbreak <- unlist(surveyor$crossbreak[i])
			plot_q_internal()
		}		
	} else {
		plot_q_internal()
					
	}

}

#' Creates latex heading.
#' 
#' Creates latex heading
#' 
#' @param surveyor A surveyor object
#' @param x A character vector
#' @param level Character vector corresponding to latex heading, e.g. Chapter, Section, etc.
#' @param pagebreak Forces a page break if TRUE
#' @keywords internal
surveyor_heading <- function(surveyor, x, headinglevel="chapter", pagebreak=FALSE){
	text <- paste("\n\\", headinglevel, "{", latexTranslate(x), "}", sep="")
	if(pagebreak) text <- paste(text, "\n\\pagebreak[4]\n")
	surveyor_write(surveyor, text)
}

###############################################################################

#' Writes result to surveyor sink file.
#' 
#' Appends text to surveyor sink file.
#' 
#' @param surveyor A surveyor object
#' @param x A character vector
#' @keywords internal
surveyor_write <- function(surveyor, x){
	if(surveyor$defaults$output_to_latex){
		cat(x,
				file=surveyor$defaults$output_filename,
				sep="",
				append=TRUE)
	} else {
		message(x)
	}
	return(invisible)
}

###############################################################################

#' Saves surveyor plot to pdf.
#' 
#' Saves surveyor plot to pdf.  Uses either ggsave() or pdf(), depending on the class of plot
#' 
#' @param surveyor A surveyor object
#' @param h A surveyor plot object
#' @param width Width in inches
#' @param heigh Heigh in inches
#' @param filename Filename without path
#' @keywords internal
surveyor_save_plot <- function(surveyor, h, width, height, filename){
	if(class(h)=="ggplot"){
		ggsave(
				h, 
				filename = filename, 
				width    = width, 
				height   = height,
				dpi      = surveyor$defaults$dpi, 
				path     = surveyor$defaults$path_graphics
		)
	}	
	if(class(h)=="trellis"){
		pdf(
				file=file.path(surveyor$defaults$path_graphics, filename),
				width    = width, 
				height   = height
		)
		print(h)
		dev.off()
	}	
}
################################################################################

#' Prints surveyor question. 
#' 
#' @param surveyor A surveyor object
#' @param q_id The question id
#' @param counter The file number
#' @param f Results from code_* function
#' @param g Results from stats_* function
#' @param h Results from plot_* function
#' @param plot_size the plot size in inches
#' @keywords internal
surveyor_print_question <- function(surveyor, q_id, counter, f, g, h, plot_size){
	# Print question description
#	surveyor_write(surveyor, qtext) 
					
	if (is.null(f)){
		cat_string <- "\nNo data\n\n"
	} else {
		# Print plot
		filename <- paste("fig", sprintf("%04d", counter), ".pdf", sep="")
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
		surveyor_save_plot(
				surveyor, 
				h, 
				width=plot_size[1], 
				height=plot_size[2] * height_multiplier,
				filename=filename
		)
#		ggsave(
#				h, 
#				filename = filename, 
#				width    = plot_size[1], 
#				height   = plot_size[2] * height_multiplier,
#				dpi      = surveyor$defaults$dpi, 
#				path     = surveyor$defaults$path_graphics
#		)

		cat_string <- paste(
#				"\n\\begin{samepage}\n",
				paste("  \\PlaceGraph{graphics/", filename, "}\n", sep=""),
#				print_cb_stats(g),
#				"\\end{samepage}\n",
#				"\\smallskip\n",
#				"\\\\",
				sep=""
		)
		
#		if(height_multiplier * plot_size[2] >= 20){
#			cat_string <- paste(cat_string, "\n\\newpage", sep="")
#		}
						
	}
	
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
theme_surveyor <- function (base_size = 12, base_family = "") 
{
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
# TODO: Define question class
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



# TODO: Once pattern is passed to question handling, write a function to guess question ids 






