# TODO: Fix stacked bar chart - make axes quiet, reduce size of legend
# TODO: Modify default behaviour to get rid of RColorBrewer warning messages

#' Blank axis titles and no legend
#' 
#' @keywords internal
quiet <- opts(
		legend.position="none",
		axis.title.x = theme_blank(),
		axis.title.y = theme_blank()
)

#' Blank axis titles
#' 
#' @keywords internal
quiet_axes <- opts(
		axis.title.x = theme_blank(),
		axis.title.y = theme_blank()
)


#' Define minimal theme
#' 
#' @keywords internal
theme_minimal <- opts(
		axis.title.x = theme_blank(),
		axis.title.y = theme_blank(),
		axis.text.x  = theme_blank(),
		axis.text.y  = theme_blank(),
		axis.ticks   = theme_blank(),
		axis.ticks.margin = unit(rep(0,4), "lines"),
		axis.ticks.length = unit(0, "cm"),
		panel.border = theme_blank(),
		panel.ticks  = theme_blank(),
		panel.grid.major = theme_blank(),
		panel.grid.minor = theme_blank(),
		plot.margin = unit(rep(0,4), "lines"),
		panel.margin = unit(rep(0,4), "lines"),
		legend.position  = "none"
)





#' Plot data in bar chart format
#'
#' @param s A surveyor_stats object
#' @param surveyor Surveyor object
#' @seealso 
#' Plot functions: 
#' \itemize{
#' \item \code{\link{plot_bar}} 
#' \item \code{\link{plot_point}} 
#' \item \code{\link{plot_text}} 
#' \item \code{\link{plot_net_score}} 
#' }
#' 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords plot
#' @export
plot_bar <- function(s, surveyor){
	f <- s$data

	# Test for yes/no responses
	if(is.factor(f$response) & all(levels(f$response) %in% c("Yes", "No"))){
		f <- f[f$response == levels(f$response)[which(levels(f$response) == "Yes")], ]
	}
		
	if (is.null(f$question)){
		# Plot single question
		p <- ggplot(f, aes_string(x="response", y="value", fill="factor(cbreak)"))
	} else {
		if (is.null(f$response)) {
		# Plot array of single values per question
			p <- ggplot(f, aes_string(x="question", y="value", fill="factor(cbreak)"))
		} else {
			# Plot array question as stacked bar
			p <- ggplot(f, aes_string(x="question", y="value", fill="factor(response)"))
		}
	}
	
	p <- p + 
			theme_surveyor(surveyor$defaults$default_theme_size) +
			geom_bar(stat="identity") + 
			coord_flip() + 
			scale_y_continuous(
					s$ylabel, 
					formatter=s$formatter) +
			facet_grid(~cbreak) + 
			opts(
				legend.position="none",
				axis.title.y = theme_blank()
			) +
			labs(fill="Response")
	
	
	if (is.null(f$question)){
		# Plot single question
		if(length(unique(f$response)) > 8){p <- p + scale_fill_hue()}
	} else {
			# Plot array of single values per question
			# Plot array question as stacked bar
			if(length(unique(f$question)) > 8){p <- p + scale_fill_hue()}
			p <- p + opts(legend.position="right")
		}	
	p
}

#' Plot data in bar chart format without modifying format.
#' 
#' The standard plot_bar() function will plot the data in a stacked bar chart format and apply percentage formatting.  plot_bar_sum() applies no formatting.
#'
#' @param s A surveyor_stats object
#' @param surveyor Surveyor object
#' @seealso 
#' Plot functions: 
#' \itemize{
#' \item \code{\link{plot_bar}} 
#' \item \code{\link{plot_point}} 
#' \item \code{\link{plot_text}} 
#' \item \code{\link{plot_net_score}} 
#' }
#' 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords plot
#' @export
plot_bar_sum <- function(s, surveyor){
	plot_bar(s, surveyor, formatter=format)
}

###############################################################################


#' Plot data in bar chart format, where the data consists of yes/no options.
#' 
#' This will produce a bar chart, but only "Yes" values will be plotted, i.e. the "No" values will be invisible.
#'
#' @param s A surveyor_stats object
#' @param surveyor Surveyor object
#' @seealso 
#' Plot functions: 
#' \itemize{
#' \item \code{\link{plot_bar}} 
#' \item \code{\link{plot_point}} 
#' \item \code{\link{plot_text}} 
#' \item \code{\link{plot_net_score}} 
#' }
#' 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords plot
#' @export
plot_yesno <- function(s, surveyor){
	f <- s$data
	level_yes <- levels(f$response)[2]
	f <- f[f$response==level_yes, ] ### Filters only the second level
	if (is.null(f$question)){
		# Plot single question
		p <- ggplot(f, aes_string(x="response", y="value", fill="factor(cbreak)"))
	} else {
		if (is.null(f$response)) {
			# Plot array of single values per question
			p <- ggplot(f, aes_string(x="question", y="value", fill="factor(cbreak)"))
		} else {
			# Plot array question as stacked bar
			p <- ggplot(f, aes_string(x="question", y="value", fill="factor(response)"))
		}
	}
	p <- p + 
			theme_surveyor(surveyor$defaults$default_theme_size) +
			geom_bar(stat="identity") + 
			coord_flip() + 
			scale_y_continuous(
					s$ylabel, 
					formatter=s$formatter) +
			facet_grid(~cbreak) + 
			opts(
					legend.position="none",
					axis.title.y = theme_blank()
			)
	
	if (is.null(f$question)){
		# Plot single question
		if(length(unique(f$response)) > 8){p <- p + scale_fill_hue()}
	} else {
		# Plot array of single values per question
		# Plot array question as stacked bar
		if(length(unique(f$question)) > 8){p <- p + scale_fill_hue()}
	}	
	p
}


###############################################################################

#' Plot data in bubble chart format
#'
#' @param s A surveyor_stats object
#' @param surveyor Surveyor object
#' @seealso 
#' Plot functions: 
#' \itemize{
#' \item \code{\link{plot_bar}} 
#' \item \code{\link{plot_point}} 
#' \item \code{\link{plot_text}} 
#' \item \code{\link{plot_net_score}} 
#' }
#' 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords plot
#' @export
plot_point <- function(s, surveyor){
	question <- NULL; rm(question) # Dummy to trick R CMD check
	response <- NULL; rm(response) # Dummy to trick R CMD check
	value <- NULL; rm(value) # Dummy to trick R CMD check
	cbreak <- NULL; rm(cbreak) # Dummy to trick R CMD check

	f <- s$data
	if (is.null(f$question)){
		# Plot single question
		p <- ggplot(f, aes(x=" ", y=response, size=value, 
								colour=factor(cbreak), fill=factor(cbreak)))
	} else {
	# Plot array question 
	p <- ggplot(f, aes(x=question, y=response, size=value, 
							colour=factor(cbreak), fill=factor(cbreak)))
		}
	p <- p + 
			theme_surveyor(surveyor$defaults$default_theme_size) +
			geom_point(stat="sum") +
			geom_text(aes(label=round(value*100, 0)),
					size=3, vjust=2) +
			coord_flip() + 
			quiet +
			ylab(s$ylabel) +
			facet_grid(~cbreak) +
			opts(
					panel.grid.minor = theme_blank(), 
					axis.text.x = theme_text(
							size=surveyor$defaults$default_theme_size, 
							angle=90, 
							hjust=1)
			)
	
	p
}

###############################################################################


#' Plot data as text
#'
#' @param f A data frame with coded answers, provided by a code_* function
#' @param surveyor Surveyor object
#' @seealso
#' Plot functions: 
#' \itemize{
#' \item \code{\link{plot_bar}} 
#' \item \code{\link{plot_point}} 
#' \item \code{\link{plot_text}} 
#' \item \code{\link{plot_net_score}} 
#' }
#' 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords plot
#' @export
plot_text <- function(f, surveyor){
		paste(
			"\\begin{itemize}",
			f,
			"\n\\end{itemize}",
			sep="\n"
	)
}

###############################################################################


# TODO: Fix plot_net_score to deal with weighting
#' Plot data in net score format (bar chart, but percentage axis)
#'
#' @param s A data frame with coded answers, provided by a code_* function
#' @param surveyor Surveyor object
#' @export
#' @seealso
#' Plot functions: 
#' \itemize{
#' \item \code{\link{plot_bar}} 
#' \item \code{\link{plot_point}} 
#' \item \code{\link{plot_text}} 
#' \item \code{\link{plot_net_score}} 
#' }
#' 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords plot
plot_net_score <- function(s, surveyor){
	f <- s$data
	f$hjust <- 0
	if (max(f$value) >= 0) f[f$value >= 0,   ]$hjust <- -0.1  
	if (max(f$value) >= 0.5) f[f$value >= 0.5, ]$hjust <- 1.1  
	if (min(f$value) <= 0) f[f$value < 0,    ]$hjust <- 1.1  
	if (min(f$value) <= -0.5) f[f$value < -0.5, ]$hjust <- -0.1  
	
	
	p <- ggplot(f, aes_string(x="question", y="value")) +
			theme_surveyor(surveyor$defaults$default_theme_size) +
			geom_bar(
					aes_string(fill="factor(cbreak)"),
					stat="identity", 
					position="identity", 
					width=0.8) +
			geom_text(
					aes_string(label="round(value*100, 0)",
							hjust="hjust"),
					size=3) +
			coord_flip(ylim=c(-1,1)) +
			opts(
					legend.position="none",
					axis.title.y = theme_blank()
			) +
			scale_y_continuous(
					s$ylabel, 
					formatter = "percent", 
					breaks    = c(-1, 0, 1)
			) +
			facet_grid(~cbreak)  
			
	if (length(unique(f$cbreak)) > 1){
		p <- p + opts(axis.text.x = theme_blank())
	}
			
	p
}

