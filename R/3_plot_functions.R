# TODO: Fix stacked bar chart - make axes quiet, reduce size of legend
# TODO: Modify default behaviour to get rid of RColorBrewer warning messages

#' Blank axis titles and no legend
#' 
quiet <- opts(
		legend.position="none",
		axis.title.x = theme_blank(),
		axis.title.y = theme_blank()
)

#' Blank axis titles
#' 
quiet_axes <- opts(
		axis.title.x = theme_blank(),
		axis.title.y = theme_blank()
)


#' Define minimal theme
#' 
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
#' Coding functions:
#' \code{\link{code_single}}, 
#' \code{\link{code_array}},
#' \code{\link{code_text}}
#' Summarising functions:
#' \code{\link{stats_bin}}, 
#' \code{\link{stats_net_score}}
#' Plot functions: 
#' \code{\link{plot_bar}}, 
#' \code{\link{plot_point}} 
#' @export
plot_bar <- function(s, surveyor){
	f <- s$data
	if (is.null(f$question)){
		# Plot single question
		p <- ggplot(f, aes(x=response, y=value, fill=factor(crossbreak)))
	} else {
		if (is.null(f$response)) {
		# Plot array of single values per question
			p <- ggplot(f, aes(x=question, y=value, fill=factor(crossbreak)))
		} else {
			# Plot array question as stacked bar
			p <- ggplot(f, aes(x=question, y=value, fill=response))
		}
	}
	p <- p + 
			theme_grey(surveyor$defaults$default_theme_size) +
			geom_bar(stat="identity") + 
			coord_flip() + 
			scale_y_continuous(s$ylabel, formatter="percent") +
			facet_grid(~crossbreak) + 
			opts(
				legend.position="none",
				axis.title.y = theme_blank()
			)
	p
}

###############################################################################

#' Plot data in bubble chart format
#'
#' @param s A surveyor_stats object
#' @param surveyor Surveyor object
#' @seealso 
#' Coding functions:
#' \code{\link{code_single}}, 
#' \code{\link{code_array}},
#' \code{\link{code_text}}
#' Summarising functions:
#' \code{\link{stats_bin}}, 
#' \code{\link{stats_net_score}}
#' Plot functions: 
#' \code{\link{plot_bar}}, 
#' \code{\link{plot_point}} 
#' @export
plot_point <- function(s, surveyor){
	f <- s$data
	if (is.null(f$question)){
		# Plot single question
		p <- ggplot(f, aes(x=" ", y=response, size=value, 
								colour=factor(crossbreak), fill=factor(crossbreak)))
	} else {
	# Plot array question 
	p <- ggplot(f, aes(x=question, y=response, size=value, 
							colour=factor(crossbreak), fill=factor(crossbreak)))
		}
	p <- p + 
			theme_grey(surveyor$defaults$default_theme_size) +
			geom_point(stat="sum") + 
			coord_flip() + 
			quiet +
			ylab(s$ylabel) +
			facet_grid(~crossbreak) +
			opts(
					panel.grid.minor = theme_blank(), 
					axis.text.x = theme_text(size=surveyor$defaults$default_theme_size, angle=90, hjust=1)
			)
	
	p
}

###############################################################################


#' Plot data as text
#'
#' @param f A data frame with coded answers, provided by a code_* function
#' @param surveyor Surveyor object
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
#' @param f A data frame with coded answers, provided by a code_* function
#' @param surveyor Surveyor object
#' @seealso plot_bar, plot_bar_stacked, plot_net_score, plot_point, plot_text 
#' @export
plot_net_score <- function(f, surveyor){
	ggplot(f, aes(x=variable, y=value, fill=factor(crossbreak))) +
			theme_grey(surveyor$defaults$default_theme_size) +
			geom_bar(stat="identity", position="identity", width=0.8) +
			coord_flip() + quiet +
			scale_y_continuous(formatter="percent", breaks=c(-1, -0.5, 0, 0.5, 1)) +
			facet_grid(~crossbreak)
}

