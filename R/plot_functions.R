#' Plot data in horizontal bar chart format
#'
#' @param f A data frame with coded answers, provided by a code_* function
#' @param surveyor Surveyor object
#' @seealso plot_bar, plot_bar_stacked, plot_net_score, plot_point, plot_text 
#' @export
plot_bar <- function(f, surveyor){
	ggplot(f, aes(x=variable, y=value, fill=factor(crossbreak))) +
			theme_grey(surveyor$defaults$default_theme_size) +
			geom_bar(stat="identity") + coord_flip() + quiet +
			facet_grid(~crossbreak) +
			scale_y_continuous(formatter="percent")
}

#plot_bar <- function(f, surveyor){
#	ggplot(f, aes(x=variable, weight=weight, fill=factor(crossbreak))) +
#			theme_grey(surveyor$defaults$default_theme_size) +
#			geom_bar(aes(group=1)) + coord_flip() + quiet +
#			facet_grid(~crossbreak)
#}



#' Plot data in horizontal stacked bar chart format
#'
#' @param f A data frame with coded answers, provided by a code_* function
#' @param surveyor Surveyor object
#' @seealso plot_bar, plot_bar_stacked, plot_net_score, plot_point, plot_text 
#' @export
plot_bar_stacked <- function(f, surveyor){
	ggplot(f, aes(x=1, y=value, fill=factor(variable))) +
			theme_grey(surveyor$defaults$default_theme_size) +
			geom_bar(stat="identity", position="stack") + coord_flip() + quiet +
			scale_y_continuous(formatter="percent") +
			facet_grid(~crossbreak)
}

#' Plot data in bubble chart format
#'
#' @param f A data frame with coded answers, provided by a code_* function
#' @param surveyor Surveyor object
#' @seealso plot_bar, plot_bar_stacked, plot_net_score, plot_point, plot_text 
#' @export
plot_point <- function(f, surveyor){
	ggplot(f, aes(x=variable, y=value, size=value, colour=factor(crossbreak), fill=factor(crossbreak))) +
			theme_grey(surveyor$defaults$default_theme_size) +
			geom_point(stat="sum") + coord_flip() + quiet +
			facet_grid(~crossbreak) +
			opts(
					panel.grid.minor = theme_blank()
			)
}
#plot_point <- function(f, surveyor){
#	ggplot(f, aes(x=variable, y=value, weight=weight, colour=factor(crossbreak), fill=factor(crossbreak))) +
#			theme_grey(surveyor$defaults$default_theme_size) +
#			geom_point(stat="sum") + coord_flip() + quiet +
#			facet_grid(~crossbreak) +
#			opts(
#					panel.grid.minor = theme_blank()
#			)
#}

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

