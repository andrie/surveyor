# TODO: Combine initialisation functions into a single function
###############################################################################


#' Initialises surveyor object defaults
#' 
#' @param output_to_latex TRUE or FALSE, determines if latex commands is output
#' @param default_theme_size Text size in points, passed to ggplot
#' @param default_plot_size Plot size in inches, e.g. c(4, 3)
#' @param default_colour_area Default RGB colour for areas in graphs (e.g. bars)
#' @param default_colour_point Default RGB colour for points in graphs (e.g. points)
#' @seealso surveyor
#' @export
#' @examples
#' surveyor_defaults()
#' surveyor_defaults(output_to_latex=TRUE) 					
surveyor_defaults <- function(
		output_to_latex = FALSE,
		default_theme_size = 12,
		default_plot_size = c(5,3),
		default_colour_area = rgb(127,201,127, 255, maxColorValue=255),
		default_colour_point = rgb(27, 158, 119, 255, maxColorValue=255)
	){

	update_geom_defaults("bar", aes_string(fill="default_colour_area"))
	update_geom_defaults("point", aes_string(fill="default_colour_point",
				colour="default_colour_point"))
	
	#set_colour_area()
	set_default_scale("fill", "discrete", "brewer", palette="Set2")
	set_default_scale("colour", "discrete", "brewer", palette="Set2")
	
	#set_colour_point()
	set_default_scale("fill", "discrete", "brewer", palette="Set1")
	set_default_scale("colour", "discrete", "brewer", palette="Set1")
	
list(
		output_to_latex,
		default_theme_size,
		default_plot_size,
		default_colour_area,
		default_colour_point
	)
}
	


#' Initialises surveyor object defaults
#' 
#' @export
#' @examples
#' surveyor_initialise()
surveyor_initialise <- function(){
	save_print <<- save_print_function()
	invisible
}

################################################################################
###  ggplot defaults and new geoms                                           ###
################################################################################

#set_colour_area <- function(){
#	colour_default = rgb(127, 201, 127, 255, maxColorValue=255)
#	set_default_scale("fill", "discrete", "brewer", palette="Set2")
#	set_default_scale("colour", "discrete", "brewer", palette="Set2")
#}
#
#set_colour_point <- function(){
#	colour_default = rgb(27, 158, 119, 255, maxColorValue=255)
#	set_default_scale("fill", "discrete", "brewer", palette="Set1")
#	set_default_scale("colour", "discrete", "brewer", palette="Set1")
#}

quiet <- opts(legend.position="none",
		axis.title.x = theme_blank(),
		axis.title.y = theme_blank())

quiet_axes <- opts(axis.title.x = theme_blank(),
		axis.title.y = theme_blank())


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


geom_flatbar <- c(stat_bin(binwidth=1), coord_flip())

stacked_bar <- c(stat_bin(binwidth=1, position="fill"),
		scale_y_continuous(formatter="percent"))

facet_cb <- facet_grid(facets=.~crossbreak)
facet_cb_v <- facet_grid(facets=crossbreak~.)

