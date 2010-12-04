# TODO: Combine initialisation functions into a single function
###############################################################################


#' Initialises surveyor object defaults
#' 
#' @param path_latex Path where latex will be saved
#' @param path_graphics Path where graphics files will be saved
#' @param output_to_latex TRUE or FALSE, determines if latex commands is output
#' @param output_filename Filename where latex output will be saved
#' @param counter_start The starting number for a counter used to store graphs, defaults to 1
#' @param default_theme_size Text size in points, passed to ggplot
#' @param default_plot_size Plot size in inches, e.g. c(4, 3)
#' @param default_colour_area Default RGB colour for areas in graphs (e.g. bars)
#' @param default_colour_point Default RGB colour for points in graphs (e.g. points)
#' @param dpi Dots per inch, passed to ggsave()
#' @seealso surveyor
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
		default_theme_size = 12,
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

		require(ggplot2)
		update_geom_defaults("bar", aes_string(fill="default_colour_area"))
		update_geom_defaults("point", aes(fill   = default_colour_point))
	#	update_geom_defaults("point", aes(color = default_colour_point))
	
		#set_colour_area()
		set_default_scale("fill", "discrete", "brewer", palette="Set2")
		set_default_scale("colour", "discrete", "brewer", palette="Set2")
	
		#set_colour_point()
		#set_default_scale("fill", "discrete", "brewer", palette="Set1")
		#set_default_scale("color", "discrete", "brewer", palette="Set1")
	
list(
		path_latex           = path_latex,
		path_graphics   		 = path_graphics,
		output_to_latex      = output_to_latex,
		output_filename      = output_filename,
		default_theme_size   = default_theme_size,
		default_plot_size    = default_plot_size,
		default_colour_area  = default_colour_area,
		default_colour_point = default_colour_point,
		dpi                  = dpi,
		counter              = new_counter(counter_start)	
	)
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


#' Blank axis titles and no legend
#' 
quiet <- opts(legend.position="none",
		axis.title.x = theme_blank(),
		axis.title.y = theme_blank())

#' Blank axis titles
#' 
quiet_axes <- opts(axis.title.x = theme_blank(),
		axis.title.y = theme_blank())


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


## #' Horizontal bar chart
## #' 
#geom_flatbar <- c(geom_bar(stat="bin", binwidth=1, position="fill"), coord_flip())

## #' Stacked bar chart
## #' 
#stacked_bar <- c(geom_bar(stat="bin", binwidth=1, position="fill"),
#		scale_y_continuous(formatter="percent"))

## #' Defines facets in grid format
## #' 
#facet_cb <- ggplot2::facet_grid(facets=.~crossbreak)

## #' Defines facets in grid format
## #' 
#facet_cb_v <- ggplot2::facet_grid(facets=crossbreak~.)

