################################################################################
###  ggplot defaults and new geoms                                           ###
################################################################################

#colour_default = "blue"
set_colour_area <- function(){
	colour_default = rgb(127, 201, 127, 255, maxColorValue=255)
	set_default_scale("fill", "discrete", "brewer", palette="Set2")
	set_default_scale("colour", "discrete", "brewer", palette="Set2")
}

set_colour_point <- function(){
	colour_default = rgb(27, 158, 119, 255, maxColorValue=255)
	set_default_scale("fill", "discrete", "brewer", palette="Set1")
	set_default_scale("colour", "discrete", "brewer", palette="Set1")
}

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


geom_flatbar <- c(geom_bar(stat="bin", binwidth=1), coord_flip())

stacked_bar <- c(geom_bar(binwidth=1, position="fill"),
		scale_y_continuous(formatter="percent"))

facet_cb <- facet_grid(facets=.~crossbreak)
facet_cb_v <- facet_grid(facets=crossbreak~.)
