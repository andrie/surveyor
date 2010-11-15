# TODO: Combine initialisation functions into a single function
###############################################################################



output_to_latex <- FALSE
size_theme_default <- 12
set_colour_area()

colour_default_area  = rgb(127,201,127, 255, maxColorValue=255)
colour_default_point = rgb(27, 158, 119, 255, maxColorValue=255)

update_geom_defaults("bar", aes_string(fill="colour_default_area"))
update_geom_defaults("point", aes_string(fill="colour_default_point",
				colour="colour_default_point"))

ggsprint <- ggsaveprint()



