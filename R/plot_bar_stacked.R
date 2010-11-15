ggplot_bar_stacked <- function(f){
	ggplot(f, aes(x=variable, weight=weight, fill=factor(value))) +
			theme_grey(size_theme_default) +
			geom_bar() + coord_flip() + quiet +
			scale_y_continuous(formatter="percent") +
			facet_grid(~crossbreak)
}

