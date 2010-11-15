ggplot_bar <- function(f){
	ggplot(f, aes(x=variable, weight=weight, fill=factor(crossbreak))) +
			theme_grey(size_theme_default) +
			geom_bar(aes(group=1)) + coord_flip() + quiet +
			facet_grid(~crossbreak)
}

