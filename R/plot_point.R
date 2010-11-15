ggplot_point <- function(f){
	ggplot(f, aes(x=variable, y=value, weight=weight, fill=factor(crossbreak))) +
			theme_grey(size_theme_default) +
			geom_point(aes(group=1), stat="sum") + coord_flip() + quiet +
			facet_grid(~crossbreak) +
#  scale_y_continuous(breaks=unique(f$value)) +
			opts(
					panel.grid.minor = theme_blank()
			)
}

