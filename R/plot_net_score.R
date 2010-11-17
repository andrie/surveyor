plot_netScore <- function(f){
	ggplot(f, aes(x=variable, y=value, fill=factor(crossbreak))) +
			theme_grey(size_theme_default) +
			ggplot2::geom_bar(stat="identity", position="identity", width=0.8) +
			coord_flip() + quiet +
			scale_y_continuous(formatter="percent", breaks=c(-1, -0.5, 0, 0.5, 1)) +
			facet_grid(~crossbreak)
}

