

plot_single_cb <- function(x, data=kd){
#  x <- data[, Qnr]
#  xt <- cb(x)
	x$x <- as.factor(x$x)
	if (nlevels(x$x) <= 7) {
		ggplot(x, aes(x=factor(1), fill=x, weight=weight)) + stacked_bar +
				theme_grey(size_theme_default) +
				quiet_axes + facet_cb + opts(axis.text.x=theme_blank()) + quiet_axes +
				scale_fill_brewer("", breaks=rev(levels(x$x)))
	} else {
		ggplot(x, aes(x=x, weight=weight)) + geom_bar() + coord_flip() +
				theme_grey(size_theme_default) +
				quiet_axes + facet_cb + opts(axis.text.x=theme_blank()) + quiet_axes
	}
}


qplot_single <- function(data, q="Q1", Qtext){
	# data is a data frame
	# Qnumber is the number of the question (text)
	# Qtext is the question text (list)
	x <- data[,q]
	qtitle <- as.character(Qtext[which(names(data)==q)])[1]
	qplot(x) + geom_bar() +
			scale_x_discrete(qtitle, breaks=brk_11pt, labels=lab_11pt_agree) +
			opts(axis.title.y = theme_blank())
}

