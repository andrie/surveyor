

### Define a class of functions with a static variable (i)
### Important: This must be initialised before use
ggsaveprint <- function() {
	i <- 1
	f <- function(plot, size=c(4, 3), dpi=600, path) {
		if (output_to_latex && class(plot)=="ggplot") {
			filename <- paste("fig", i, ".eps", sep="")
			message(paste("Now saving ", filename, sep=""))
			ggsave(plot, filename=filename, width=size[1], height=size[2],
					dpi=dpi, path=path)
			cat("\\PlaceGraph{graphics/", filename, "}\n", sep="")
#          cat("\\FloatBarrier","\n",sep="")
			i <<- i + 1
		} else {
			print(plot)
		}
	}
	
	invisible( f )
}

### Must be initialised as follows:
# ggsprint <- ggsaveprint()

################################################################################
### Plugin architecture to process each question
################################################################################

plot_q <- function(Qnr,
		code_function,
		plot_function,
		plot_size=size_graph_default,
		dpi=600,
		plot_path=gpath,
		...){
	if (output_to_latex) printQLatex(getQtext(Qnr))
	message(Qnr)
	f <- code_q(Qnr, code_function, ...)
	g <- plot_function(f)
	if (identical(code_function, code_array)) plot_size[2] <- plot_size[2]*1.5
	ggsprint(plot=g, size=plot_size, dpi=dpi, path=plot_path)
}


code_q <- function(Qnr="Q1",
		code_function=code_single,
		data=kd,
		Qtext=Qs,
		multicode=FALSE,
		remove.other=FALSE,
		index="crossbreak",
		weight=TRUE,
		...){
	code_function(Qnr,
			data,
			Qtext,
			multicode=multicode,
			remove.other=remove.other,
			index=index,
			weight=weight,
			...)
}


ggprint <- function(x, output_to_latex=FALSE){
	if (output_to_latex){
		invisible(x)
	} else {
		print(x)
	}
}

ggwindows <- function(x, y, record){
#  windows(x, y, record)
	invisible(x)
}


