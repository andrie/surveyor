################################################################################
###  Define Latex functions
################################################################################

#' Prints a string to Latex, dealing with special characters
#'
#' @param x A string containing question text
printQlatex <- function(x){
#  cat("\\section{", x, "}\n\n", sep="")
	inn <- "&"
	out <- "\\&"
	latexTranslate(
					paste("\n\\section{", x, "}\n\n", sep=""), 
					inn=inn, 
					out=out)
}

#' Prepares a string to be printed sideways (vertical) in LaTex
#'
#' @param x A character vector
latex_sideways <- function(x){
	paste(
			"\\begin{sideways}",
			x,
			"\\end{sideways}",
			sep=""
	)
}

#' Captures the print output of a Latex xtable
#'
#' @param x A table object
capture_table <- function(x){
	align <- paste(c("l", rep("r", dim(x)[2])), collapse="")
	x <- as.table(x)
	paste(capture.output(
					print(xtable(x,
									floating=false,
									table.placement="!h",
									align=align,
									digits=1),
							caption.placement="top",
							floating=FALSE,
							sanitize.colnames.function=function(str){latex_sideways(str)})
			), collapse="\n")		
}


