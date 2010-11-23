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

