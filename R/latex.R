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
	sanitize <- function(str) {
		result <- str
		result <- gsub("\\\\", "SANITIZE.BACKSLASH", result)
		result <- gsub("$", "\\$", result, fixed = TRUE)
		result <- gsub(">", "$>$", result, fixed = TRUE)
		result <- gsub("<", "$<$", result, fixed = TRUE)
		result <- gsub("|", "$|$", result, fixed = TRUE)
		result <- gsub("{", "\\{", result, fixed = TRUE)
		result <- gsub("}", "\\}", result, fixed = TRUE)
		result <- gsub("%", "\\%", result, fixed = TRUE)
		result <- gsub("&", "\\&", result, fixed = TRUE)
		result <- gsub("_", "\\_", result, fixed = TRUE)
		result <- gsub("#", "\\#", result, fixed = TRUE)
		result <- gsub("^", "\\verb|^|", result, fixed = TRUE)
		result <- gsub("~", "\\~{}", result, fixed = TRUE)
		result <- gsub("SANITIZE.BACKSLASH", "$\\backslash$", 
				result, fixed = TRUE)
		return(result)
	}
	x <- sanitize(x)
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
#' @param caption Caption for table
latex_table <- function(x, caption){
#	align <- paste(c("l", rep("r", dim(x)[2])), collapse="")
	align <- paste(c("p{5cm}", rep("r", dim(x)[2])), collapse="")
	x <- as.table(x)
	paste(capture.output(
					print(xtable(
									x,
									caption=caption,
									align=align,
									digits=1,
									table.placement="!h"),
							floating=FALSE,
							caption.placement="bottom",
							sanitize.colnames.function=function(str){latex_sideways(str)})
			), collapse="\n")		
}


