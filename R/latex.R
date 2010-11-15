################################################################################
###  Define Latex functions
################################################################################

printQLatex <- function(x){
#  cat("\\section{", x, "}\n\n", sep="")
	cat(latexTranslate(paste("\\section{", x, "}\n\n", sep=""), inn="&", out="\\&"))
}

