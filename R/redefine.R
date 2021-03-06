#-------------------------------------------------------------------------------
###  Redefine some standard R functions                                      ###
#-------------------------------------------------------------------------------


#' For a contingency table in array form, compute the sum of table entries for a given index. 
#' 
#' Redefines margin.table to deal with NA values
#' 
#' @param x an array
#' @param margin index number (1 for rows, etc.)
#' @param na.rm logical. Should missing values be removed? Passed to sum()
#' @keywords internal 
## #' @examples 
## #' m <- matrix(1:4,2)
## #' surveyor:::smargin.table(m, 1)
## #' surveyor:::smargin.table(m, 2)
smargin.table <- function (x, margin = NULL, na.rm=TRUE){
	if (!is.array(x))
		stop("'x' is not an array")
	if (length(margin)) {
		z <- apply(x, margin, sum, na.rm)
		dim(z) <- dim(x)[margin]
		dimnames(z) <- dimnames(x)[margin]
	}
	else return(sum(x), na.rm=TRUE)
	class(z) <- oldClass(x)
	z
}


#' Express Table Entries as Fraction of Marginal Table
#' 
#' Redefines prop.table to deal with NA values
#' 
#' @param x an array
#' @param margin index number (1 for rows, etc.)
#' @param na.rm logical. Should missing values be removed? Passed to sum()
#' @keywords internal 
## #' @examples 
## #' m <- matrix(1:4,2)
## #' surveyor:::sprop.table(m,1)
## #' surveyor:::sprop.table(m,2)
sprop.table <- function (x, margin = NULL, na.rm=TRUE){
	if (length(margin))
		sweep(x, margin, smargin.table(x, margin), "/", check.margin = FALSE)
	else x/sum(x, na.rm)
}


#' Paste matrix
#' 
#' Paste matrix
#' 
#' @param mtext Matrix
#' @param sep Separator text
#' @param collapse Collapse text
#' @export 
#' @author Jens Oehlschlagel-Akiyoshi
#' @
paste.matrix <- function(mtext, sep=" ", collapse=NULL){
	if (is.null(collapse))
		apply(mtext, 1, paste, collapse=sep)
	else
		paste(apply(mtext, 1, paste, collapse=sep), collapse=collapse)
}


