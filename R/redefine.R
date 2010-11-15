################################################################################
###  Redefine some standard R functions                                      ###
################################################################################


margin.table <- function (x, margin = NULL)
# Redefines margin.table to deal with na.rm
{
	if (!is.array(x))
		stop("'x' is not an array")
	if (length(margin)) {
		z <- apply(x, margin, sum, na.rm=TRUE)
		dim(z) <- dim(x)[margin]
		dimnames(z) <- dimnames(x)[margin]
	}
	else return(sum(x), na.rm=TRUE)
	class(z) <- oldClass(x)
	z
}


prop.table <- function (x, margin = NULL, na.rm=false)
# Redefines prop.table to deal with na.rm
{
	if (length(margin))
		sweep(x, margin, margin.table(x, margin), "/", check.margin = FALSE)
	else x/sum(x, na.rm)
}


# Version with apply()
paste.matrix <- function(mtext, sep=" ", collapse=NULL){
	if (is.null(collapse))
		apply(mtext, 1, paste, collapse=sep)
	else
		paste(apply(mtext, 1, paste, collapse=sep), collapse=collapse)
}
