# TODO: Add comment
# 
# Author: Andrie
###############################################################################


#' Tests whether levels contain "Don't know" 
#' 
#' @param x Vector or Factor 
#' @param dk Character vector, containing search terms, e.g. c("Don't know", "Don't Know")
#' @return TRUE or FALSE
#' @seealso
#' \code{\link{remove_dk}}, 
#' \code{\link{remove_all_dk}}, 
has_dk <- function(x, dk="Don't Know"){
	l <- levels(x)
	any(l %in% dk)
}

#' Removes "Don't know" from levels 
#' 
#' @param x Vector or Factor 
#' @param dk Character vector, containing search terms, e.g. c("Don't know", "Don't Know")
#' @return A factor with "Dont know" removed
#' @seealso
#' \code{\link{has_dk}}, 
#' \code{\link{remove_all_dk}}, 
remove_dk <- function(x, dk="Don't Know"){
	if (has_dk(x, dk)){
		l <- levels(x)
		l[which(levels(x) %in% dk)] <- NA
		x <- factor(x, levels=l)		
	} else {
		x
	}
}

#' Removes "Don't know" from factor levels in data frame 
#' 
#' @param x Vector or Factor 
#' @param dk Character vector, containing search terms, e.g. c("Don't know", "Don't Know")
#' @return A data frame
#' @export
remove_all_dk <- function(x, dk=c("I don't know", "Don't Know", "Don't know","Dont know" , "DK", "NA")){
	newx <- llply(x, remove_dk, dk)
	n1 <- sum(as.numeric(llply(x, has_dk, dk)))
	n2 <- sum(as.numeric(llply(newx, has_dk, dk)))
	dk <- paste(dk, collapse=", ")
	message(paste("Removed", n1-n2,"instances of levels that equal [", dk, "]"))
	data.frame(newx)
}	

