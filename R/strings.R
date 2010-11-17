################################################################################
###  String functions                                                        ###
################################################################################

#substrn <- function(x, n){
#	###  Discards the first n characters in string x
#	substr(as.character(x), n, nchar(as.character(x)))
#}

#' Finds the common and unique elements in a character vector
#'
#' @param x Character vector
#' @examples
#' q_string <- c("Q_1", "Q_2", "Q_3") 
#' str_common_unique(q_string)$common
#' str_common_unique(q_string)$unique
str_common_unique <- function(x){
	# Function takes a character string as input and find the common and
	# unique elements.  Assumes that the common element is at start of string
	# Store a copy of x
	if (length(x) <= 1){
		## Handles case with a single string element
		list(common=x, unique="")
	} else {
		## Handles case with multiple string elements
		y <- x
		# Make all strings the same length as shortest string
		y <- substr(y, 1, min(nchar(y)))
		# Create matrix of characters
		y <- laply(y, function(x){unlist(strsplit(x, NULL))})
		# Test which characters are identical
		y <- aaply(y, 1, function(x){x==y[1,]})
		
		
		common <- min(aaply(y, 1, function(x){which(x==FALSE)[1]-1}), na.rm=TRUE)
		xcommon <- substr(x[1], 1, common)
		xunique <- substr(x, common+1, nchar(x))
		
		list(common=xcommon, unique=xunique)
	}
}

#' Wraps a string into separate lengths by inserting line breaks at word boundaries
#'
#' @param x Character vector
#' @param len Length of new strings
#' @examples
#' str_wrap("the quick brown fox jumps over the lazy dog", 10)  
	str_wrap <- function(x, len=30)
{
	str_wrapfunction <- function(x, len) (paste(strwrap(x,width=len),collapse="\n"))
	str <- laply(x, str_wrapfunction, len)
	as.character(str)
}


#' Returns a string in reverse order
#'
#' @param x Character vector
#' @examples
#' str_reverse("the quick brown fox jumps over the lazy dog")  
str_reverse <- function(x) {
	as.character(
			lapply(x, function(x){
				paste(rev(substring(x,1:nchar(x),1:nchar(x))),collapse="")
			})
	)
}

