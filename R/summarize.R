###############################################################################
# Step 2 in the triad of code, summarize and plot
###############################################################################

#' Tests for all NA values 
#' 
#' @param x A list, data frame or vector 
#' @return TRUE if all values are NA, FALSE otherwise
all_na <- function(x){
	if (is.list(x) || is.data.frame(x)){
		return(all(as.logical(llply(x, function(y) all(is.na(y))))))
	}
	return(all(is.na(x)))
}

#' Tests for all NULL values 
#' 
#' @param x A list, data frame or vector 
#' @return TRUE if all values are NULL, FALSE otherwise
all_null <- function(x){
	if (is.list(x) || is.data.frame(x)){
		return(all(as.logical(llply(x, function(y) all(is.null(y))))))
	}
	return(all(is.null(x)))
}


#' Calculates summary statistics
#' 
#' Takes the result of a code_function, e.g. code_single(), and calculates
#' summary values, for direct plotting by a plot_function, e.g. plot_bar()
#' 
#' @param f A data frame with four columns: variable, value, crossbreak, weight 
#' @return A data frame with three columns: crossbreak, variable, value
#' @export
stats_bin <- function(f){
	ft <- f
	crossbreakweight <- ddply(ft, .(crossbreak), summarise, weight=sum(weight))
	row.names(crossbreakweight) <- crossbreakweight$crossbreak
	ft$weight <- ft$weight / crossbreakweight[ft$crossbreak, ]$weight
	
	ddply(ft, .(crossbreak, variable), 
			summarise, 
			value=sum(weight)
	)
	
}

#################################################################################
####  net score calculation and graphing                                      ###
#################################################################################

#' Calculates a net score
#'
#' This assumes that x is an ordered factor, from low to high
#' It then calculates a net percentage score
#' 
#' @param x An ordered factor
net_score <- function(x){
	
	x <- x[!is.na(x)]
	
	xu <- unclass(x)
	m <- ceiling(length(levels(x))/2)
	below <- length(xu[xu < m])
	above <- length(xu[xu > m])
	net <- above - below
	netScore <- net / length(x)
	netScore
}

#' Code survey data as net score
#'
#' Code survey data in net score form
#' 
#' @param f Output from a code_function()
#' @return data frame
#' @seealso \code{\link{code_single}}, \code{\link{code_rank}}
#' @export
stats_net_score <- function(f){
	
#	segment_net_score <- function(x){
#		# make data.frame with subset net scores
#		# first drop crossbreak column
#		x <- x[, which(names(x)!="crossbreak")]
#		x <- ldply(x, net_score)
#		
#		x$variable <- r[as.character(x$.id)]
#		x <- rename(x, c(V1="value"))
#		
#		x$variable <- wordwrap(x$variable, 50)
#		x$weight <- rep(1, nrow(x))
#		x
#	}
	
	ddply(x, .(crossbreak, variable), value=net_score(value))
}

