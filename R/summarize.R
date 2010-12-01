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
#' @param x A data frame with four columns: crossbreak, question, response, weight 
#' @return A data frame with three columns: crossbreak, variable, value
#' @export
stats_bin <- function(x){
	if(is.null(x)){
		return(NULL)
	}
	crossbreakweight <- ddply(x, .(crossbreak), summarise, weight=sum(weight))
	row.names(crossbreakweight) <- crossbreakweight$crossbreak
	x$weight <- x$weight / crossbreakweight[x$crossbreak, ]$weight

	
	if (length(unique(x$question))==1){
		# code single
		ddply(x, .(crossbreak, response), 
				summarise, 
				value=sum(weight)
		)
	} else {
		# code array
		ddply(x, .(crossbreak, question, response), 
				summarise, 
				value=sum(weight)
		)
	}
}

#' Calculates summary statistics for ranking type questions
#' 
#' Takes the result of a code_function, e.g. code_single(), and calculates
#' summary values, for direct plotting by a plot_function, e.g. plot_bar()
#' 
#' @param x A data frame with four columns: crossbreak, question, response, weight 
#' @return A data frame with three columns: crossbreak, variable, value
#' @export
stats_rank <- function(x){
	if(is.null(x)){
		return(NULL)
	}
	crossbreakweight <- ddply(x, .(crossbreak), summarise, weight=sum(weight))
	row.names(crossbreakweight) <- crossbreakweight$crossbreak
	x$weight <- x$weight / crossbreakweight[x$crossbreak, ]$weight
	
	
	if (length(unique(x$question))==1){
		# code single
		ddply(x, .(crossbreak, response), 
				summarise, 
				value=sum(weight)
		)
	} else {
		# code array
		ddply(x, .(crossbreak, question, response), 
				summarise, 
				value=sum(weight)
		)
	}
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
#' @param x A data frame with four columns: crossbreak, question, response, weight 
#' @return data frame
#' @seealso \code{\link{stats_bin}}
#' @export
stats_net_score <- function(x){
	if (length(unique(x$question))==1){
		# code single
		ddply(x, .(crossbreak, response), 
				summarise,
				value=net_score(response))
	} else {
		# code array
#		ddply(x, .(crossbreak, question, response), 
		ddply(x, .(crossbreak, question), 
				summarise,
				value=net_score(response))
	}
}

