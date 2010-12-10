###############################################################################
# Step 2 in the triad of code, summarize and plot
###############################################################################

#' Creates surveyor_stats object, used as input to plot 
#' 
#' @param data A data frame 
#' @param ylabel Character string to print as plot y label
#' @return A surveyor_stats object
surveyor_stats <- function(
		data,
		ylabel = "Fraction of respondents"){
	ss <- list(
			data=data, 
			ylabel=ylabel)
	class(ss) <- "surveyor_stats"
	ss
}

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
	cbweight <- ddply(x, .(crossbreak, question), summarise, weight=sum(weight))
	row.names(cbweight) <- paste(cbweight$crossbreak, cbweight$question, sep="_")
	x$weight <- x$weight / cbweight[paste(x$crossbreak, x$question, sep="_"), ]$weight
	
	
	if (length(unique(x$question))==1){
		# code single
		df <- ddply(x, .(crossbreak, response), 
				summarise, 
				value=sum(weight)
		)
	} else {
		# code array
		df <- ddply(x, .(crossbreak, question, response), 
				summarise, 
				value=sum(weight)
		)
	}
	surveyor_stats(
			df,
			ylabel="Fraction of respondents")
}

# TODO: Fix stats_rank

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
		df <- ddply(x, .(crossbreak, response), 
				summarise, 
				value=sum(weight)
		)
	} else {
		# code array
		df <- ddply(x, .(crossbreak, question, response), 
				summarise, 
				value=sum(weight)
		)
	}
	surveyor_stats(
			df,
			ylabel="Rank (1 is high)")
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

# TODO: Fix stats_net_score to deal with 

#' Code survey data as net score
#'
#' Code survey data in net score form
#' 
#' @param x A data frame with four columns: crossbreak, question, response, weight 
#' @return data frame
#' @seealso \code{\link{stats_bin}}
#' @export
stats_net_score <- function(x){
	if (length(unique(x$question))==0){
		# code single
		df <- ddply(x, .(crossbreak, response), 
				summarise,
				value=net_score(response))
	} else {
		# code array
#		ddply(x, .(crossbreak, question, response), 
		df <- ddply(x, .(crossbreak, question), 
				summarise,
				value=net_score(response))
	}
	surveyor_stats(
			df,
			ylabel="Net score")
}

