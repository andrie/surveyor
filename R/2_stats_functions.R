###############################################################################
# Step 2 in the triad of code, summarize and plot
###############################################################################

#' Creates surveyor_stats object, used as input to plot 
#' 
#' @param data A data frame 
#' @param ylabel Character string to print as plot y label
#' @return A surveyor_stats object
#' @keywords internal
surveyor_stats <- function(
		data,
		ylabel = "Fraction of respondents",
		formatter="percent",
		nquestion=length(unique(data$question))
){
	ss <- list(
			data=data, 
			ylabel=ylabel,
			formatter=formatter,
			nquestion=nquestion)
	class(ss) <- "surveyor_stats"
	ss
}

#' Sorts df in descending order 
#' 
#' @param df A data frame containing at least two columns: response and value 
#' @return A data frame
#' @keywords internal
reorder_response <- function(df){
	resp_levels  <- df[order(df$value, decreasing=TRUE), ]$response
	df$response <- factor(df$response, levels=resp_levels, ordered=TRUE)
	df
}



#' Tests for all NA values 
#' 
#' @param x A list, data frame or vector 
#' @return TRUE if all values are NA, FALSE otherwise
#' @keywords internal
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
#' @keywords internal
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
#' The results are sorted in descending order of value, and "response" is
#' coerced into an ordered factor (unless "response" is already an ordered 
#' factor).
#' 
#' @param x A data frame with four columns: cbreak, question, response, weight 
#' @return A data frame with three columns: cbreak, variable, value
#' @seealso
#' Stats functions:
#' \itemize{
#' \item \code{\link{stats_bin}} 
#' \item \code{\link{stats_rank}} 
#' \item \code{\link{stats_net_score}}
#' }
#' 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords stats
#' @export
stats_bin <- function(x){
	if(is.null(x)){
		return(NULL)
	}
	weight <- NULL; rm(weight) # Dummy to trick R CMD check
	cbweight <- ddply(x, c("cbreak", "question"), summarise, weight=sum(weight))
	row.names(cbweight) <- paste(cbweight$cbreak, cbweight$question, sep="_")
	x$weight <- x$weight / cbweight[paste(x$cbreak, x$question, sep="_"), ]$weight
	
	
	if (length(unique(x$question))==1){
		# code single
		df <- ddply(x, c("cbreak", "response"), 
				summarise, 
				value=sum(weight)
		)
		if (is.ordered(x$response)){
			df$response <- factor(df$response, levels=levels(x$response), ordered=TRUE)
		} else {
			df <- reorder_response(df)
		}
		
	} else {
		# code array
		df <- ddply(x, c("cbreak", "question", "response"), 
				summarise, 
				value=sum(weight)
		)
	}
	surveyor_stats(
			df,
			ylabel="Fraction of respondents")
}


#' Calculates numeric sum
#'
#' Add description 
#' 
#' @param x A data frame with four columns: cbreak, question, response, weight 
#' @return A data frame with three columns: cbreak, variable, value
#' @seealso
#' Stats functions:
#' \itemize{
#' \item \code{\link{stats_bin}} 
#' \item \code{\link{stats_rank}} 
#' \item \code{\link{stats_net_score}}
#' }
#' 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords stats
#' @export
stats_sum <- function(x){
	if(is.null(x)){
		return(NULL)
	}
	weight <- NULL; rm(weight) # Dummy to trick R CMD check
	cbweight <- ddply(x, c("cbreak", "question"), summarise, weight=sum(weight))
	row.names(cbweight) <- paste(cbweight$cbreak, cbweight$question, sep="_")
	x$weight <- x$weight / cbweight[paste(x$cbreak, x$question, sep="_"), ]$weight
	
	
	if (length(unique(x$question))==1){
		# code single
		df <- ddply(x, c("cbreak"), 
				summarise, 
				value=sum(weight*response, na.rm=TRUE)
		)
		
	} else {
		# code array
		df <- ddply(x, c("cbreak", "question"), 
				summarise, 
				value=sum(weight*response, na.rm=TRUE)
		)
	}
	surveyor_stats(
			df,
			ylabel="Value",
			formatter="format")
}


#' Calculates summary statistics for ranking type questions
#' 
#' Takes the result of a code_function, e.g. code_single(), and calculates
#' summary values, for direct plotting by a plot_function, e.g. plot_bar()
#' 
#' @param x A data frame with four columns: cbreak, question, response, weight 
#' @param top_n Numeric, indicates how the ranking is summarised
#' @return A data frame with three columns: cbreak, variable, value
#' @seealso
#' Stats functions:
#' \itemize{
#' \item \code{\link{stats_bin}} 
#' \item \code{\link{stats_rank}} 
#' \item \code{\link{stats_net_score}}
#' }
#' 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords stats
#' @export
stats_rank <- function(x, top_n=3){
	weight <- NULL; rm(weight) # Dummy to trick R CMD check
	value <- NULL; rm(value) # Dummy to trick R CMD check
	if(is.null(x)){
		return(NULL)
	}
	cbreakweight <- ddply(x, "cbreak", summarise, weight=sum(weight))
	row.names(cbreakweight) <- cbreakweight$cbreak
	x$weight <- x$weight / cbreakweight[x$cbreak, ]$weight
	
	
	if (length(unique(x$question))==1){
		# code single
		df <- ddply(x, c("cbreak", "response"), 
				summarise, 
				value=sum(weight)
		)
		
	} else {
		# code array
		df <- ddply(x, c("cbreak", "question", "response"), 
				summarise, 
				value=sum(weight)
		)
	}
	
	h1 <- df[df$question<=3, ]

	h2 <- ddply(h1, c("cbreak", "response"), summarize, value=sum(value))
	h2 <- reorder_response(h2)
	
#	if (is.ordered(x$response)){
#		h2$response <- factor(h2$response, levels=levels(x$response), ordered=TRUE)
#	} else {
#		resp_levels  <- df[order(h2$value, decreasing=TRUE), ]$response
#		h2$response <- factor(h2$response, levels=resp_levels, ordered=TRUE)
#	}
	
	
	surveyor_stats(
			h2,
			ylabel=paste("Percentage of responses in top", top_n)
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
#' @export 
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
#' @param x A data frame with four columns: cbreak, question, response, weight 
#' @return data frame
#' @seealso
#' Stats functions:
#' \itemize{
#' \item \code{\link{stats_bin}} 
#' \item \code{\link{stats_rank}} 
#' \item \code{\link{stats_net_score}}
#' }
#' 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords stats
#' @export
stats_net_score <- function(x){
	response <- NULL; rm(response) # Dummy to trick R CMD check
	if (length(unique(x$question))==0){
		# code single
		df <- ddply(x, c("cbreak", "response"), 
				summarise,
				value=net_score(response))
	} else {
		# code array
#		ddply(x, .(cbreak, question, response), 
		df <- ddply(x, c("cbreak", "question"), 
				summarise,
				value=net_score(response))
		quest_levels  <- df[order(df$value, decreasing=TRUE), ]$question
		df$question <- factor(df$question, levels=quest_levels, ordered=TRUE)
	}
	surveyor_stats(
			df,
			ylabel="Net score")
}

