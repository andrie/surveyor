###############################################################################
# Step 2 in the triad of code, summarize and plot
###############################################################################

#' Creates surveyor_stats object, used as input to plot.
#'  
#' Creates surveyor_stats object, used as input to plot 
#' 
#' @param data A data frame 
#' @param ylabel Character string to print as plot y label
#' @param formatter Name of a formatting function
#' @param nquestion Number of identifiable questions, used for plot sizing downstream
#' @param scale_breaks Vector that specifies breaks in ggplot
#' @param stats_method Character description of calling function name - for audit trail
#' @return A surveyor_stats object
#' @keywords internal
surveyor_stats <- function(
		data,
		ylabel = "Fraction of respondents",
		formatter="percent",
		nquestion=length(unique(data$question)),
		scale_breaks=NULL,
		stats_method=""
){
	structure(
			list(
				data=quickdf(data), 
				ylabel=ylabel,
				formatter=formatter,
				nquestion=nquestion,
				scale_breaks=scale_breaks,
				stats_method=stats_method
			),
			class = "surveyor_stats"
	)
}

#' Sorts data.frame in descending order
#' 
#' Sorts df in descending order 
#' 
#' @param df A data frame containing at least two columns: response and value 
#' @return A data frame
#' @keywords internal
reorder_response <- function(df){
	resp_levels  <- df[order(df$value, decreasing=TRUE), ]$response
	resp_levels <- unique(resp_levels)
	df$response <- factor(df$response, levels=resp_levels, ordered=TRUE)
	df
}



#' Tests for all NA values.
#'  
#' Tests for all NA values. 
#' 
#' @param x A list, data frame or vector 
#' @return TRUE if all values are NA, FALSE otherwise
#' @keywords internal
all_na <- function(x){
	if (is.list(x) || is.data.frame(x)){
		return(all(as.logical(sapply(x, function(y) all(is.na(y))))))
	}
	return(all(is.na(x)))
}

#' Tests for all NULL values 
#' 
#' Tests for all NULL values 
#' 
#' @param x A list, data frame or vector 
#' @return TRUE if all values are NULL, FALSE otherwise
#' @keywords internal
all_null <- function(x){
	if (is.list(x) || is.data.frame(x)){
		return(all(as.logical(sapply(x, function(y) all(is.null(y))))))
	}
	return(all(is.null(x)))
}

#' Guesses whether a question should be coded as net score.
#' 
#' Evaluates the first and last factor levels of x and tests whether these levels contain words in match_words
#' 
#' @param x A factor of character strings
#' @param match_words A character vector of words to match 
#' @return A data frame with three columns - cbreak, variable, value
#' @keywords internal
identify_net_score <- function(x, match_words = NULL){
	if(is.null(match_words)) match_words <- c(
				"satisfied", "dissatisfied",
				"agree", "disagree",
				"important", "unimportant",
				"likely", "unlikely"
		)	
	l <- levels(x)[c(1, nlevels(x))]
	w <- gsub("[[:punct:]]", "", l)
	w <- tolower(w)
	w <- strsplit(w, " ")
	all(sapply(w, function(wx)any(wx %in% tolower(match_words))))
}


#' Inspects data and guesses what type of analysis to do.
#' 
#' If data is categorical stats_bin, if data is metric then stats_sum
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
stats_guess <- function(x){
	if(is.null(x)){
		return(NULL)
	}
	
	if(is.factor(x$response)){
		if(identify_net_score(x$response)){
			stats_net_score(x)
		} else {	
			stats_bin_percent(x)
		}
	} else {
		if(is.numeric(x$response)){
			stats_sum(x)
		} else {
			stats_bin_percent(x)
		}	
	}
}



#' Calculates summary statistics.
#' 
#' Takes the result of a code_function, e.g. code_single(), and calculates summary values, for direct plotting by a plot_function, e.g. plot_bar()
#' 
#' The results are sorted in descending order of value, and "response" is coerced into an ordered factor (unless "response" is already an ordered factor).
#' 
#' @param x A data frame with four columns: cbreak, question, response, weight 
#' @param ylabel The label to print on y-axis of plots; used downstream
#' @param stats_method A character vector describing name of stats method.  Used for audit trail
#' @param convert_to_percent If true, will express results as fractions, rather than counts
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
stats_bin <- function(x, ylabel="Respondents", stats_method="stats_bin", convert_to_percent=FALSE){
	if(is.null(x)){
		return(NULL)
	}
	weight <- NULL; rm(weight) # Dummy to trick R CMD check
	
  cbweight <- ddply(x, c("cbreak", "question"), summarise, weight=sum(weight))
  row.names(cbweight) <- paste(cbweight$cbreak, cbweight$question, sep="_")
  
#  x <- data.table(x)
#  print(str(x))
#  cbweight <- x[, list(weight=sum(weight), names=paste(cbreak, question, sep="_")), 
#      by=list(cbreak, question)]
#  cbweight$names <- paste(cbweight$cbreak, cbweight$question, sep="_")
	
	if(convert_to_percent){
		x$weight <- x$weight / cbweight[paste(x$cbreak, x$question, sep="_"), ]$weight
#    x$weight <- x$weight / with(cbweight, weight[match(paste(x$cbreak, x$question, sep="_"), names)])
  }
	
	
	if (is.factor(x$response)){
		if(nlevels(x$response[drop=TRUE])==1) x$response <- as.character(x$response[drop=TRUE])
	}
	
	if (length(unique(x$question))==1){
		# code single
		df <- ddply(x, c("cbreak", "response"), summarise, value=sum(weight))
#    df <- x[, list(value=sum(weight)), by=c("cbreak", "response")]
    #print(str(df))
    if (is.factor(x$response)){
			x$response <- x$response[drop=TRUE]
			if(is.ordered(x$response)){
				df$response <- factor(df$response, levels=levels(x$response), ordered=TRUE)
			} else {
				df <- reorder_response(df)
			}
		}		
		
	} else {
		# code array
		df <- ddply(x, c("cbreak", "question", "response"), summarise, value=sum(weight))
#    print(str(x))
#    df <- x[, list(value=sum(weight)), by=c("cbreak", "question", "response")]
#    print(str(df))
}
	
	# Test for yes/no responses
	if(is.factor(df$response) && all(levels(df$response) %in% c("Yes", "No"))){
		df <- df[df$response == levels(df$response)[which(levels(df$response) == "Yes")], ]
	}
	if(is.logical(df$response)){
		df <- df[df$response == TRUE, ]
	}
	
	surveyor_stats(
			df,
			ylabel=ylabel,
			stats_method=stats_method,
			formatter=ifelse(convert_to_percent, "paste_percent", "format"))
}

#' Calculates summary statistics
#' 
#' Wrapper around \code{\link{stats_bin}}, binning statistics and calculating percentage
#' 
#' The results are sorted in descending order of value, and "response" is coerced into an ordered factor (unless "response" is already an ordered factor).
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
stats_bin_percent <- function(x){
	stats_bin(
			x,
			ylabel="Fraction of respondents",
			stats_method="stats_bin_percent",
			convert_to_percent=TRUE)
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
	response <- NULL; rm(response) # Dummy to trick R CMD check
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
	
	scale_breaks <- c(min(df$value), 0, max(df$value))
	scale_breaks <- round_first_signif(scale_breaks)
	
	surveyor_stats(
			df,
			ylabel="Value",
			formatter="format_round",
			stats_method="stats_sum",
			scale_breaks=scale_breaks)
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
stats_mean <- function(x){
	if(is.null(x)){
		return(NULL)
	}
	weight <- NULL; rm(weight) # Dummy to trick R CMD check
	response <- NULL; rm(response) # Dummy to trick R CMD check

	x$response <- as.numeric(x$response)
	cbweight <- ddply(x, c("cbreak", "question"), summarise, weight=sum(weight))
	row.names(cbweight) <- paste(cbweight$cbreak, cbweight$question, sep="_")
	x$weight <- x$weight / cbweight[paste(x$cbreak, x$question, sep="_"), ]$weight
	
	
	if (length(unique(x$question))==1){
		# code single
		df <- ddply(x, c("cbreak"), 
				summarise, 
				value = sum(weight*response, na.rm=TRUE)
		)
		
	} else {
		# code array
		df <- ddply(x, c("cbreak", "question"), 
				summarise, 
				value = sum(weight*response, na.rm=TRUE)
		)
	}
	
	scale_breaks <- c(min(df$value), 0, max(df$value))
	scale_breaks <- round_first_signif(scale_breaks)
	
	surveyor_stats(
			df,
			ylabel="Value",
			formatter="format",
			stats_method="stats_mean",
			scale_breaks=scale_breaks)
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
	x$question <- as.numeric(x$question)
	x <- x[x$question <= top_n, ]
	
	cbreakweight <- ddply(x, "cbreak", summarise, weight=sum(weight))
#	row.names(cbreakweight) <- cbreakweight$cbreak
	x$weight <- top_n * x$weight / cbreakweight$weight[match(x$cbreak, cbreakweight$cbreak)]
	
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
	
#	df$question <- as.numeric(df$question)
#	h1 <- df[df$question <= top_n, ]
#	h2 <- ddply(h1, c("cbreak", "response"), function(xt)summarise(xt, value=sum(xt$value)))
#	h2 <- reorder_response(h2)
	h2 <- ddply(df, c("cbreak", "response"), function(xt)summarise(xt, value=sum(xt$value)))
	h2 <- reorder_response(h2)
	
	surveyor_stats(
			h2,
			ylabel=paste("Percentage of responses in top", top_n),
			stats_method="stats_rank"
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

#' Code survey data as net score.
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
		#df$question <- factor(df$question, levels=quest_levels, ordered=TRUE)
	}
	surveyor_stats(
			df,
			ylabel="Net score",
      formatter="format",
			stats_method="stats_net_score")
}

#' Code survey data as text.
#'
#' Code survey data in text form
#' 
#' @param x A data frame column 
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
stats_text <- function(x){
  surveyor_stats(
      x,
      ylabel="NA",
      formatter="format",
      stats_method="stats_text")
}