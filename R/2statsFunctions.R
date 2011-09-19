#-------------------------------------------------------------------------------
# Step 2 in the triad of code, summarize and plot
#-------------------------------------------------------------------------------

#' Creates surveyorStats object, used as input to plot.
#'  
#' Creates surveyorStats object, used as input to plot 
#' 
#' @param data A data frame 
#' @param surveyorCode A surveyorCode object
#' @param ylabel Character string to print as plot y label
#' @param formatter Name of a formatting function
#' @param nquestion Number of identifiable questions / responses, used for plot sizing downstream
#' @param scale_breaks Vector that specifies breaks in ggplot
#' @param stats_method Character description of calling function name - for audit trail
#' @return A surveyorStats object
#' @keywords internal
as.surveyorStats <- function(
		data,
    surveyorCode,
		ylabel = "Fraction of respondents",
		formatter="percent",
		nquestion=NULL,
		scale_breaks=NULL,
		stats_method="",
    plotFunction=""
){
  stopifnot(is.surveyorCode(surveyorCode))
  if(is.null(nquestion)) 
    nquestion <- ifelse(
        !is.null(data$question), 
        length(unique(data$question)),
        length(unique(data$response)))
	structure(
			list(
				data=quickdf(data), 
        surveyor = surveyorCode$surveyor,
        qid = surveyorCode$qid,
				ylabel=ylabel,
				formatter=formatter,
				nquestion=nquestion,
				scale_breaks=scale_breaks,
				stats_method=stats_method,
        plotFunction=plotFunction
			),
			class = "surveyorStats"
	)
}

#' Test object for membership of class "surveyorStats".
#'  
#' Test object for membership of class "surveyorStats".
#' 
#' @param x Object 
#' @return TRUE or FALSE
#' @keywords internal
is.surveyorStats <- function(x){
  inherits(x, "surveyorStats")
}



#' Guesses whether a question should be coded as net score.
#' 
#' Evaluates the first and last factor levels of x and tests whether these levels contain words in match_words
#' 
#' @param x A factor of character strings
#' @param match_words A character vector of words to match 
#' @return A data frame with three columns - cbreak, variable, value
#' @keywords internal
identify_netScore <- function(x, match_words = NULL){
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
#' If data is categorical statsBin, if data is metric then statsSum
#' 
#' @param surveyorCode An object of class "surveyorCode".  This is a list with the first element being a data frame with four columns: cbreak, question, response, weight 
#' @param ... Passed to relevant stats function
#' @return A data frame with three columns: cbreak, variable, value
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords stats
#' @family statsFunctions
#' @export
statsGuess <- function(surveyorCode, ...){
  stopifnot(is.surveyorCode(surveyorCode))
  x <- surveyorCode$data
  if(is.null(x)){
		return(NULL)
	}
	
	if(is.factor(x$response)){
		if(identify_netScore(x$response)){
			statsNetScore(surveyorCode, ...)
		} else {	
			statsBinPercent(surveyorCode, ...)
		}
	} else {
		if(is.numeric(x$response)){
			statsSum(surveyorCode, ...)
		} else {
			statsBinPercent(surveyorCode, ...)
		}	
	}
}



#' Calculates summary statistics.
#' 
#' Takes the result of a code_function, e.g. codeSingle(), and calculates summary values, for direct plotting by a plotFunction, e.g. plotBar()
#' 
#' The results are sorted in descending order of value, and "response" is coerced into an ordered factor (unless "response" is already an ordered factor).
#' 
#' @param surveyorCode An object of class "surveyorCode".  This is a list with the first element being a data frame with four columns: cbreak, question, response, weight 
#' @param ylabel The label to print on y-axis of plots; used downstream
#' @param stats_method A character vector describing name of stats method.  Used for audit trail
#' @param convert_to_percent If true, will express results as fractions, rather than counts
#' @return A data frame with three columns: cbreak, variable, value
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords stats
#' @family statsFunctions
#' @export
statsBin <- function(surveyorCode, ylabel="Respondents", stats_method="statsBin", convert_to_percent=FALSE){
  stopifnot(is.surveyorCode(surveyorCode))
  x <- surveyorCode$data
  
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
				df <- reorderResponse(df)
			}
		}		
		
	} else {
		# code array
		df <- ddply(x, c("cbreak", "question", "response"), summarise, value=sum(weight))
#    print(str(x))
#    df <- x[, list(value=sum(weight)), by=c("cbreak", "question", "response")]
#    print(str(df))
  df <- reorderQuestion(df, reverse=TRUE)
}
	
	# Test for yes/no responses
	if(is.factor(df$response) && all(levels(df$response) %in% c("Yes", "No"))){
		df <- df[df$response == levels(df$response)[which(levels(df$response) == "Yes")], ]
	}
	if(is.logical(df$response)){
		df <- df[df$response == TRUE, ]
	}
	
	as.surveyorStats(
			df,
      surveyorCode,
			ylabel=ylabel,
			stats_method=stats_method,
			formatter=ifelse(convert_to_percent, "paste_percent", "format"))
}

#' Calculates summary statistics
#' 
#' Wrapper around \code{\link{statsBin}}, binning statistics and calculating percentage
#' 
#' The results are sorted in descending order of value, and "response" is coerced into an ordered factor (unless "response" is already an ordered factor).
#' 
#' @param surveyorCode An object of class "surveyorCode".  This is a list with the first element being a data frame with four columns: cbreak, question, response, weight
#' @param ... Passed to surveyorPlot
#' @return A data frame with three columns: cbreak, variable, value
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords stats
#' @family statsFunctions
#' @export
statsBinPercent <- function(surveyorCode, ...){
  stopifnot(is.surveyorCode(surveyorCode))
  #x <- surveyorCode$data
  statsBin(
      surveyorCode,
			ylabel="Fraction of respondents",
			stats_method="statsBinPercent",
			convert_to_percent=TRUE)
}


#' Calculates median
#'
#' Add description 
#' 
#' @param surveyorCode An object of class "surveyorCode".  This is a list with the first element being a data frame with four columns: cbreak, question, response, weight 
#' @param fun The function to use to calculate central tendency, e.g. mean, median or sum
#' @param stats_method The name of the function, for audit trail 
#' @param yLabel y axis label
#' @return A data frame with three columns: cbreak, variable, value
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords stats
#' @family statsFunctions
#' @export
statsMedian <- function(surveyorCode, fun="weightedMedian", stats_method="statsMedian", yLabel="Median value"){
  stopifnot(is.surveyorCode(surveyorCode))
  x <- surveyorCode$data
  if(is.null(x)){
    return(NULL)
  }
  
#  x$response <- ifelse(is.factor(x$response),
#      as.numeric(levels(x$response)[x$response]),
#      as.numeric(x$response)
#  )
#  cat(str(x$response))
  #browser()
  if (length(unique(x$question))==1){
    # code single
    df <- ddply(x, c("cbreak"), 
        summarise, 
        value = weightedMedian("response", "weight", na.rm=TRUE)
    )
    
  } else {
    # code array
    df <- ddply(x, c("cbreak", "question"), 
        summarise, 
        value = weightedMedian("response", "weight", na.rm=TRUE)
    )
  }
  
#  df$value <- levels(x$response)[df$value]
#  df$value <- factor(df$value, levels=levels(x$response))
  
  #scale_breaks <- c(min(df$value), 0, max(df$value))
  #scale_breaks <- round_first_signif(scale_breaks)
  
  as.surveyorStats(
      df,
      surveyorCode,
      ylabel=yLabel,
      formatter="format",
      stats_method=stats_method
#      scale_breaks=scale_breaks
  )
}


#' Calculates numeric sum
#'
#' Add description 
#' 
#' @param surveyorCode An object of class "surveyorCode".  This is a list with the first element being a data frame with four columns: cbreak, question, response, weight
#' @param ... Passed to surveyorPlot
#' @return A data frame with three columns: cbreak, variable, value
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords stats
#' @family statsFunctions
#' @export
statsSum <- function(surveyorCode, ...){
  stopifnot(is.surveyorCode(surveyorCode))
  x <- surveyorCode$data
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
	
	as.surveyorStats(
			df,
      surveyorCode,
			ylabel="Value",
			formatter="format_round",
			stats_method="statsSum",
			scale_breaks=scale_breaks)
}



#' Calculates numeric sum
#'
#' Add description 
#' 
#' @param surveyorCode An object of class "surveyorCode".  This is a list with the first element being a data frame with four columns: cbreak, question, response, weight
#' @param fun The function to use to calculate central tendency, e.g. mean, median or sum
#' @param stats_method The name of the function, for audit trail 
#' @return A data frame with three columns: cbreak, variable, value
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords stats
#' @family statsFunctions
#' @export
statsMean <- function(surveyorCode, fun="mean", stats_method="statsMean"){
  stopifnot(is.surveyorCode(surveyorCode))
  x <- surveyorCode$data
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
	
	as.surveyorStats(
			df,
      surveyorCode,
      ylabel="Value",
			formatter="format",
			stats_method=stats_method,
			scale_breaks=scale_breaks)
}


#' Calculates summary statistics for ranking type questions
#' 
#' Takes the result of a code_function, e.g. codeSingle(), and calculates
#' summary values, for direct plotting by a plotFunction, e.g. plotBar()
#' 
#' @param surveyorCode An object of class "surveyorCode".  This is a list with the first element being a data frame with four columns: cbreak, question, response, weight 
#' @param top_n Numeric, indicates how the ranking is summarised
#' @return A data frame with three columns: cbreak, variable, value
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords stats
#' @family statsFunctions
#' @export
statsRank <- function(surveyorCode, top_n=3){
  stopifnot(is.surveyorCode(surveyorCode))
  x <- surveyorCode$data
  weight <- NULL; rm(weight) # Dummy to trick R CMD check
	value <- NULL; rm(value) # Dummy to trick R CMD check
	if(is.null(x)){
		return(NULL)
	}
  
  ### Removes all characters other than digits from question text, e.g. "Rank 1" >- "1"
  x$question <- gsub(".*([[:digit:]]).*", "\\1", x$question)
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
#	h2 <- reorderResponse(h2)
	h2 <- ddply(df, c("cbreak", "response"), function(xt)summarise(xt, value=sum(xt$value)))
	h2 <- reorderResponse(h2)
	
	as.surveyorStats(
			h2,
      surveyorCode,
      ylabel=paste("Percentage of responses in top", top_n),
			stats_method="statsRank"
	)
}

#-------------------------------------------------------------------------------#
####  net score calculation and graphing                                      ###
#-------------------------------------------------------------------------------#

#' Calculates a net score
#'
#' This assumes that x is an ordered factor, from low to high
#' It then calculates a net percentage score
#' 
#' @param x An ordered factor
#' @export 
netScore <- function(x){
	
	x <- x[!is.na(x)]
	
	xu <- unclass(x)
	m <- ceiling(length(levels(x))/2)
	below <- length(xu[xu < m])
	above <- length(xu[xu > m])
	net <- above - below
	netScore <- net / length(x)
	netScore
}

# TODO: Surveyor: Fix statsNetScore to deal with weighting 

#' Code survey data as net score.
#'
#' Code survey data in net score form
#' 
#' @param surveyorCode An object of class "surveyorCode".  This is a list with the first element being a data frame with four columns: cbreak, question, response, weight 
#' @return data frame
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords stats
#' @family statsFunctions
#' @export
statsNetScore <- function(surveyorCode){
  stopifnot(is.surveyorCode(surveyorCode))
  x <- surveyorCode$data
  response <- NULL; rm(response) # Dummy to trick R CMD check
	if (length(unique(x$question))==0){
		# code single
		df <- ddply(x, c("cbreak", "response"), 
				summarise,
				value=netScore(response))
	} else {
		# code array
#		ddply(x, .(cbreak, question, response), 
		df <- ddply(x, c("cbreak", "question"), 
				summarise,
				value=netScore(response))
		#quest_levels  <- df[order(df$value, decreasing=TRUE), ]$question
		#df$question <- factor(df$question, levels=quest_levels, ordered=TRUE)
    df <- reorderQuestion(df, reverse=FALSE)
  }
	as.surveyorStats(
			df,
      surveyorCode,
      ylabel="Net score",
      formatter="format",
			stats_method="statsNetScore",
      plotFunction="plotNetScore")
}

#' Code survey data as text.
#'
#' Code survey data in text form
#' 
#' @param surveyorCode An object of class "surveyorCode".  This is a list with the first element being a data frame with four columns: cbreak, question, response, weight 
#' @return data frame
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords stats
#' @family statsFunctions
#' @export
statsText <- function(surveyorCode){
  stopifnot(is.surveyorCode(surveyorCode))
  x <- surveyorCode$data
  as.surveyorStats(
      x,
      surveyorCode,
      ylabel="NA",
      formatter="format",
      stats_method="statsText",
      plotFunction="plotText")
}