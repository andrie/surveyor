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
		formatter="formatPercent",
		nquestion=NULL,
		scale_breaks=NULL,
		stats_method="",
    plotFunction="",
    ...
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
        surveyorDefaults = surveyorCode$surveyorDefaults,
        plotTitle = surveyorCode$plotTitle,
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

#------------------------------------------------------------------------------

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
#' @param ... Passed to \code{\link{as.surveyorStats}}
#' @return A data frame with three columns: cbreak, variable, value
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords stats
#' @family statsFunctions
#' @export
statsBin <- function(
    surveyorCode, 
    ylabel="Respondents", 
    stats_method="statsBin", 
    convert_to_percent=FALSE, 
#    autosort = TRUE,
    ...){
  stopifnot(is.surveyorCode(surveyorCode))
  x <- surveyorCode$data
  
#  browser()
  if(convert_to_percent){
    cbweight <- splitPercentCombine(x, statsFunction=weightedCount)
    row.names(cbweight) <- paste(cbweight$cbreak, cbweight$question, sep="_")
    x$weight <- x$weight / cbweight[paste(x$cbreak, x$question, sep="_"), ]$weight
  }

  if (is.factor(x$response)){
    if(nlevels(x$response[drop=TRUE])==1) x$response <- as.character(x$response[drop=TRUE])
  }
    
  dat <- splitBinCombine(x, statsFunction=weightedCount)
  
  if (length(unique(x$question))==1){
    # code single
    if (is.factor(x$response)){
      x$response <- x$response[drop=TRUE]
      if(is.ordered(x$response)){
        dat$response <- factor(dat$response, levels=levels(x$response), ordered=TRUE)
      } else {
        dat <- reorderResponse(dat)
      }
    }   
    
  } else {
    # code array
    if(!is.ordered(dat$question)) dat <- reorderQuestion(dat, reverse=TRUE)
  }
  
  # Test for yes/no responses
  if(is.factor(dat$response) && all(levels(dat$response) %in% c("Yes", "No"))){
    dat <- dat[dat$response == levels(dat$response)[which(levels(dat$response) == "Yes")], ]
  }
  if(is.logical(dat$response)){
    dat <- dat[dat$response == TRUE, ]
  }
  if(is.numeric(dat$response)){
    dat$response <- as.character(dat$response)
  }
  
  
  as.surveyorStats(
      dat,
      surveyorCode,
      ylabel=ylabel,
      stats_method=stats_method,
      formatter=ifelse(convert_to_percent, "formatPercent", "format"),
      ...)
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
statsBinOld <- function(surveyorCode, ylabel="Respondents", stats_method="statsBin", convert_to_percent=FALSE){
  stopifnot(is.surveyorCode(surveyorCode))
  x <- surveyorCode$data
  
	if(is.null(x)){
		return(NULL)
	}
	weight <- NULL; rm(weight) # Dummy to trick R CMD check
	
  
#  x <- data.table(x)
#  print(str(x))
#  cbweight <- x[, list(weight=sum(weight), names=paste(cbreak, question, sep="_")), 
#      by=list(cbreak, question)]
#  cbweight$names <- paste(cbweight$cbreak, cbweight$question, sep="_")
	
	if(convert_to_percent){
    cbweight <- ddply(x, c("cbreak", "question"), summarise, weight=sum(weight))
    row.names(cbweight) <- paste(cbweight$cbreak, cbweight$question, sep="_")
    x$weight <- x$weight / cbweight[paste(x$cbreak, x$question, sep="_"), ]$weight
#    x$weight <- x$weight / with(cbweight, weight[match(paste(x$cbreak, x$question, sep="_"), names)])
  }
	
	
	if (is.factor(x$response)){
		if(nlevels(x$response[drop=TRUE])==1) x$response <- as.character(x$response[drop=TRUE])
	}
	
	if (length(unique(x$question))==1){
		# code single
		dat <- ddply(x, c("cbreak", "response"), summarise, value=sum(weight))
#    dat <- x[, list(value=sum(weight)), by=c("cbreak", "response")]
    #print(str(dat))
    if (is.factor(x$response)){
			x$response <- x$response[drop=TRUE]
			if(is.ordered(x$response)){
				dat$response <- factor(dat$response, levels=levels(x$response), ordered=TRUE)
			} else {
				dat <- reorderResponse(dat)
			}
		}		
		
	} else {
		# code array
		dat <- ddply(x, c("cbreak", "question", "response"), summarise, value=sum(weight))
#    print(str(x))
#    dat <- x[, list(value=sum(weight)), by=c("cbreak", "question", "response")]
#    print(str(dat))
  dat <- reorderQuestion(dat, reverse=TRUE)
}
	
	# Test for yes/no responses
	if(is.factor(dat$response) && all(levels(dat$response) %in% c("Yes", "No"))){
		dat <- dat[dat$response == levels(dat$response)[which(levels(dat$response) == "Yes")], ]
	}
	if(is.logical(dat$response)){
		dat <- dat[dat$response == TRUE, ]
	}
	
	as.surveyorStats(
			dat,
      surveyorCode,
			ylabel=ylabel,
			stats_method=stats_method,
			formatter=ifelse(convert_to_percent, "formatPercent", "format"))
}

#' Calculates summary statistics
#' 
#' Wrapper around \code{\link{statsBin}}, binning statistics and calculating percentage
#' 
#' The results are sorted in descending order of value, and "response" is coerced into an ordered factor (unless "response" is already an ordered factor).
#' 
#' @inheritParams statsBin
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
			convert_to_percent=TRUE,
      ...)
}

statsBinPercentOld <- function(surveyorCode, ...){
  stopifnot(is.surveyorCode(surveyorCode))
  #x <- surveyorCode$data
  statsBinOld(
      surveyorCode,
      ylabel="Fraction of respondents",
      stats_method="statsBinPercent",
      convert_to_percent=TRUE,
      ...)
}


#' Calculates numeric sum.
#'
#' Add description 
#' 
#' @inheritParams statsCentral
#' @return A data frame with three columns: cbreak, variable, value
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords stats
#' @family statsFunctions
#' @export
statsSum <- function(surveyorCode, ylabel="Sum", ...){
  statsCentral(surveyorCode, statsFunction="weightedSum", ylabel=ylabel, ...)
}

#' Calculates numeric count.
#'
#' Add description 
#' 
#' @inheritParams statsCentral
#' @return A data frame with three columns: cbreak, variable, value
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords stats
#' @family statsFunctions
#' @export
statsCount <- function(surveyorCode, ylabel="Count", ...){
  statsCentral(surveyorCode, statsFunction="weightedCount", ylabel=ylabel, ...)
}

#' Calculates central tendency.
#'
#' Add description 
#' 
#' @param surveyorCode An object of class "surveyorCode".  This is a list with the first element being a data frame with four columns: cbreak, question, response, weight
#' @param statsFunction The name of a weighted central tendency function, e.g. \code{\link{weightedMean}}, \code{\link{weightedSum}}, \code{\link{weightedMedian}} or \code{\link{weightedCount}}
#' @param ylabel y-axis label on plot
#' @param ... Other arguments passed to \code{\link{as.surveyorStats}}
#' @return A data frame with three columns: cbreak, question, value
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords stats
#' @family statsFunctions
#' @export
statsCentral <- function(
    surveyorCode, 
    statsFunction=c("weightedMean", "median", "sum", "count"),
    ylabel="Mean", ...){
  stopifnot(is.surveyorCode(surveyorCode))

  x <- surveyorCode$data
  if(is.null(x)){
    return(NULL)
  }
  
  cFunction <- switch(statsFunction[1],
      mean = weightedMean,
      median = weighedMedian, 
      sum = weightedSum,
      count= weighedCount)
  cFunction <- match.fun(statsFunction[1])
  dat <- splitMeanCombine(x, cFunction)
  
  scale_breaks <- c(min(dat$value), 0, max(dat$value))
  scale_breaks <- roundFirstSignif(scale_breaks)
  
  as.surveyorStats(
      dat,
      surveyorCode,
      ylabel=ylabel,
      formatter="format",
      stats_method=statsFunction,
      scale_breaks=scale_breaks)
}


#' Calculates numeric mean.
#'
#' Add description 
#' 
#' @inheritParams statsCentral
#' @return A data frame with three columns: cbreak, variable, value
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords stats
#' @family statsFunctions
#' @export
statsMean <- function(surveyorCode, ylabel="Mean", ...){
  statsCentral(surveyorCode, statsFunction="weightedMean", ylabel=ylabel, ...)
}



#' Calculates numeric median.
#'
#' Add description 
#' 
#' @inheritParams statsCentral
#' @return A data frame with three columns: cbreak, variable, value
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords stats
#' @family statsFunctions
#' @export
statsMedian <- function(surveyorCode, ylabel="Median", ...){
  statsCentral(surveyorCode, statsFunction="weightedMedian", ylabel=ylabel, ...)
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
		dat <- ddply(x, c("cbreak", "response"), 
				summarise, 
				value=sum(weight)
		)
		
	} else {
		# code array
		dat <- ddply(x, c("cbreak", "question", "response"), 
				summarise, 
				value=sum(weight)
		)
	}
	
#	dat$question <- as.numeric(dat$question)
#	h1 <- dat[dat$question <= top_n, ]
#	h2 <- ddply(h1, c("cbreak", "response"), function(xt)summarise(xt, value=sum(xt$value)))
#	h2 <- reorderResponse(h2)
	h2 <- ddply(dat, c("cbreak", "response"), function(xt)summarise(xt, value=sum(xt$value)))
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
		dat <- ddply(x, c("cbreak", "response"), 
				summarise,
				value=netScore(response))
	} else {
		# code array
#		ddply(x, .(cbreak, question, response), 
		dat <- ddply(x, c("cbreak", "question"), 
				summarise,
				value=netScore(response))
		#quest_levels  <- dat[order(dat$value, decreasing=TRUE), ]$question
		#dat$question <- factor(dat$question, levels=quest_levels, ordered=TRUE)
    dat <- reorderQuestion(dat, reverse=FALSE)
  }
	as.surveyorStats(
			dat,
      surveyorCode,
      ylabel="Net score",
      formatter="formatPercent",
			stats_method="statsNetScore",
      plotFunction="plotNetScore")
}


#' Removes a selected range of 'content-free' strings.
#' 
#' Removes all strings that match the regular expression.
#' 
#' @param x A regular expression in character format
#' @param remove A character string passed to grep() 
#' @export
filter_nocomment <- function(x, remove="^(No|no|NO|Nope|None|none|n.a.|NA|n/a).?$"){
  # This function strips out some content-free answers
  z <- x[!is.na(x)]
  grep(remove, z, invert=TRUE)
}


#' Code survey data as text.
#'
#' Code survey data in text form
#' 
#' @param surveyorCode An object of class "surveyorCode".  This is a list with the first element being a data frame with four columns: cbreak, question, response, weight 
#' @param ... Other arguments passed to \code{\link{as.surveyorStats}}
#' @return data frame
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords stats
#' @family statsFunctions
#' @export
statsText <- function(surveyorCode, ...){
  stopifnot(is.surveyorCode(surveyorCode))
  x <- surveyorCode$data
  
  if(is.null(x)){
    return(NULL)
  }

  keep1 <- which(!is.na(x$response))
  keep2 <- filter_nocomment(x$response)
  
  x <- x[intersect(keep1, keep2), ]
  if(nrow(x)==0) x <- surveyorCode$data[1, ]
  
  as.surveyorStats(
      x,
      surveyorCode,
      ylabel="NA",
      formatter="format",
      stats_method="statsText",
      plotFunction="plotText")
}
