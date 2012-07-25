# surveyorStats helper functions
# 
# Author: Andrie
#-------------------------------------------------------------------------------

#' Sorts data.frame responses in descending order by value.
#' 
#' Sorts data.frame responses in descending order by value 
#' 
#' @param df A data frame containing at least two columns: response and value 
#' @return A data frame
#' @keywords internal
#' @seealso \code{\link{as.surveyorStats}}
#' @family stats helper functions
reorderResponse <- function(df){
  resp_levels  <- df[order(df$value, decreasing=TRUE), ]$response
  resp_levels <- unique(resp_levels)
  df$response <- factor(df$response, levels=resp_levels, ordered=TRUE)
  df
}

#' Sorts data.frame questions in descending order by value.
#' 
#' Sorts data.frame question in descending order by value 
#' 
#' @param df A data frame containing at least two columns: response and value 
#' @return A data frame
#' @keywords internal
#' @seealso \code{\link{as.surveyorStats}}
#' @family stats helper functions
reorderQuestion <- function(df, reverse=FALSE){
  q_levels  <- df[order(df$value, decreasing=TRUE), ]$question
  q_levels <- unique(q_levels)
  if(reverse) q_levels <- rev(q_levels)
  df$question <- factor(df$question, levels=q_levels, ordered=TRUE)
  df
}


#' Tests for all NA values.
#'  
#' Tests for all NA values. 
#' 
#' @param x A list, data frame or vector 
#' @return TRUE if all values are NA, FALSE otherwise
#' @keywords internal
#' @seealso \code{\link{as.surveyorStats}}
#' @family stats helper functions
allNA <- function(x){
  if (inherits(x, "list") || inherits(x, "data.frame")){
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
#' @seealso \code{\link{as.surveyorStats}}
#' @family stats helper functions
#' @keywords internal
allNull <- function(x){
  if (inherits(x, "list") || inherits(x, "data.frame")){
    return(all(as.logical(sapply(x, function(y) all(is.null(y))))))
  }
  return(all(is.null(x)))
}

#' Determines question type as single/grid question and single/multi response.
#' 
#' @param s A surveyorStats object
#' @seealso \code{\link{as.surveyorStats}}
#' @family stats helper functions
#' @keywords internal
qType <- function(s){
  #stopifnot(is.surveyorStats(s))
  
  qDataType <- function(data){
    if (is.null(data$question)){
      x <- ifelse (is.null(data$response), "singleQ_singleResponse", "singleQ_multiResponse")
    } else {
      x <- ifelse(is.null(data$response), "gridQ_singleResponse", "gridQ_multiResponse")
    }
    if(x=="gridQ_multiResponse" && is.yesno(s)) x <- "gridQ_singleResponse"
    x
    
  }
    
  if(is.surveyorStats(s)){
    if(is.null(s$data)) return(NULL)
    qDataType(s$data)
  } else {
    qDataType(s)
  }

}


#' Checks if question has response of yes/no.
#' 
#' @param s A surveyorStats object
#' @keywords internal
#' @seealso \code{\link{as.surveyorStats}}
#' @family stats helper functions
is.yesno <- function(s){
  #stopifnot(is.surveyorStats(s))
  if(is.surveyorStats(s)) dat <- s$data else dat <- s
  ifelse(!is.null(dat$response), x <- dat$response, return(FALSE))  
  ret <- FALSE
  if(is.factor(x)){
    if(length(levels(x))==2){
      if(sort(levels(x))==c("No", "Yes") || sort(levels(x))==c("Not selected", "Yes")){
        ret <- TRUE
      }}}
  ret
}



#' Splits a data.frame, applies a function and combines into data.frame.
#' 
#' This is a fast implementation of \code{\link[plyr]{ddply}}, optimised for a \code{\link{as.surveyorCode}} object.
#' 
#' @param dat a \code{\link{as.surveyorCode}} object
#' @param statsFunction A function that calculates a weighed statistic, such as \code{\link{weightedMean}}, \code{\link{weightedMedian}}, \code{\link{weightedSum}} or \code{\link{weightedCount}}
#' @seealso splitBinCombine
#' @keywords Internal
#' @seealso \code{\link{as.surveyorStats}}
#' @family stats helper functions
splitMeanCombine <- function(dat, statsFunction=weightedMean){
  weight <- NULL
  single <- any(identical(unique(dat$question), structure(1L, .Label = "1", class = "factor")) ,
      identical(unique(dat$question), structure(1L, .Label = "1", class = c("ordered", "factor"))))
  question <- response <- value <- cbreak <- NULL  # Dummy to trick R CMD check
  cFunction = match.fun(statsFunction)
  
  if(single){
    
    pieces <- split(dat, dat$cbreak) 
    cbreak <- unname(sapply(pieces, function(i)i$cbreak[1], USE.NAMES=FALSE)) 
    if(!single) {
      question <- unname(sapply(pieces, function(i)i$question[1], USE.NAMES=FALSE))
      if(is.ordered(dat$question)) question <- ordered(question)
    }
    value <- vapply(
        pieces, function(i)cFunction(i$response, i$weight), 
        FUN.VALUE=0, USE.NAMES=FALSE)
  
    quickdf(list(cbreak = cbreak, value = value))
    
  } else {
    
    dat <- data.table(dat)
    as.data.frame(
        dat[, list(value=cFunction(response, weight)), by=list(cbreak, question)][order(cbreak, question)]
    )
    
  }
  
}

#' Splits a data.frame, applies a function and combines into data.frame.
#' 
#' This is a fast implementation of \code{\link[plyr]{ddply}}, optimised for a \code{\link{as.surveyorCode}} object.
#' 
#' @inheritParams splitMeanCombine
#' @seealso splitMeanCombine
#' @keywords Internal
#' @seealso \code{\link{as.surveyorStats}}
#' @family stats helper functions
splitBinCombine <- function(dat, statsFunction=weightedCount){
  
  single <- length(unique(dat$question)) == 1
  cFunction = match.fun(statsFunction)
  if(single){ 
    pieces <- split(dat, list(dat$response, dat$cbreak), drop=TRUE)
  } else { 
    pieces <- split(dat, list(dat$response, dat$question, dat$cbreak), drop=TRUE)
  }
  
  cbreak <- unname(sapply(
          pieces, function(i)i$cbreak[1]))
  
  if(!single) {
    question <- unname(sapply(pieces, function(i)i$question[1]))
    if(is.ordered(dat$question)) question <- ordered(question)
  }    
    
  
  response <- unname(sapply(
      pieces, function(i)i$response[1]))
      
  
  value <- vapply(
      pieces, function(i)cFunction(i$response, i$weight), 
      FUN.VALUE=0, USE.NAMES=FALSE)
  
  if(single){
    quickdf(list(cbreak = cbreak, response=response, value = value))
  } else {
    quickdf(list(cbreak = cbreak, question = question, response=response, value = value))
  }
}

#' Splits a data.frame, applies a function and combines into data.frame.
#' 
#' This is a fast implementation of \code{\link[plyr]{ddply}}, optimised for a \code{\link{as.surveyorCode}} object.
#' 
#' @inheritParams splitMeanCombine
#' @seealso splitMeanCombine
#' @keywords Internal
#' @seealso \code{\link{as.surveyorStats}}
#' @family stats helper functions
splitPercentCombine <- function(dat, statsFunction=weightedCount){
  
  cFunction = match.fun(statsFunction)
  pieces <- split(dat, list(dat$question, dat$cbreak), drop=TRUE)
  
  cbreak <- unname(sapply(
          pieces, function(i)i$cbreak[1]))
  
  question <- unname(sapply(
            pieces, function(i)i$question[1]))
  if(is.ordered(dat$question)) question <- ordered(question)
  
  weight <- vapply(pieces, function(i)sum(i$weight), 
      FUN.VALUE=0, USE.NAMES=FALSE)
  
  quickdf(list(cbreak = cbreak, question=question, weight = weight))
}


#' Calculates weighted count.
#' 
#' Computes a weighted count of a numeric vector.
#' 
#' @param x a numeric vector containing the values whose weighted sum is to be computed.
#' @param w a vector of weights the same length as x giving the weights to use for each element of x. Negative weights are treated as zero weights. Default value is equal weight to all values.#
#' @param na.rm a logical value indicating whether NA values in x should be stripped before the computation proceeds, or not. If NA, no check at all for NAs is done. Default value is NA (for efficiency).
#' @family "central tendency functions"
#' @family stats helper functions
#' @seealso \code{\link{as.surveyorStats}}
#' @export 
weightedCount <- function(x, w, na.rm=TRUE){
  notNA <- !(is.na(x) | is.na(w))
  sum(w[notNA])
}


#' Calculates weighted sum.
#' 
#' Computes a weighted sum of a numeric vector.
#' 
#' @param x a numeric vector containing the values whose weighted sum is to be computed.
#' @param w a vector of weights the same length as x giving the weights to use for each element of x. Negative weights are treated as zero weights. Default value is equal weight to all values.#
#' @param na.rm a logical value indicating whether NA values in x should be stripped before the computation proceeds, or not. If NA, no check at all for NAs is done. Default value is NA (for efficiency).
#' @family "central tendency functions"
#' @family stats helper functions
#' @seealso \code{\link{as.surveyorStats}}
#' @export 
weightedSum <- function(x, w, na.rm=TRUE){
  notNA <- !(is.na(x) | is.na(w))
  sum(x[notNA]*w[notNA])
}

#' Calculates weighted mean.
#' 
#' Computes a weighted mean of a numeric vector.
#' 
#' @param x a numeric vector containing the values whose weighted mean is to be computed.
#' @param w a vector of weights the same length as x giving the weights to use for each element of x. Negative weights are treated as zero weights. Default value is equal weight to all values.#
#' @param na.rm a logical value indicating whether NA values in x should be stripped before the computation proceeds, or not. If NA, no check at all for NAs is done. Default value is NA (for efficiency).
#' @family "central tendency functions"
#' @family stats helper functions
#' @seealso \code{\link{as.surveyorStats}}
#' @export 
weightedMean <- function(x, w, na.rm=TRUE){
  notNA <- !(is.na(x) | is.na(w))
  sum(x[notNA]*w[notNA]) / sum(w[notNA])
}


#' Calculates weighted median.
#' 
#' Computes a weighted median of a numeric vector.
#' 
#' @param x a numeric vector containing the values whose weighted median is to be computed.
#' @param w a vector of weights the same length as x giving the weights to use for each element of x. Negative weights are treated as zero weights. Default value is equal weight to all values.#
#' @param na.rm a logical value indicating whether NA values in x should be stripped before the computation proceeds, or not. If NA, no check at all for NAs is done. Default value is NA (for efficiency).
#' @param interpolate If TRUE, linear interpolation is used to get a consistent estimate of the weighted median.
#' @param ties If interpolate == FALSE, a character string specifying how to solve ties between two x's that are satisfying the weighted median criteria. Note that at most two values can satisfy the criteria. When ties is "min", the smaller value of the two is returned and when it is "max", the larger value is returned. If ties is "mean", the mean of the two values is returned and if it is "both", both values are returned. Finally, if ties is "weighted" (or NULL) a weighted average of the two are returned, where the weights are weights of all values x[i] <= x[k] and x[i] >= x[k], respectively.  
#' @param method If "shell", then order() is used and when method="quick", then internal qsort() is used. 
#' @param ... Not used.
#' @note This function is a duplicate from \code{\link[aroma.light]{weightedMedian}} in the aroma.light package
#' @family "central tendency functions"
#' @family stats helper functions
#' @seealso \code{\link{as.surveyorStats}}
#' @export 
weightedMedian <- function (x, w, na.rm = TRUE, interpolate = is.null(ties), ties = NULL, 
    method = c("quick", "shell"), ...) 
{
  if (missing(w)) {
    w <- rep(1, times = length(x))
  }
  naValue <- NA
  storage.mode(naValue) <- storage.mode(x)
  if (is.na(na.rm)) {
  }
  else if (identical(na.rm, TRUE)) {
    tmp <- !(is.na(x) | is.na(w))
    x <- .subset(x, tmp)
    w <- .subset(w, tmp)
  }
  else if (any(is.na(x))) {
    return(naValue)
  }
  n <- length(w)
  tmp <- (w > 0)
  if (!all(tmp)) {
    x <- .subset(x, tmp)
    w <- .subset(w, tmp)
    n <- sum(tmp)
  }
  if (n == 0) {
    return(naValue)
  }
  else if (n == 1) {
    return(x)
  }
  tmp <- is.infinite(w)
  if (any(tmp)) {
    x <- .subset(x, tmp)
    n <- length(x)
    half <- (n + 1)/2
    if (n%%2 == 1) {
      #return(.Internal(psort(x, half))[half])
      return(sort(x, partial=half)[half])
    }
    else {
      partial <- c(half, half + 1)
      #return(sum(.Internal(psort(x, partial))[partial])/2)
      return(sum(sort(x, partial=partial)[partial])/2)
    }
  }
  if (identical(method, "quick")) {
    #l <- .Internal(qsort(x, TRUE))
    l <- sort.int(x, method="quick", index.return=TRUE)
    x <- .subset2(l, 1)
    w <- .subset(w, .subset2(l, 2))
  }
  else {
    #ord <- .Internal(order(TRUE, FALSE, x))
    ord <- order(x)
    x <- .subset(x, ord)
    w <- .subset(w, ord)
  }
  wcum <- cumsum(w)
  wsum <- .subset(wcum, n)
  wmid <- wsum/2
  if (interpolate == TRUE) {
    wcum <- wcum - (w/2)
    if (n < 1e+05) {
      lows <- (wcum < wmid)
      k <- max(1, sum(lows))
    }
    else {
      k0 <- 1
      k1 <- n
      while ((dk <- (k1 - k0)) > 1) {
        k <- k0 + dk%/%2
        if (.subset(wcum, k) < wmid) 
          k0 <- k
        else k1 <- k
      }
      k <- k0
    }
    Dx <- .subset(x, k + 1) - .subset(x, k)
    Dy <- .subset(wcum, k + 1) - .subset(wcum, k)
    dy <- wmid - .subset(wcum, k)
    dx <- (dy/Dy) * Dx
    xm <- dx + .subset(x, k)
    return(xm)
  }
  lows <- (wcum <= wmid)
  k <- sum(lows)
  if (k == 0) 
    return(.subset(x, 1))
  if (k == n) 
    return(.subset(x, n))
  wlow <- .subset(wcum, k)
  whigh <- wsum - wlow
  if (whigh > wmid) 
    return(.subset(x, k + 1))
  if (is.null(ties) || ties == "weighted") {
    (wlow * .subset(x, k) + whigh * .subset(x, k + 1))/wsum
  }
  else if (ties == "max") {
    .subset(x, k + 1)
  }
  else if (ties == "min") {
    .subset(x, k)
  }
  else if (ties == "mean") {
    (.subset(x, k) + .subset(x, k + 1))/2
  }
  else if (ties == "both") {
    .subset(x, k, k + 1)
  }
}
