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
all_na <- function(x){
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
#' @keywords internal
all_null <- function(x){
  if (inherits(x, "list") || inherits(x, "data.frame")){
    return(all(as.logical(sapply(x, function(y) all(is.null(y))))))
  }
  return(all(is.null(x)))
}

#' Determines question type as single/grid question and single/multi response.
#' 
#' @param s A surveyorStats object
#' @keywords internal
qType <- function(s){
  stopifnot(is.surveyorStats(s))
  if(is.null(s$data)) return(NULL)
  if (is.null(s$data$question)){
    x <- ifelse (is.null(s$data$response), "singleQ_singleResponse", "singleQ_multiResponse")
  } else {
    x <- ifelse(is.null(s$data$response), "gridQ_singleResponse", "gridQ_multiResponse")
  }
  if(x=="gridQ_multiResponse" && is.yesno(s)) x <- "gridQ_singleResponse"
  x
}


#' Checks if question has response of yes/no.
#' 
#' @param s A surveyorStats object
#' @keywords internal
is.yesno <- function(s){
  stopifnot(is.surveyorStats(s))
  ifelse(!is.null(s$data$response), x <- s$data$response, return(FALSE))  
  ret <- FALSE
  if(is.factor(x)){
    if(length(levels(x))==2){
      if(sort(levels(x))==c("No", "Yes") || sort(levels(x))==c("Not selected", "Yes")){
        ret <- TRUE
      }}}
  ret
}

#' Calculated weighted median
#' 
#' Computes a weighted median of a numeric vector. This function is a duplicate from \code{\link[aroma.light]{weightedMedian}} in the aroma.light package
#' 
#' @param x a numeric vector containing the values whose weighted median is to be computed.
#' @param w a vector of weights the same length as x giving the weights to use for each element of x. Negative weights are treated as zero weights. Default value is equal weight to all values.#
#' @param na.rm a logical value indicating whether NA values in x should be stripped before the computation proceeds, or not. If NA, no check at all for NAs is done. Default value is NA (for efficiency).
#' @param interpolate If TRUE, linear interpolation is used to get a consistent estimate of the weighted median.
#' @param ties If interpolate == FALSE, a character string specifying how to solve ties between two x's that are satisfying the weighted median criteria. Note that at most two values can satisfy the criteria. When ties is "min", the smaller value of the two is returned and when it is "max", the larger value is returned. If ties is "mean", the mean of the two values is returned and if it is "both", both values are returned. Finally, if ties is "weighted" (or NULL) a weighted average of the two are returned, where the weights are weights of all values x[i] <= x[k] and x[i] >= x[k], respectively.  
#' @param method If "shell", then order() is used and when method="quick", then internal qsort() is used. 
#' @param ... Not used.
#' @keywords Internal
weightedMedian <- function (x, w, na.rm = NA, interpolate = is.null(ties), ties = NULL, 
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
      return(.Internal(psort(x, half))[half])
    }
    else {
      partial <- c(half, half + 1)
      return(sum(.Internal(psort(x, partial))[partial])/2)
    }
  }
  if (identical(method, "quick")) {
    l <- .Internal(qsort(x, TRUE))
    x <- .subset2(l, 1)
    w <- .subset(w, .subset2(l, 2))
  }
  else {
    ord <- .Internal(order(TRUE, FALSE, x))
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
