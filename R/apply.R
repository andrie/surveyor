# 
# Author: andrie
###############################################################################


#' Plots nothing, useful to suppress plot and return data only.
#'
#' @param s A surveyorStats object
#' @param plotFunction Character vector: Identifies the name of the plot function used to create the plot
#' @param ... ignored
#' @seealso 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @family plotFunctions
#' @keywords plot
#' @export
plotNull <- function(s, plotFunction="plotNull", ...){
  p <- NA
  class(p) <- "text"
  as.surveyorPlot(p, s, plotFunction=plotFunction, ...)
}


#' Apply surveyPlot to list of question definitions.
#' 
#' @param banner a list of arguments to send to \code{\link{surveyPlot}}
#' @param s Optional surveyor object. If supplied, appends s to each element of defs
#' @param mlist Options list of modified arguments
#' @family Surveyor apply functions
#' @export 
surveyPlotApply <- function(banner, s, mlist){
  if(!missing(s))
    banner <- lapply(banner, function(xx)modifyList(xx, val=list(x=s)))
  if(!missing(mlist))
    banner <- lapply(banner, function(xx)modifyList(xx, val=mlist))
  #lapply(banner, function(xx)do.call(surveyPlot, xx)[[1]][["plot"]])
  x <- lapply(banner, function(xx)do.call(surveyPlot, xx))
  unlist(x, recursive=FALSE, use.names=FALSE)
}

#' Wrapper around surveyPlot that suppresses plot creation and returns data only.
#' 
#' @inheritParams surveyPlot
#' @export 
surveyTable <- function (x, qid, statsFunction = "statsGuess", plotFunction=NA, ...){
  surveyPlot(x=x, qid=qid, statsFunction=statsFunction, plotFunction=NA, ...)
} 


#TODO: Add plot titles, formatting, etc.
#TODO: cbind() or merge() multiple crossbreak results


#' Apply surveyTable to list of question definitions.
#' 
#' @param banner a list of arguments to send to \code{\link{surveyPlot}}
#' @param s Optional surveyor object. If supplied, appends s to each element of defs
#' @param mlist Options list of modified arguments
#' @param simplify If TRUE, simplifies results to data frame, otherwise the result is a list
#' @param remove a character vector of stats_methods to remove from data
#' @param allBreaks If TRUE, sets all \code{onlyBreaks} arguments to NULL, thus creating surveyTable for all crossbreaks in surveyor
#' @return A data frame (if simplified=TRUE), otherwise a list
#' @family Surveyor apply functions
#' @export 
surveyTableApply <- function(banner, s, mlist, simplify=TRUE, remove=c("statsText"), allBreaks=TRUE){
  if(!missing(s))
     banner <- lapply(banner, modifyList, val=list(x=s))
  if(!missing(mlist))
    banner <- lapply(banner, modifyList, val=mlist)
  if(allBreaks)
    banner <- lapply(banner, modifyList, val=list(onlyBreaks=NULL))
  ret <- lapply(banner, function(xx)do.call(surveyTable, xx))
  toRemove <- sapply(seq_along(ret), 
      function(i)ifelse(ret[[i]][[1]]$stats_method=="statsText", TRUE, FALSE))
  ret <- ret[!toRemove]
  if(simplify){
    message("makeTable: combining results")
    xx <- lapply(seq_along(ret), function(i) {
          z <- ret[[i]][[1]]$data;
          cbind(q=names(ret)[i], tcast(z))
        } )
    ret <- do.call(rbind, xx)
  }
  ret
}

#' Cast list of crosstab data into data frame.
#' 
#' @param x The data element from a \code{\link{surveyTable}} call
#' @seealso makeTable
#' @keywords internal
tcast <- function(x){
  qr <- c("question", "response")
  if("question" %in% names(x)){
    if("response" %in% names(x)){
      dcast(x, question + response ~ cbreak)
    } else {
      z <- dcast(x, question ~ cbreak)
      z$response <- NA
      z[, c(qr, setdiff(names(z), qr))]
    }
  } else {
    if("response" %in% names(x)){
      z <- dcast(x, response ~ cbreak)
      z$question <- NA
      z[, c(qr, setdiff(names(z), qr))]
    } else {
      z <- dcast(x, 0~cbreak)
      z[1] <- NULL
      z$question <- NA
      z$response <- NA
      z[, c(qr, setdiff(names(z), qr))]
    }
  }
}


