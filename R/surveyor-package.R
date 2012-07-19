# package documentation
# 
# Author: Andrie
#-------------------------------------------------------------------------------


#' Tools for analysing and reporting on survey objects.
#'
#' This is a collection of functions to make it easier to analyse and report
#' on market survey data.
#' 
#' \itemize{
#' \item Initialise data and create surveyor object: \code{\link{as.surveyor}} 
#' \item Define default settings : \code{\link{surveyorDefaults}}
#' }
#' 
#' Plot survey question:
#' \itemize{ 
#' \item \code{\link{surveyPlot}} 
#' }
#' 
#' Coding functions:
#' \itemize{
#' \item \code{\link{codeGuess}}
#' \item \code{\link{codeQuickArray}}
#' }
#' 
#' Summarising functions:
#' \itemize{
#' \item \code{\link{statsGuess}} 
#' \item \code{\link{statsBin}} 
#' \item \code{\link{statsBinPercent}} 
#' \item \code{\link{statsRank}} 
#' \item \code{\link{statsNetScore}}
#' \item \code{\link{statsSum}} 
#' }
#' 
#' Plot functions: 
#' \itemize{
#' \item \code{\link{plotGuess}} 
#' \item \code{\link{plotBar}} 
#' \item \code{\link{plotColumn}} 
#' \item \code{\link{plotPoint}} 
#' \item \code{\link{plotText}} 
#' \item \code{\link{plotNetScore}} 
#' }
#' 
#' @name surveyor-package
#' @aliases surveyor surveyor-package
#' @docType package
#' @import  xtable lattice data.table ggplot2 RColorBrewer grid
#' @importFrom latticeExtra ggplot2like
#' @importFrom Hmisc latexTranslate
#' @importFrom plyr quickdf summarise ddply round_any
#' @importFrom stringr str_wrap
#' @title Tools for analysing and reporting on survey objects.
#' @author Andrie de Vries \email{andrie.de.vries@@pentalibra.com}
#' @keywords package
#' @seealso \code{\link{as.surveyor}}

NULL

