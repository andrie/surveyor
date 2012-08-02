


#' Creates surveyorPlot object and adds plot title.
#'  
#' Creates \code{surveyorPlot} object, a container for either ggplot or lattice graphic. The method \code{print.surveyorPlot} knows how to print the final plot object 
#' 
#' @param plot A \code{ggplot} or \code{lattice} object
#' @param surveyorStats An \code{\link{as.surveyorStats}} object
#' @param expansion Multiplier for plot vertical dimension
#' @param plotFunction String. The plot function that was used to create the plot. This is used purely to keep an audit trail of how the final output was created.
#' @param plotSize Numeric vector of length 2, specifying the width and height of the plot (in inches)
#' @param addPlotTitle If TRUE, adds question text as plot title, otherwise the plot has no title
#' @param ... Ignored
#' @return A surveyorPlot object. This is a list of:
#' \describe{
#' \item{plot}{A \code{ggplot} or \code{lattice} object}
#' \item{expansion}{Expansion factor. Used to adjust the vertical scale of the plot when there are many categories}
#' \item{plotFunction}{String indicating which plot function created the plot. Useful for debugging}
#' \item{qType}{See also \code{\link{qType}}}
#' \item{qid}{Question identifier, e.g. "Q4"}
#' \item{data}{Data used in the plot}
#' \item{nquestion}{}
#' \item{formatter}{Formatting function for axis, e.g. \code{\link{formatRound}}}
#' }
#' @export
#' @seealso \code{\link{as.surveyorStats}}, \code{\link{plotGuess}}
#' @seealso \code{\link{surveyPlot}} to plot a surveyor object
#' @family surveyPlot arguments
as.surveyorPlot <- function(
    plot,
    surveyorStats,
    expansion = 1,
    plotFunction ="",
    plotSize  = par("din"),
    addPlotTitle = surveyorStats$surveyorDefaults$addPlotTitle,
    ...
){
  stopifnot(is.surveyorStats(surveyorStats))
  ### Adds plot title ###
  args <- list(...)
  plotTitle <- surveyorStats$plotTitle
  plotTitle <- strwrap(
      plotTitle, 
      width=0.8 * nchar(plotTitle) * plotSize[1] / strwidth(plotTitle, units="inches")
  )
  plotTitle <- paste(plotTitle, collapse="\n")
  if(addPlotTitle){
    if(inherits(plot, "ggplot")){
      plot <- plot + opts(title=plotTitle)
      class(plot) <- c("ggplotmod", "ggplot")
    }
    if(inherits(plot, "trellis")){
      plot <- update(plot, main=plotTitle)
    }
  }
  
  ### Create list ###
  structure(
      list(
          plot  = plot,
          expansion    = expansion,
          plotFunction = plotFunction,
          qType = qType(surveyorStats),
          qid   = surveyorStats$qid,
          data  = surveyorStats$data,
          nquestion    = surveyorStats$nquestion,
          formatter    = surveyorStats$formatter
      ),
      class = "surveyorPlot"
  )
}

#' Display structure of surveyorPlot object.
#' 
#' Structure is displayed to maximum of 2 levels
#' 
#' @param x surveyorPlot object
#' @param max.level Passed to argument \code{max.level} in \code{\link{str}}. Defaults to 2, to limit the amount of information returned by \code{ggplot} and \code{lattice} 
#' @param ... Other arguments passed to \code{\link{str}}
#' @seealso \code{\link{str}}
str.surveyorPlot <- function(x, max.level=2, ...) NextMethod("str", max.level=max.level, ...)

#' modified print method for ggplot to align title to plot instead of plotting grid.
#' 
#' @method print ggplotmod
#' @param x plot to display
#' @param newpage draw new (empty) page first?
#' @param vp viewport to draw plot in
#' @param ... other arguments not used by this method 
#' @export
print.ggplotmod <- function (x, newpage = is.null(vp), vp = NULL, ...){
  ggplot2:::set_last_plot(x)
  if (newpage) 
    grid.newpage()
  data <- ggplot_build(x)
  gtable <- ggplot_gtable(data)
  gtable$layout[which(gtable$layout$name == "title"), c("l", "r")] <- c(1, max(gtable$layout$r))
  if (is.null(vp)) {
    grid.draw(gtable)
  }
  else {
    if (is.character(vp)) 
      seekViewport(vp)
    else pushViewport(vp)
    grid.draw(gtable)
    upViewport()
  }
  invisible(data)
}


#' print method for surveyorPlot object.
#' 
#' @param x plot to display
#' @param ... other arguments not used by this method 
#' @method print surveyorPlot
print.surveyorPlot <- function(x, ...) print(x$plot)


#' Test object for membership of class "surveyorPlot".
#'  
#' Test object for membership of class "surveyorPlot".
#' 
#' @param x Object 
#' @return TRUE or FALSE
#' @keywords internal
is.surveyorPlot <- function(x){
  inherits(x, "surveyorPlot")
}



#' Guesses which plot format is optimal
#' 
#' Investigates columns in supplied data, and then chooses either \code{\link{plotBar}} or \code{\link{plotColumn}}
#'
#' @param s A surveyorStats object
#' @param ... Other parameters passed to specific plot function
#' @seealso 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @family plotFunctions
#' @keywords plot
#' @export
plotGuess <- function(s, ...){
  stopifnot(is.surveyorStats(s))
  f <- s$data
  if(s$plotFunction != ""){
    if(is.function(match.fun(s$plotFunction))){
#    if(s$plotFunction=="plotNetScore") {
#      plotNetScore(s, surveyor)
    match.fun(s$plotFunction)(s, ...)
    } else {
      stop(paste("Plot function specified in surveyorStats not found, plotFunction =", s$plotFunction))
    }  
  } else {
    if (is.null(f$question)){
      # Plot single question
      if (is.null(f$response)) {
        plotColumn(s, ...)
      } else {  
        plotBar(s, ...)
      }  
      
    } else {
      if (is.null(f$response)) {
        # Plot array of single values per question
        plotBar(s, ...)
      } else {
        # Plot array question as stacked bar
        plotBar(s, ...)
      }
    }
  }
}


#' Create lattice labels.
#' 
#' @keywords internal
latticeLabels <- function(x, y, just=0.5, horizontal=TRUE, stack=TRUE, formatter="format"){
  formatter <- match.fun(formatter)
  if(horizontal){
    labels <- formatter(x)
    hjust <- ifelse(x < mean(x), 0, 1)
    vjust <- 0.5
    #if(stack) x <- do.call(c, unname(lapply(split(x, y), function(t)cumsum(t)-t*(1-just))))
  } else {
    labels <- formatter(y)
    vjust <- ifelse(y < mean(y), 0, 1)
    hjust <- 0.5
    #if(stack) y <- do.call(c, unname(lapply(split(y, x), function(t)cumsum(t)-t*(1-just))))
  }
  for (i in seq_along(x)){
    ltext(x[i], y[i], labels=labels[i], adj =c(hjust[i], vjust[i]))
  }
  
}

#' Apply Brewer pallete colours to plot
#' 
#' @keywords internal
plotColours <- function(s, colours=3, 
    brewerPalette=s$surveyorDefaults$brewerPalette, revBrewerPal=s$surveyorDefaults$revBrewerPal, ...){
  cols <- brewer.pal(max(3, colours), brewerPalette)
  colours <- seq_len(colours)
  if(revBrewerPal) colours <- rev(colours)
  #message(paste(cols, collapse=" - "))
  cols[colours]
}


#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------


#' Plot data as text.
#' 
#' Plots questions that are summarised using \code{\link{statsText}}.
#'
#' @param s A surveyorStats object
#' @param plotFunction Character vector: Identifies the name of the plot function used to create the plot
#' @param ... Ignored
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @family plotFunctions
#' @keywords plot
#' @export
plotText <- function(s, plotFunction="plotText", ...){
  stopifnot(is.surveyorStats(s))
  ### Only print if cbreak equal to first crossbreak in surveyor
#  print(is.list(surveyor$crossbreak))
#  print(all(s$data$cbreak!=surveyor$crossbreak[[1]]))
#  p <- ""
#  flag <- FALSE
#  if(is.list(s$surveyor$crossbreak)){
#    if(identical(s$surveyor$cbreak, s$surveyor$crossbreak[[1]])) flag <- TRUE
#  }
#  
#  if(!is.list(s$surveyor$crossbreak)) flag <- TRUE
#  
#  if(flag){
    #if(s$data$cbreak != surveyor$crossbreak) return(NULL)
    ### Carry on as usual
    unique_resp <- unique(s$data$response)
    unique_resp <- levels(s$data$response)
    items <- paste("\\item", Hmisc:::latexTranslate(unique_resp))
    items <- paste(items, collapse="\n")
#  }
  p <- paste("\\begin{itemize}", items, "\\end{itemize}\\n", collapse="\\n")
  class(p) <- "text"
  as.surveyorPlot(p, s, plotFunction=plotFunction, ...)
}

#-------------------------------------------------------------------------------

#' Cuts values and assign new values to each break.
#' 
#' This is useful for creating hjust and vjust positions for text on plots.  this is essentially a wrapper around \code{\link{cut}}
#' @param x Vector to cut
#' @param breaks Vector with break points.  Needs to include the min and max of x
#' @param  newValues Vector with new values. Should be length one less than breaks.
cutJust <- function(x, breaks, newValues){
  if(length(unique(breaks)) < length(breaks))
    cx <- 1 else
    cx <- cut(x, breaks=breaks, include.lowest=TRUE)
  newValues[cx]
}



