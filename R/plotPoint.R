# 
# Author: andrie
###############################################################################


# TODO: create lattice equivalents for plotPoint

#' Plot data in bubble chart format
#'
#' @param s A surveyorStats object
#' @param plotFunction Character vector: Identifies the name of the plot function used to create the plot
#' @param formatter A formatting function used to format labels
#' @param ... Ignored
#' @seealso 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @family plotFunctions
#' @keywords plot
#' @export
plotPoint <- function(s, plotFunction="plotPoint", formatter="formatPercent", ...){
  stopifnot(is.surveyorStats(s))
  question <- response <- value <- cbreak <- NULL  # Dummy to trick R CMD check
  
  f <- s$data
  if (is.null(f$question)){
    # Plot single question
    p <- ggplot(f, aes(x=" ", y=response, size=value, 
            colour=factor(cbreak)))
  } else {
    # Plot array question 
    if(is.null(f$response)) {
      p <- ggplot(f, aes(x=question, y=value,  
              colour=factor(cbreak)))
    } else {
      p <- ggplot(f, aes(x=question, y=response, size=value, 
              colour=factor(cbreak)))
    }
  }
  labeldata <- within(f, label <- formatValues(f$value, formatter))
  p <- p + 
      theme_surveyor(s$surveyorDefaults$defaultThemeSize) +
      geom_point(stat="sum") +
      geom_text(data=labeldata, aes_string(label="label"), 
          size=s$surveyorDefaults$defaultThemeSize / 12 * 4, 
          vjust=2, colour="black") +
      coord_flip() + 
      quiet +
      ylab(s$ylabel) +
      facet_grid(.~cbreak) +
      opts(
          panel.grid.minor = theme_blank(), 
          axis.text.x = theme_text(
              size=s$surveyorDefaults$defaultThemeSize, 
              angle=90, 
              hjust=1)
      )
  
  nColours <- nlevels(factor(f$cbreak)) + any(is.na(f$cbreak))
  fillColours <- plotColours(s, colours=nColours, ...)
  p <- p + scale_colour_manual(values=fillColours)
  
  as.surveyorPlot(p, s, plotFunction=plotFunction, ...)
}

