# 
# Author: andrie
###############################################################################


#' Plot data in net score format (bar chart, but percentage axis).
#'
#' @param s A surveyorStats object
#' @param plotFunction Character vector: Identifies the name of the plot function used to create the plot
#' @param width Question text gets line-wrapped at width
#' @param ... Ignored
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @family plotFunctions
#' @keywords plot
#' @export
plotNetScore <- function(s, plotFunction="plotNetScore", width=50, ...){
  
  cbreak <- NULL # Trick to fool R CMD check
  
  stopifnot(is.surveyorStats(s))
  if(is.null(s$data$question)) s$data$question <- 1
  s <- formatLabels(s)
  f <- s$data
  f$question <- str_wrap(f$question, width=width)
  
  f$hjust <- cutJust(f$value, breaks=seq(-1, 1, 0.5), newValues=c(-0.1, 1.1, -0.1, 1.1))
  
  if(!s$surveyorDefaults$fastgraphics){
    p <- ggplot(f, aes_string(x="question", y="value")) +
        theme_surveyor(s$surveyorDefaults$defaultThemeSize) +
        geom_bar(
            aes_string(fill="factor(cbreak)"),
            stat="identity", 
            position="identity", 
            width=0.8) +
        geom_text(
            aes_string(label="round(value*100, 0)",
                hjust="hjust"),
            size=s$surveyorDefaults$defaultThemeSize * 0.25) +
        coord_flip(ylim=c(-1,1)) +
        opts(
            legend.position="none",
            axis.title.y = theme_blank()
        ) +
        scale_y_continuous(
            s$ylabel, 
            labels = scales::percent, 
            breaks = c(-1, 0, 1)
        ) +
        facet_grid(~cbreak)
    
    nColours <- nlevels(factor(f$cbreak)) + any(is.na(f$cbreak))
    fillColours <- plotColours(s, colours=nColours, ...)
    p <- p + scale_fill_manual(values=fillColours)
    
    if (length(unique(f$cbreak)) > 3){
      p <- p + opts(axis.text.x = theme_blank())
    } else {
      p <- p + opts(axis.text.x = theme_text(
              size=s$surveyorDefaults$defaultThemeSize * 10/14,
              angle = 90, hjust = 1, vjust=c(1, 0.5, 0))) 
    }
  }
  
  if(s$surveyorDefaults$fastgraphics){
    #trellis.par.set(ggplot2like(n = 4, h.start = 180))
    qlayout <- c(ifelse(is.factor(f$cbreak), nlevels(f$cbreak), length(unique(f$cbreak))), 1)
    q <- switch(qType(s),
        singleQ_singleResponse = 
            lattice::barchart(response~value|cbreak, 
                f, layout=qlayout, groups=cbreak, origin=0, horizontal=TRUE, stack=TRUE),
        gridQ_singleResponse =
            lattice::barchart(question~value|cbreak,
                f, layout=qlayout, groups=cbreak, origin=0, horizontal=TRUE, stack=TRUE),
        stop("plotBar: Invalid value of qType.  This should never happen")
    )
    q <- update(q, 
        par.settings=modifyList(
            latticeExtra::ggplot2like(n = 4, h.start = 180),
            list(
                fontsize=list(text=s$surveyorDefaults$defaultThemeSize),
                axis.text = list(col = "black")
            )
        ),
        between=list(x=0.5),
        col=plotColours(s, colours=length(q$panel.args), ...),
        xlab=s$ylabel,
        scales=list(
            x=list(
                limits=c(-1.1, 1.1),
                labels=c(NA, "-100%", "-50%", "0", "50%", "100%", NA),
                draw=FALSE
            )
        ),
        panel=function(x, y, ...){
          args=list(...)
          strip.custom(bg="grey80") 
          panel.fill(col="grey90", lwd=0)
          panel.grid(col="white", h=length(unique(y))-1) 
          panel.barchart(x, y, ...)
          llines(unit(rep(0, 2), "native"), c(0, 100), col="grey50")
          for (i in seq_along(x)){
            ltext(
                x[i], y[i], 
                labels=formatPercent(x[i], digits=0), 
                adj=c(
                    cutJust(x[i], breaks=seq(-1, 1, 0.5), newValues=c(-0.1, 1.1, -0.1, 1.1)),
                    0.5)
            )
          }
        })
  }
  
  ifelse(s$surveyorDefaults$fastgraphics, 
      return(as.surveyorPlot(q, s, plotFunction="plotNetScore", ...)), 
      return(as.surveyorPlot(p, s, plotFunction="plotNetScore", ...))
  )
}

