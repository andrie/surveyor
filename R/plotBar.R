basic.bar.lattice <- function(f, qType){
  question <- response <- value <- cbreak <- NULL  # Dummy to trick R CMD check
  qlayout <- c(ifelse(is.factor(f$cbreak), nlevels(f$cbreak), length(unique(f$cbreak))), 1)
  q <- switch(qType,
      singleQ_singleResponse = { 
          qlayout <- c(1, 1)
          lattice::barchart(cbreak~value,
              f, layout=qlayout,  box.ratio=1.5, origin=0, groups=cbreak, stack=FALSE)   ####<<<<<<<<<<<
            }, 
      singleQ_multiResponse = 
          lattice::barchart(factor(response)~value|cbreak, 
              f, layout=qlayout, box.ratio=1.5, origin=0, groups=cbreak, stack=TRUE),
      gridQ_singleResponse = 
          lattice::barchart(question~value|cbreak, 
              f, layout=qlayout,  box.ratio=1.5, origin=0, groups=cbreak, stack=TRUE),
      gridQ_multiResponse = 
          lattice::barchart(question~value|cbreak, 
              f, layout=qlayout,  box.ratio=1.5, origin=0, groups=cbreak, stack=TRUE, 
              auto.key=list(space="right")),
      stop("plotBar: Invalid value of qType.  This should never happen")
  )
  q
}

basic.bar.ggplot <- function(f, qType){
  p <- switch(qType,
      singleQ_singleResponse =
          ggplot(f, aes_string(x="1", y="value", fill="factor(cbreak)")),
      singleQ_multiResponse =
          ggplot(f, aes_string(x="response", y="value", fill="factor(cbreak)")),
      gridQ_singleResponse = 
          ggplot(f, aes_string(x="question", y="value", fill="factor(cbreak)")),
      gridQ_multiResponse = 
          ggplot(f, aes_string(x="question", y="value", fill="factor(response)")),
      stop("plotBar: Invalid value of qType.  This should never happen")
  )    
  p
}


#' Plot data in bar chart format
#'
#' @param s A surveyorStats object
#' @param plotFunction Character vector: Identifies the name of the plot function used to create the plot
#' @param ... ignored
#' @seealso 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @family plotFunctions
#' @keywords plot
#' @export
plotBar <- function(s, plotFunction="plotBar", ...){
  stopifnot(is.surveyorStats(s))
  s <- formatLabels(s)
	f <- s$data
  qType <- qType(s)
  #print(str(f))
		
	f$cbreak <- f$cbreak[drop=TRUE]
	
	if(s$surveyorDefaults$fastgraphics){
    # plot using lattice
    #trellis.par.set(ggplot2like(n = 4, h.start = 180))
    q <- basic.bar.lattice(f, qType)
    # Plot options 
    q <- update(q, 
        par.settings=modifyList(
            latticeExtra::ggplot2like(n = 4, h.start = 180),
            list(
                fontsize=list(text=s$surveyorDefaults$defaultThemeSize),
                axis.text = list(col = "black")
            )
        ),
        between=list(x=0.5, y=0.5),
        col=plotColours(s, colours=length(q$panel.args)),
        xlab=s$ylabel,
        horizontal=TRUE,
        panel=function(x, y, ...){
          args <- list(...)
          strip.custom(bg="grey80") 
          panel.fill(col="grey90", lwd=0)
          panel.grid(col="white", h=5)
          panel.barchart(x, y, ...)
          if(qType %in% c("singleQ_multiResponse", "gridQ_singleResponse") || is.yesno(s)){
            latticeLabels(x, y, just=0.5, horizontal=args$horizontal, 
                stack=args$stack, formatter=s$formatter)
          }
        })
  }
    
  if(!s$surveyorDefaults$fastgraphics){
    # plot using ggplot 
    p <- basic.bar.ggplot(f, qType)
    p <- p + geom_bar(stat="identity") 
    # Add labels
    if(qType %in% c("singleQ_multiResponse", "gridQ_singleResponse") || is.yesno(s))
      p <- p + geom_text(aes_string(label="labelsValue", hjust="labelsJust"), size=3)
    
    # Plot options 
  p <- p + 
				theme_surveyor(s$surveyorDefaults$defaultThemeSize) +
				coord_flip() + 
				scale_y_continuous(
						s$ylabel, 
						#formatter=s$formatter,
            labels=match.fun(s$formatter),
						breaks=s$scale_breaks) +
				facet_grid(.~cbreak) + 
				opts(
						legend.position="none",
						axis.title.y = theme_blank()
				) +
				labs(fill="Response")
    
    # Add legend for multiple response  
    if(qType=="gridQ_multiResponse"  && !is.yesno(s))
      p <- p + opts(legend.position="right")

    # Deal with too many colours 
    if(length(unique(f$response)) > 8) p <- p + scale_fill_hue()
		if (qType %in% c("singleQ_multiResponse", "gridQ_multiResponse", "gridQ_singleResponse")){
			p <- p + opts(
					axis.text.x=theme_text(size=s$surveyorDefaults$defaultThemeSize*0.5, angle=90)
			)
    }
	}
		
	ifelse(s$surveyorDefaults$fastgraphics,
      return(as.surveyPlot(q, s, plotFunction=plotFunction, ...)), 
      return(as.surveyPlot(p, s, plotFunction=plotFunction, ...))
  )
}


#' Plot data in bar chart format without modifying format.
#' 
#' The standard plotBar() function will plot the data in a stacked bar chart format and apply percentage formatting.  plotBarSum() applies no formatting.
#'
#' @param s A surveyorStats object
#' @param plotFunction Character vector: Identifies the name of the plot function used to create the plot
#' @param ... Ignored
#' @seealso 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @family plotFunctions
#' @keywords plot
#' @export
plotBarSum <- function(s, plotFunction="plotBarSum", ...){
	plotBar(s, plotFunction=plotFunction, ...)
}

