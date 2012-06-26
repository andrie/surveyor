basic.column.lattice <- function(f, qType){
  question <- response <- value <- cbreak <- NULL  # Dummy to trick R CMD check
  qlayout <- c(ifelse(is.factor(f$cbreak), nlevels(f$cbreak), length(unique(f$cbreak))), 1)
  q <- switch(qType,
      singleQ_singleResponse = {
          #qlayout <- c(1, 1)
          lattice::barchart(value~rep("1", nlevels(f$cbreak)) | cbreak, 
              f, layout=qlayout,  box.ratio=1.5, groups=value, origin=0, stack=TRUE)
        },
      singleQ_multiResponse = 
          lattice::barchart(value~factor(response)|cbreak, 
              f, layout=qlayout, box.ratio=1.5, origin=0, groups=value, stack=TRUE),
      gridQ_singleResponse = 
          lattice::barchart(value~question|cbreak, 
              f, layout=qlayout,  box.ratio=1.5, origin=0, groups=cbreak, stack=TRUE),
      gridQ_multiResponse = 
          lattice::barchart(value~question|cbreak, 
              f, layout=qlayout,  box.ratio=1.5, origin=0, groups=response, stack=TRUE, 
              auto.key=list(space="right")),
      stop("plotColumn: Invalid value of qType.  This should never happen")
  )
  q
}


basic.column.ggplot <- function(f, qType){
  p <- switch(qType,
    singleQ_singleResponse =
        ggplot(f, aes_string(x="factor(cbreak)", y="value", fill="factor(value)")),
    singleQ_multiResponse =
        ggplot(f, aes_string(x="factor(cbreak)", y="value", fill="factor(value)")),
    gridQ_singleResponse = 
        ggplot(f, aes_string(x="factor(cbreak)", y="value", fill="factor(value)")),
    gridQ_multiResponse = 
        ggplot(f, aes_string(x="factor(cbreak)", y="value", fill="factor(value)")),
    stop("plotBar: Invalid value of qType.  This should never happen")
  )    
  p
}


#' Plot data in column chart format
#'
#' @param s A surveyorStats object
#' @param plotFunction Character vector: Identifies the name of the plot function used to create the plot
#' @param ... Ignored
#' @seealso 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @family plotFunctions
#' @keywords plot
#' @export
plotColumn <- function(s, plotFunction="plotColumn", ...){
  stopifnot(is.surveyorStats(s))
  s <- formatLabels(s)
	f <- s$data
  qType <- qType(s)
  
	f$cbreak <- f$cbreak[drop=TRUE]
	
  if(s$surveyorDefaults$fastgraphics){
    # plot using lattice
    #trellis.par.set(ggplot2like(n = 4, h.start = 180))
    q <- basic.column.lattice(f, qType)
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
        ylab=s$ylabel,
        horizontal=FALSE,
        panel=function(x, y, ...){
          args <- list(...)
          strip.custom(bg="grey80") 
          panel.fill(col="grey90", lwd=0)
          panel.grid(col="white", h=5)
          panel.barchart(x, y, ...)
          if(qType %in% c("singleQ_singleResponse", "singleQ_multiResponse", "gridQ_singleResponse") || is.yesno(s)){
            latticeLabels(x, y, just=0.5, horizontal=args$horizontal, 
                stack=args$stack, formatter=s$formatter)
          }
        })
  }
  
  if(!s$surveyorDefaults$fastgraphics){
    ### Set up basic ggplot graphic ###
    p <- basic.column.ggplot(f, qType)
    p <- p + geom_bar(stat="identity")
    ### Add plot options ###
    p <- p + 
        theme_surveyor(s$surveyorDefaults$defaultThemeSize) +
        #coord_flip() + 
        scale_y_continuous(
            s$ylabel, 
            #formatter=s$formatter,
            labels=match.fun(s$formatter),
            breaks=s$scale_breaks) +
        opts(
            legend.position="none",
            axis.title.y = theme_blank()
        ) +
        labs(fill="Response")
    
    ### Add labels ###
    if(qType %in% c("singleQ_multiResponse", "gridQ_singleResponse"))
      p <- p + geom_text(aes_string(label="labelsValue", vjust="labelsJust"), size=3)
		
    ### Add legend for multiple response ### 
    if(qType=="gridQ_multiResponse"  && !is.yesno(s))
      p <- p + opts(legend.position="right")
    
    ### Deal with too many colours ###
    if(length(unique(f$response)) > 8) p <- p + scale_fill_hue()
    if (qType == "singleQ_multiResponse"){
      p <- p + opts(
          axis.text.x=theme_text(size=s$surveyorDefaults$defaultThemeSize*0.9, angle=0)
      )
    }  
    if (qType == "gridQ_multiResponse"){
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


