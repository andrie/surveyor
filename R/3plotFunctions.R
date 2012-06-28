


#' Creates surveyPlot object and adds plot title.
#'  
#' Creates surveyPlot object, a container for either ggplot or lattice graphic. 
#' 
#' @param plot A ggplot or lattice object 
#' @param expansion Multiplier for plot vertical dimension
#' @param plotFunction The plot function that was used to create the plot
#' @return A surveyPlot object
#' @keywords internal
as.surveyPlot <- function(
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
      class = "surveyPlot"
  )
}


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


#' print method for surveyPlot object.
#' 
#' @param x plot to display
#' @param ... other arguments not used by this method 
#' @method print surveyPlot
print.surveyPlot <- function(x, ...) print(x$plot)


#' Test object for membership of class "surveyPlot".
#'  
#' Test object for membership of class "surveyPlot".
#' 
#' @param x Object 
#' @return TRUE or FALSE
#' @keywords internal
is.surveyPlot <- function(x){
  inherits(x, "surveyPlot")
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

# Apply Brewer pallete colours to plot
plotColours <- function(s, colours=3, 
    set=s$surveyorDefaults$brewerPalette, reverse=s$surveyorDefaults$revBrewerPal){
  cols <- brewer.pal(max(3, colours), set)
  colours <- seq_len(colours)
  if(reverse) colours <- rev(colours)
  #message(paste(cols, collapse=" - "))
  cols[colours]
}


#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------

# TODO: create lattice equivalents for plotPoint
# TODO: create plotPoint charts for each qType

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
								colour=factor(cbreak), fill=factor(cbreak)))
	} else {
	# Plot array question 
    if(is.null(f$response)) {
      p <- ggplot(f, aes(x=question, y=value,  
              colour=factor(cbreak), fill=factor(cbreak)))
    } else {
    	p <- ggplot(f, aes(x=question, y=response, size=value, 
  							colour=factor(cbreak), fill=factor(cbreak)))
    }
	}
  labeldata <- within(f, label <- formatValues(f$value, formatter))
	p <- p + 
			theme_surveyor(s$surveyorDefaults$defaultThemeSize) +
			geom_point(stat="sum") +
			geom_text(data=labeldata, aes_string(label="label"), size=3, vjust=2) +
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
	
	as.surveyPlot(p, s, plotFunction=plotFunction, ...)
}

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
  as.surveyPlot(p, s, plotFunction=plotFunction, ...)
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
  					size=3) +
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
  			
  	if (length(unique(f$cbreak)) > 1){
  		p <- p + opts(axis.text.x = theme_blank())
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
        col=plotColours(s, colours=length(q$panel.args)),
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
      return(as.surveyPlot(q, s, plotFunction="plotNetScore", ...)), 
      return(as.surveyPlot(p, s, plotFunction="plotNetScore", ...))
  )
}

