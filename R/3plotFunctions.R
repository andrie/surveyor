


#' Creates surveyorPlot object.
#'  
#' Creates surveyorPlot object, a container for either ggplot or lattice graphic. 
#' 
#' @param plot A ggplot or lattice object 
#' @param expansion Multiplier for plot vertical dimension
#' @param plotFunction The plot function that was used to create the plot
#' @return A surveyorPlot object
#' @keywords internal
as.surveyorPlot <- function(
    plot,
    surveyorStats,
    expansion = 1,
    plotFunction =""
){
  stopifnot(is.surveyorStats(surveyorStats))
  ### Adds plot title ###
  if(surveyorStats$surveyorDefaults$add_plot_title & inherits(plot, "ggplot")){
      plot <- plot + opts(title=surveyorStats$surveyor$plot_title)
    }
  
  ### Create list ###
  structure(
      list(
          plot=plot,
          expansion=expansion,
          plotFunction=plotFunction
      ),
      class = "surveyorPlot"
  )
}

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
    ### plot using lattice
		qlayout <- c(ifelse(is.factor(f$cbreak), nlevels(f$cbreak), length(unique(f$cbreak))), 1)
    q <- switch(qType,
        singleQ_singleResponse = 
            lattice::barchart(value~cbreak, f, layout=qlayout,  box.ratio=1.5, origin=0),
        singleQ_multiResponse = 
            lattice::barchart(response~value|cbreak, f, layout=qlayout,	box.ratio=1.5, origin=0),
        gridQ_singleResponse = 
            lattice::barchart(question~value|cbreak, 
						    f, layout=qlayout,	box.ratio=1.5, origin=0, groups=f$cbreak, stack=TRUE),
        gridQ_multiResponse = 
            lattice::barchart(question~value|cbreak, 
						  f, layout=qlayout,	box.ratio=1.5, origin=0, groups=f$response, stack=TRUE, 
						  auto.key=list(space="right")),
        stop("plotBar: Invalid value of qType.  This should never happen")
    )
  }
    
  if(!s$surveyorDefaults$fastgraphics){
    ### Set up basic ggplot graphic ###
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
    
    p <- p + geom_bar(stat="identity") 
    
    ### Add labels ###
    if(qType %in% c("singleQ_multiResponse", "gridQ_singleResponse") || is.yesno(s))
#      p <- p + geom_text(aes_string(label="signif(value, 3)"), hjust=1, size=3)
      p <- p + geom_text(aes_string(label="labelsValue", hjust="labelsJust"), size=3)
    
    ### Plot options ###
    p <- p + 
				theme_surveyor(s$surveyorDefaults$defaultThemeSize) +
				coord_flip() + 
				scale_y_continuous(
						s$ylabel, 
						formatter=s$formatter,
						breaks=s$scale_breaks) +
				facet_grid(~cbreak) + 
				opts(
						legend.position="none",
						axis.title.y = theme_blank()
				) +
				labs(fill="Response")
    
    ### Add legend for multiple response ### 
    if(qType=="gridQ_multiResponse"  && !is.yesno(s))
      p <- p + opts(legend.position="right")

    ### Deal with too many colours ###
    if(length(unique(f$response)) > 8) p <- p + scale_fill_hue()
		if (qType %in% c("singleQ_multiResponse", "gridQ_multiResponse", "gridQ_singleResponse")){
			p <- p + opts(
					axis.text.x=theme_text(size=s$surveyorDefaults$defaultThemeSize*0.5, angle=90)
			)
    }
	}
		
	ifelse(s$surveyorDefaults$fastgraphics,
      return(as.surveyorPlot(q, s, plotFunction=plotFunction)), 
      return(as.surveyorPlot(p, s, plotFunction=plotFunction))
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

#-------------------------------------------------------------------------------
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
    ### plot using lattice
    qlayout <- c(ifelse(is.factor(f$cbreak), nlevels(f$cbreak), length(unique(f$cbreak))), 1)
    q <- switch(qType,
        singleQ_singleResponse = 
            lattice::barchart(value~cbreak, f, layout=qlayout,  box.ratio=1.5, origin=0),
        singleQ_multiResponse = 
            lattice::barchart(response~value|cbreak, f, layout=qlayout, box.ratio=1.5, origin=0),
        gridQ_singleResponse = 
            lattice::barchart(question~value|cbreak, 
                f, layout=qlayout,  box.ratio=1.5, origin=0, groups=f$cbreak, stack=TRUE),
        gridQ_multiResponse = 
            lattice::barchart(question~value|cbreak, 
                f, layout=qlayout,  box.ratio=1.5, origin=0, groups=f$response, stack=TRUE, 
                auto.key=list(space="right")),
        stop("plotColumn: Invalid value of qType.  This should never happen")
    )
  }
  
  if(!s$surveyorDefaults$fastgraphics){
    ### Set up basic ggplot graphic ###
    p <- switch(qType,
        singleQ_singleResponse =
            ggplot(f, aes_string(x="factor(cbreak)", y="value", fill="factor(cbreak)")),
        singleQ_multiResponse =
            ggplot(f, aes_string(x="factor(cbreak)", y="value", fill="factor(cbreak)")),
        gridQ_singleResponse = 
            ggplot(f, aes_string(x="factor(cbreak)", y="value", fill="factor(cbreak)")),
        gridQ_multiResponse = 
            ggplot(f, aes_string(x="factor(cbreak)", y="value", fill="factor(cbreak)")),
        stop("plotBar: Invalid value of qType.  This should never happen")
    )    
    
    p <- p + geom_bar(stat="identity")
  
    ### Add plot options ###
    p <- p + 
				theme_surveyor(s$surveyorDefaults$defaultThemeSize) +
				scale_y_continuous(
						s$ylabel, 
						formatter=s$formatter) +
				opts(
						legend.position="none",
						axis.title.x = theme_blank(),
						strip.text.y=theme_text(angle=0),
						axis.text.y=theme_blank()
				) +
				labs(fill="Response")

    ### Add labels ###
    if(qType %in% c("singleQ_singleResponse", "singleQ_multiResponse", "gridQ_singleResponse") || 
        is.yesno(s))
#      p <- p + geom_text(aes_string(label="signif(value, 3)"), hjust=1, size=3)
#      p <- p + geom_text(aes_string(label="labelsValue"), vjust=1, size=3)
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
      return(as.surveyorPlot(q, s, plotFunction=plotFunction)), 
      return(as.surveyorPlot(p, s, plotFunction=plotFunction))
  )
}




#-------------------------------------------------------------------------------

#' Plot data in bubble chart format
#'
#' @param s A surveyorStats object
#' @param plotFunction Character vector: Identifies the name of the plot function used to create the plot
#' @param ... Ignored
#' @seealso 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @family plotFunctions
#' @keywords plot
#' @export
plotPoint <- function(s, plotFunction="plotPoint", ...){
  stopifnot(is.surveyorStats(s))
  question <- NULL; rm(question) # Dummy to trick R CMD check
	response <- NULL; rm(response) # Dummy to trick R CMD check
	value <- NULL; rm(value) # Dummy to trick R CMD check
	cbreak <- NULL; rm(cbreak) # Dummy to trick R CMD check

	f <- s$data
	if (is.null(f$question)){
		# Plot single question
		p <- ggplot(f, aes(x=" ", y=response, size=value, 
								colour=factor(cbreak), fill=factor(cbreak)))
	} else {
	# Plot array question 
	p <- ggplot(f, aes(x=question, y=response, size=value, 
							colour=factor(cbreak), fill=factor(cbreak)))
		}
	p <- p + 
			theme_surveyor(s$surveyorDefaults$defaultThemeSize) +
			geom_point(stat="sum") +
			geom_text(aes(label=round(value*100, 0)),
					size=3, vjust=2) +
			coord_flip() + 
			quiet +
			ylab(s$ylabel) +
			facet_grid(~cbreak) +
			opts(
					panel.grid.minor = theme_blank(), 
					axis.text.x = theme_text(
							size=s$surveyorDefaults$defaultThemeSize, 
							angle=90, 
							hjust=1)
			)
	
	as.surveyorPlot(p, s, plotFunction=plotFunction)
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
  p <- ""
  flag <- FALSE
  #browser()
  if(is.list(s$surveyor$crossbreak)){
    if(identical(s$surveyor$cbreak, s$surveyor$crossbreak[[1]])) flag <- TRUE
  }
  
  if(!is.list(s$surveyor$crossbreak)) flag <- TRUE
  
  if(flag){
    #if(s$data$cbreak != surveyor$crossbreak) return(NULL)
    ### Carry on as usual
    unique_resp <- unique(s$data$response)
    items <- paste("\\item", latexTranslate(unique_resp))
      p <- paste(
  			"\\begin{itemize}",
  			items,
  			"\n\\end{itemize}",
  			collapse="\n"
  	  )
  }
  class(p) <- "text"
  as.surveyorPlot(p, s, plotFunction=plotFunction)
}

#-------------------------------------------------------------------------------


# TODO: Surveyor: Fix plotNetScore to deal with weighting
#' Plot data in net score format (bar chart, but percentage axis)
#'
#' @param s A surveyorStats object
#' @param plotFunction Character vector: Identifies the name of the plot function used to create the plot
#' @param ... Ignored
#' @seealso
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @family plotFunctions
#' @keywords plot
#' @export
plotNetScore <- function(s, plotFunction="plotNetScore", ...){
  stopifnot(is.surveyorStats(s))
  f <- s$data
	f$hjust <- 0
	if (max(f$value) > 0)    f[f$value > 0,   ]$hjust <- -0.1  
	if (max(f$value) > 0.5)  f[f$value > 0.5, ]$hjust <-  1.1  
	if (min(f$value) < 0)    f[f$value < 0,    ]$hjust <-  1.1  
	if (min(f$value) < -0.5) f[f$value < -0.5, ]$hjust <- -0.1  
	
	
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
					formatter = "percent", 
					breaks    = c(-1, 0, 1)
			) +
			facet_grid(~cbreak)  
			
	if (length(unique(f$cbreak)) > 1){
		p <- p + opts(axis.text.x = theme_blank())
	}
	
	qlayout <- c(ifelse(is.factor(f$cbreak), nlevels(f$cbreak), length(unique(f$cbreak))), 1)
	q <- lattice::barchart(question~value|cbreak, f, layout=qlayout, origin=0)
			
  ifelse(s$surveyorDefaults$fastgraphics, 
      return(as.surveyorPlot(q, s, plotFunction="plotNetScore")), 
      return(as.surveyorPlot(p, s, plotFunction="plotNetScore"))
  )
}

