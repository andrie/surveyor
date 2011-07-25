# TODO: Surveyor: Plot_bar with crossbreak and multiple questions doesn't have correct colours



#' Creates surveyor_plot object.
#'  
#' Creates surveyor_plot object, a container for either ggplot or lattice graphic. 
#' 
#' @param plot A ggplot or lattice object 
#' @param expansion Multiplier for plot vertical dimension
#' @param plot_function The plot function that was used to create the plot
#' @return A surveyor_plot object
#' @keywords internal
as.surveyor_plot <- function(
    plot,
    surveyor_stats,
    expansion = 1,
    plot_function =""
){
  stopifnot(is.surveyor_stats(surveyor_stats))
  ### Adds plot title ###
  if(surveyor_stats$surveyor$defaults$add_plot_title & inherits(plot, "ggplot")){
      plot <- plot + opts(title=surveyor_stats$surveyor$plot_title)
    }
  
  ### Create list ###
  structure(
      list(
          plot=plot,
          expansion=expansion,
          plot_function=plot_function
      ),
      class = "surveyor_plot"
  )
}

#' Test object for membership of class "surveyor_plot".
#'  
#' Test object for membership of class "surveyor_plot".
#' 
#' @param x Object 
#' @return TRUE or FALSE
#' @keywords internal
is.surveyor_plot <- function(x){
  inherits(x, "surveyor_plot")
}



#' Guesses which plot format is optimal
#' 
#' Investigates columns in supplied data, and then chooses either \code{\link{plot_bar}} or \code{\link{plot_column}}
#'
#' @param s A surveyor_stats object
#' @param ... Other parameters passed to specific plot function
#' @seealso 
#' Plot functions: 
#' \itemize{
#' \item \code{\link{plot_bar}} 
#' \item \code{\link{plot_bar_sum}} 
#' \item \code{\link{plot_column}} 
#' \item \code{\link{plot_point}} 
#' \item \code{\link{plot_text}} 
#' \item \code{\link{plot_net_score}} 
#' }
#' 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords plot
#' @export
plot_guess <- function(s, ...){
  stopifnot(is.surveyor_stats(s))
  f <- s$data
  if(s$plot_function != ""){
    if(is.function(match.fun(s$plot_function))){
#    if(s$plot_function=="plot_net_score") {
#      plot_net_score(s, surveyor)
    match.fun(s$plot_function)(s, ...)
    } else {
      stop(paste("Plot function specified in surveyor_stats not found, plot_function =", s$plot_function))
    }  
  } else {
    if (is.null(f$question)){
      # Plot single question
      if (is.null(f$response)) {
        plot_column(s, ...)
      } else {  
        plot_bar(s, ...)
      }  
      
    } else {
      if (is.null(f$response)) {
        # Plot array of single values per question
        plot_bar(s, ...)
      } else {
        # Plot array question as stacked bar
        plot_bar(s, ...)
      }
    }
  }
}


#' Plot data in bar chart format
#'
#' @param s A surveyor_stats object
#' @param plot_function Character vector: Identifies the name of the plot function used to create the plot
#' @param ... ignored
#' @seealso 
#' Plot functions: 
#' \itemize{
#' \item \code{\link{plot_bar}} 
#' \item \code{\link{plot_bar_sum}} 
#' \item \code{\link{plot_column}} 
#' \item \code{\link{plot_point}} 
#' \item \code{\link{plot_text}} 
#' \item \code{\link{plot_net_score}} 
#' }
#' 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords plot
#' @export
plot_bar <- function(s, plot_function="plot_bar", ...){
  stopifnot(is.surveyor_stats(s))
  s <- format_labels(s)
	f <- s$data
  qtype <- qtype(s)
  #print(str(f))
		
	f$cbreak <- f$cbreak[drop=TRUE]
	
	if(s$surveyor$defaults$fastgraphics){
    ### plot using lattice
		qlayout <- c(ifelse(is.factor(f$cbreak), nlevels(f$cbreak), length(unique(f$cbreak))), 1)
    q <- switch(qtype,
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
        stop("plot_bar: Invalid value of qtype.  This should never happen")
    )
  }
    
  if(!s$surveyor$defaults$fastgraphics){
    ### Set up basic ggplot graphic ###
    p <- switch(qtype,
        singleQ_singleResponse =
            ggplot(f, aes_string(x="1", y="value", fill="factor(cbreak)")),
        singleQ_multiResponse =
            ggplot(f, aes_string(x="response", y="value", fill="factor(cbreak)")),
        gridQ_singleResponse = 
            ggplot(f, aes_string(x="question", y="value", fill="factor(cbreak)")),
        gridQ_multiResponse = 
                ggplot(f, aes_string(x="question", y="value", fill="factor(response)")),
        stop("plot_bar: Invalid value of qtype.  This should never happen")
    )    
    
    p <- p + geom_bar(stat="identity") 
    
    ### Add labels ###
    if(qtype %in% c("singleQ_multiResponse", "gridQ_singleResponse") || is.yesno(s))
#      p <- p + geom_text(aes_string(label="signif(value, 3)"), hjust=1, size=3)
      p <- p + geom_text(aes_string(label="value_labels", hjust="labels_just"), size=3)
    
    ### Plot options ###
    p <- p + 
				theme_surveyor(s$surveyor$defaults$default_theme_size) +
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
    if(qtype=="gridQ_multiResponse"  && !is.yesno(s))
      p <- p + opts(legend.position="right")

    ### Deal with too many colours ###
    if(length(unique(f$response)) > 8) p <- p + scale_fill_hue()
		if (qtype %in% c("singleQ_multiResponse", "gridQ_multiResponse", "gridQ_singleResponse")){
			p <- p + opts(
					axis.text.x=theme_text(size=s$surveyor$defaults$default_theme_size*0.5, angle=90)
			)
    }
	}
		
	ifelse(s$surveyor$defaults$fastgraphics,
      return(as.surveyor_plot(q, s, plot_function=plot_function)), 
      return(as.surveyor_plot(p, s, plot_function=plot_function))
  )
}


#' Plot data in bar chart format without modifying format.
#' 
#' The standard plot_bar() function will plot the data in a stacked bar chart format and apply percentage formatting.  plot_bar_sum() applies no formatting.
#'
#' @param s A surveyor_stats object
#' @param plot_function Character vector: Identifies the name of the plot function used to create the plot
#' @param ... Ignored
#' @seealso 
#' Plot functions: 
#' \itemize{
#' \item \code{\link{plot_bar}} 
#' \item \code{\link{plot_bar_sum}} 
#' \item \code{\link{plot_column}} 
#' \item \code{\link{plot_point}} 
#' \item \code{\link{plot_text}} 
#' \item \code{\link{plot_net_score}} 
#' }
#' 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords plot
#' @export
plot_bar_sum <- function(s, plot_function="plot_bar_sum", ...){
	plot_bar(s, plot_function=plot_function, ...)
}

###############################################################################
#' Plot data in column chart format
#'
#' @param s A surveyor_stats object
#' @param plot_function Character vector: Identifies the name of the plot function used to create the plot
#' @param ... Ignored
#' @seealso 
#' Plot functions: 
#' \itemize{
#' \item \code{\link{plot_bar}} 
#' \item \code{\link{plot_bar_sum}} 
#' \item \code{\link{plot_column}} 
#' \item \code{\link{plot_point}} 
#' \item \code{\link{plot_text}} 
#' \item \code{\link{plot_net_score}} 
#' }
#' 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords plot
#' @export
plot_column <- function(s, plot_function="plot_column", ...){
  stopifnot(is.surveyor_stats(s))
  s <- format_labels(s)
	f <- s$data
  qtype <- qtype(s)
  
	f$cbreak <- f$cbreak[drop=TRUE]
	
  if(s$surveyor$defaults$fastgraphics){
    ### plot using lattice
    qlayout <- c(ifelse(is.factor(f$cbreak), nlevels(f$cbreak), length(unique(f$cbreak))), 1)
    q <- switch(qtype,
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
        stop("plot_column: Invalid value of qtype.  This should never happen")
    )
  }
  
  if(!s$surveyor$defaults$fastgraphics){
    ### Set up basic ggplot graphic ###
    p <- switch(qtype,
        singleQ_singleResponse =
            ggplot(f, aes_string(x="factor(cbreak)", y="value", fill="factor(cbreak)")),
        singleQ_multiResponse =
            ggplot(f, aes_string(x="factor(cbreak)", y="value", fill="factor(cbreak)")),
        gridQ_singleResponse = 
            ggplot(f, aes_string(x="factor(cbreak)", y="value", fill="factor(cbreak)")),
        gridQ_multiResponse = 
            ggplot(f, aes_string(x="factor(cbreak)", y="value", fill="factor(cbreak)")),
        stop("plot_bar: Invalid value of qtype.  This should never happen")
    )    
    
    p <- p + geom_bar(stat="identity")
  
    ### Add plot options ###
    p <- p + 
				theme_surveyor(s$surveyor$defaults$default_theme_size) +
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
    if(qtype %in% c("singleQ_singleResponse", "singleQ_multiResponse", "gridQ_singleResponse") || 
        is.yesno(s))
#      p <- p + geom_text(aes_string(label="signif(value, 3)"), hjust=1, size=3)
#      p <- p + geom_text(aes_string(label="value_labels"), vjust=1, size=3)
      p <- p + geom_text(aes_string(label="value_labels", vjust="labels_just"), size=3)
    
		
    ### Add legend for multiple response ### 
    if(qtype=="gridQ_multiResponse"  && !is.yesno(s))
      p <- p + opts(legend.position="right")
    
    ### Deal with too many colours ###
    if(length(unique(f$response)) > 8) p <- p + scale_fill_hue()
    if (qtype == "singleQ_multiResponse"){
      p <- p + opts(
          axis.text.x=theme_text(size=s$surveyor$defaults$default_theme_size*0.9, angle=0)
      )
    }  
    if (qtype == "gridQ_multiResponse"){
      p <- p + opts(
          axis.text.x=theme_text(size=s$surveyor$defaults$default_theme_size*0.5, angle=90)
      )
    }  
    
	}
	
  ifelse(s$surveyor$defaults$fastgraphics,
      return(as.surveyor_plot(q, s, plot_function=plot_function)), 
      return(as.surveyor_plot(p, s, plot_function=plot_function))
  )
}




###############################################################################

#' Plot data in bubble chart format
#'
#' @param s A surveyor_stats object
#' @param plot_function Character vector: Identifies the name of the plot function used to create the plot
#' @param ... Ignored
#' @seealso 
#' Plot functions: 
#' \itemize{
#' \item \code{\link{plot_bar}} 
#' \item \code{\link{plot_bar_sum}} 
#' \item \code{\link{plot_column}} 
#' \item \code{\link{plot_point}} 
#' \item \code{\link{plot_text}} 
#' \item \code{\link{plot_net_score}} 
#' }
#' 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords plot
#' @export
plot_point <- function(s, plot_function="plot_point", ...){
  stopifnot(is.surveyor_stats(s))
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
			theme_surveyor(s$surveyor$defaults$default_theme_size) +
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
							size=s$surveyor$defaults$default_theme_size, 
							angle=90, 
							hjust=1)
			)
	
	as.surveyor_plot(p, s, plot_function=plot_function)
}

###############################################################################


#' Plot data as text.
#' 
#' Plots questions that are summarised using \code{\link{stats_text}}.
#'
#' @param s A surveyor_stats object
#' @param plot_function Character vector: Identifies the name of the plot function used to create the plot
#' @param ... Ignored
#' @seealso
#' Plot functions: 
#' \itemize{
#' \item \code{\link{plot_bar}} 
#' \item \code{\link{plot_bar_sum}} 
#' \item \code{\link{plot_column}} 
#' \item \code{\link{plot_point}} 
#' \item \code{\link{plot_text}} 
#' \item \code{\link{plot_net_score}} 
#' }
#' 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords plot
#' @export
plot_text <- function(s, plot_function="plot_text", ...){
  stopifnot(is.surveyor_stats(s))
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
  as.surveyor_plot(p, s, plot_function=plot_function)
}

###############################################################################


# TODO: Surveyor: Fix plot_net_score to deal with weighting
#' Plot data in net score format (bar chart, but percentage axis)
#'
#' @param s A surveyor_stats object
#' @param plot_function Character vector: Identifies the name of the plot function used to create the plot
#' @param ... Ignored
#' @export
#' @seealso
#' Plot functions: 
#' \itemize{
#' \item \code{\link{plot_bar}} 
#' \item \code{\link{plot_bar_sum}} 
#' \item \code{\link{plot_column}} 
#' \item \code{\link{plot_point}} 
#' \item \code{\link{plot_text}} 
#' \item \code{\link{plot_net_score}} 
#' }
#' 
#' For an overview of the surveyor package \code{\link{surveyor}}
#' @keywords plot
plot_net_score <- function(s, plot_function="plot_net_score", ...){
  stopifnot(is.surveyor_stats(s))
  f <- s$data
	f$hjust <- 0
	if (max(f$value) > 0)    f[f$value > 0,   ]$hjust <- -0.1  
	if (max(f$value) > 0.5)  f[f$value > 0.5, ]$hjust <-  1.1  
	if (min(f$value) < 0)    f[f$value < 0,    ]$hjust <-  1.1  
	if (min(f$value) < -0.5) f[f$value < -0.5, ]$hjust <- -0.1  
	
	
	p <- ggplot(f, aes_string(x="question", y="value")) +
			theme_surveyor(s$surveyor$defaults$default_theme_size) +
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
			
  ifelse(s$surveyor$defaults$fastgraphics, 
      return(as.surveyor_plot(q, s, plot_function="plot_net_score")), 
      return(as.surveyor_plot(p, s, plot_function="plot_net_score"))
  )
}

