# TODO: Surveyor: Fix stacked bar chart - make axes quiet, reduce size of legend
# TODO: Surveyor: Modify default behaviour to get rid of RColorBrewer warning messages



#' Blank axis titles and no legend
#' 
#' @keywords internal
quiet <- opts(
		legend.position="none",
		axis.title.x = theme_blank(),
		axis.title.y = theme_blank()
)

#' Blank axis titles
#' 
#' @keywords internal
quiet_axes <- opts(
		axis.title.x = theme_blank(),
		axis.title.y = theme_blank()
)


#' Define minimal theme
#' 
#' @keywords internal
theme_minimal <- opts(
		axis.title.x = theme_blank(),
		axis.title.y = theme_blank(),
		axis.text.x  = theme_blank(),
		axis.text.y  = theme_blank(),
		axis.ticks   = theme_blank(),
		axis.ticks.margin = unit(rep(0,4), "lines"),
		axis.ticks.length = unit(0, "cm"),
		panel.border = theme_blank(),
		panel.ticks  = theme_blank(),
		panel.grid.major = theme_blank(),
		panel.grid.minor = theme_blank(),
		plot.margin = unit(rep(0,4), "lines"),
		panel.margin = unit(rep(0,4), "lines"),
		legend.position  = "none"
)

#' Creates surveyor_plot object.
#'  
#' Creates surveyor_plot object, a container for either ggplot or lattice graphic. 
#' 
#' @param plot A ggplot or lattice object 
#' @param expansion Multiplier for plot vertical dimension
#' @param plot_function The plot function that was used to create the plot
#' @return A surveyor_plot object
#' @keywords internal
as_surveyor_plot <- function(
    plot,
    expansion = 1,
    plot_function =""
){
  structure(
      list(
          plot=plot,
          expansion=expansion,
          plot_function=plot_function
      ),
      class = "surveyor_plot"
  )
}


#' Guesses which plot format is optimal
#' 
#' Investigates columns in supplied data, and then chooses either \code{\link{plot_bar}} or \code{\link{plot_column}}
#'
#' @param s A surveyor_stats object
#' @param surveyor Surveyor object
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
plot_guess <- function(s, surveyor){
  if(class(s)!="surveyor_stats")  stop("plot_guess: s must be a surveyor_stats object")
  if(class(surveyor)!="surveyor") stop("plot_guess: surveyor must be a surveyor object")
  f <- s$data
  if(s$plot_function!=""){
    if(s$plot_function=="plot_net_score") plot_net_score(s, surveyor)
  } else {
    if (is.null(f$question)){
      # Plot single question
      if (is.null(f$response)) {
        plot_column(s, surveyor)
      } else {  
        plot_bar(s, surveyor)
      }  
      
    } else {
      if (is.null(f$response)) {
        # Plot array of single values per question
        plot_bar(s, surveyor)
      } else {
        # Plot array question as stacked bar
        plot_bar(s, surveyor)
      }
    }
  }
}



#' Plot data in bar chart format
#'
#' @param s A surveyor_stats object
#' @param surveyor Surveyor object
#' @param plot_function Character vector: Identifies the name of the plot function used to create the plot
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
plot_bar <- function(s, surveyor, plot_function="plot_bar"){
	f <- s$data
  #print(str(f))
		
	f$cbreak <- f$cbreak[drop=TRUE]
	
	if(surveyor$defaults$fastgraphics){
    ### plot using lattice
		qlayout <- c(ifelse(is.factor(f$cbreak), nlevels(f$cbreak), length(unique(f$cbreak))), 1)
		if (is.null(f$question)){
			# Plot single question
      if (is.null(f$response)) {
        q <- lattice::barchart(value~cbreak, f, layout=qlayout,  box.ratio=1.5, origin=0)
      } else {  
        q <- lattice::barchart(response~value|cbreak, f, layout=qlayout,	box.ratio=1.5, origin=0)
      }  
			
		} else {
			if (is.null(f$response)) {
				# Plot array of single values per question
				q <- lattice::barchart(question~value|cbreak, 
						f, layout=qlayout,	box.ratio=1.5, origin=0, groups=f$cbreak, stack=TRUE)
			} else {
				# Plot array question as stacked bar
				q <- lattice::barchart(question~value|cbreak, 
						f, layout=qlayout,	box.ratio=1.5, origin=0, groups=f$response, stack=TRUE, 
						auto.key=list(space="right"))
			}
		}
	} else {
    ### plot using ggplot
    if (is.null(f$question)){
      # Plot single question
      if (is.null(f$response)) {
        p <- ggplot(f, aes_string(x="1", y="value", fill="factor(cbreak)"))
      } else {  
        p <- ggplot(f, aes_string(x="response", y="value", fill="factor(cbreak)"))
      }  
			
		} else {
      if (is.null(f$response)) {
        # Plot array of single values per question
				p <- ggplot(f, aes_string(x="question", y="value", fill="factor(cbreak)"))
			} else {
				# Plot array question as stacked bar
				p <- ggplot(f, aes_string(x="question", y="value", fill="factor(response)"))
			}
		}
		p <- p + 
				theme_surveyor(surveyor$defaults$default_theme_size) +
				geom_bar(stat="identity") + 
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
		
		
		if (is.null(f$question)){
			# Plot single question
			if(length(unique(f$response)) > 8){p <- p + scale_fill_hue()}
		} else {
			# Plot array of single values per question
			# Plot array question as stacked bar
			p <- p + opts(
					axis.text.x=theme_text(size=surveyor$defaults$default_theme_size*0.5, angle=90)
			)
			if(length(unique(f$question)) > 8){p <- p + scale_fill_hue()}
			if (!is.null(f$response) & nlevels(f$response[drop=TRUE])>1){ 
				p <- p + opts(legend.position="right")
			}	
#      if (!is.null(f$response) && !is.null(f$response)){ 
#        p <- p + opts(legend.position="right")
#      } 
    }
	}
		
	ifelse(surveyor$defaults$fastgraphics,
      return(as_surveyor_plot(q, plot_function=plot_function)), 
      return(as_surveyor_plot(p, plot_function=plot_function))
  )
}

#' Plot data in bar chart format without modifying format.
#' 
#' The standard plot_bar() function will plot the data in a stacked bar chart format and apply percentage formatting.  plot_bar_sum() applies no formatting.
#'
#' @param s A surveyor_stats object
#' @param surveyor Surveyor object
#' @param plot_function Character vector: Identifies the name of the plot function used to create the plot
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
plot_bar_sum <- function(s, surveyor){
	plot_bar(s, surveyor, plot_function="plot_bar_sum")
}

###############################################################################
#' Plot data in column chart format
#'
#' @param s A surveyor_stats object
#' @param surveyor Surveyor object
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
plot_column <- function(s, surveyor, plot_function="plot_column"){
	f <- s$data
	
	f$cbreak <- f$cbreak[drop=TRUE]
	
	if(surveyor$defaults$fastgraphics){
		qlayout <- c(ifelse(is.factor(f$cbreak), nlevels(f$cbreak), length(unique(f$cbreak))), 1)
		if (is.null(f$question)){
			# Plot single question
			q <- lattice::barchart(response~value|cbreak, f, layout=qlayout,	box.ratio=1.5, origin=0)
			
		} else {
			if (is.null(f$response)) {
				# Plot array of single values per question
				q <- lattice::barchart(question~value|cbreak, 
						f, layout=qlayout,	box.ratio=1.5, origin=0, groups=f$cbreak, stack=TRUE)
			} else {
				# Plot array question as stacked bar
				q <- lattice::barchart(question~value|cbreak, 
						f, layout=qlayout,	box.ratio=1.5, origin=0, groups=f$response, stack=TRUE, 
						auto.key=list(space="right"))
			}
		}
	} else {
		if (is.null(f$question)){
			# Plot single question
#			p <- ggplot(f, aes_string(x="response", y="value", fill="factor(cbreak)"))
			p <- ggplot(f, aes_string(x="factor(cbreak)", y="value", fill="factor(cbreak)"))
			
		} else {
			if (is.null(f$response)) {
				# Plot array of single values per question
#				p <- ggplot(f, aes_string(x="question", y="value", fill="factor(cbreak)"))
				p <- ggplot(f, aes_string(x="factor(cbreak)", y="value", fill="factor(cbreak)"))
			} else {
				# Plot array question as stacked bar
#			p <- ggplot(f, aes_string(x="question", y="value", fill="factor(response)"))
			p <- ggplot(f, aes_string(x="factor(cbreak)", y="value", fill="factor(response)"))
			}
#      f$value_labels <- match.fun(s$formatter)(f$value)
#      p <- p + geom_text(data=f, aes_string(label="value_labels"), vjust=0.5, size=3)
    }
		p <- p + 
				theme_surveyor(surveyor$defaults$default_theme_size) +
				geom_bar(stat="identity") + 
				#coord_flip() + 
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

		
		if (is.null(f$question)){
			# Plot single question
			if(length(unique(f$response)) > 8){p <- p + scale_fill_hue()}
		} else {
			# Plot array of single values per question
			# Plot array question as stacked bar
			p <- p + facet_grid(question~., scales="free") +
					opts(
							strip.text.y = theme_text(size = surveyor$defaults$default_theme_size * 0.8)
#							axis.text.y = theme_text(size = surveyor$defaults$default_theme_size * 0.5)
					)
			p <- p + geom_text(aes_string(label="signif(value, 3)"), vjust=0.5, size=3)
			if(length(unique(f$question)) > 8){p <- p + scale_fill_hue()}
			if (!is.null(f$response) & nlevels(f$response[drop=TRUE]==1)){ 
				p <- p + opts(legend.position="right")
			}	
		}
	}
	
  ifelse(surveyor$defaults$fastgraphics,
      return(as_surveyor_plot(q, plot_function=plot_function)), 
      return(as_surveyor_plot(p, plot_function=plot_function))
  )
}




###############################################################################

#' Plot data in bubble chart format
#'
#' @param s A surveyor_stats object
#' @param surveyor Surveyor object
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
plot_point <- function(s, surveyor){
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
			theme_surveyor(surveyor$defaults$default_theme_size) +
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
							size=surveyor$defaults$default_theme_size, 
							angle=90, 
							hjust=1)
			)
	
	as_surveyor_plot(p, plot_function="plot_point")
}

###############################################################################


#' Plot data as text
#'
#' @param s A surveyor_stats object
#' @param surveyor Surveyor object
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
plot_text <- function(s, surveyor){
  items <- paste("\\item", latexTranslate(s$data$response))
    p <- paste(
			"\\begin{itemize}",
			items,
			"\n\\end{itemize}",
			collapse="\n"
	  )
    class(p) <- "text"
    as_surveyor_plot(p, plot_function="plot_text")
  }

###############################################################################


# TODO: Surveyor: Fix plot_net_score to deal with weighting
#' Plot data in net score format (bar chart, but percentage axis)
#'
#' @param s A data frame with coded answers, provided by a code_* function
#' @param surveyor Surveyor object
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
plot_net_score <- function(s, surveyor){
	f <- s$data
	f$hjust <- 0
	if (max(f$value) > 0)    f[f$value > 0,   ]$hjust <- -0.1  
	if (max(f$value) > 0.5)  f[f$value > 0.5, ]$hjust <-  1.1  
	if (min(f$value) < 0)    f[f$value < 0,    ]$hjust <-  1.1  
	if (min(f$value) < -0.5) f[f$value < -0.5, ]$hjust <- -0.1  
	
	
	p <- ggplot(f, aes_string(x="question", y="value")) +
			theme_surveyor(surveyor$defaults$default_theme_size) +
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
			
	ifelse(surveyor$defaults$fastgraphics, 
      as_surveyor_plot(q, plot_function="plot_net_score"), 
      as_surveyor_plot(p, plot_function="plot_net_score")
  )
}

