################################################################################
###  print cbreak functions                                              ###
################################################################################




#' Prints cbreak information in latex format
#'
#' Prints cbreak information in latex format
#' 
#' @param g A surveyor_stats object
#' @seealso Coding functions:
#' \code{\link{code_single}}, 
#' \code{\link{code_array}},
#' \code{\link{code_text}}
#' Summarising functions:
#' \code{\link{stats_bin}}, 
#' \code{\link{stats_net_score}}
#' Plot functions: 
#' \code{\link{plot_bar}}, 
#' \code{\link{plot_point}} 
#' @return data frame
#' @keywords internal
table_guess <- function(g){
	
	g_orig <- g
	g <- g$data
	
	if(is.null(g)){
		return(NULL)
	}
	
	sum_function <- function(x)signif(sum(x, na.rm=TRUE), 3)
	
	if (is.null(g$question)){
		# Plot single question
		v <- tapply(g$value, list(g$response, g$cbreak[drop=TRUE]), sum_function)
	} else {
		if (is.null(g$response)) {
			v <- tapply(g$value, list(g$question, g$cbreak[drop=TRUE]), sum_function)
		} else {
			if(nlevels(g$response[drop=TRUE])==1){
				v <- tapply(g$value, list(g$question, g$cbreak[drop=TRUE]), sum_function)
			} else {
				# Plot array question as stacked bar
				v <- tapply(g$value, list(g$question, g$response[drop=TRUE], g$cbreak[drop=TRUE]), sum_function)
			}
		}
	}
	
	if(g_orig$formatter=="percent") v <- apply(v, c(1, 2), paste_percent)
	
	paste(latex_table(v, "Weighted totals"), sep="", collapse="\n")

}

#' Prepares a string to be printed sideways (vertical) in LaTex
#'
#' @param x A character vector
#' @keywords internal
latex_sideways <- function(x){
  sanitize <- function(str) {
    result <- str
    result <- gsub("\\\\", "SANITIZE.BACKSLASH", result)
    result <- gsub("$", "\\$", result, fixed = TRUE)
    result <- gsub(">", "$>$", result, fixed = TRUE)
    result <- gsub("<", "$<$", result, fixed = TRUE)
    result <- gsub("|", "$|$", result, fixed = TRUE)
    result <- gsub("{", "\\{", result, fixed = TRUE)
    result <- gsub("}", "\\}", result, fixed = TRUE)
    result <- gsub("%", "\\%", result, fixed = TRUE)
    result <- gsub("&", "\\&", result, fixed = TRUE)
    result <- gsub("_", "\\_", result, fixed = TRUE)
    result <- gsub("#", "\\#", result, fixed = TRUE)
    result <- gsub("^", "\\verb|^|", result, fixed = TRUE)
    result <- gsub("~", "\\~{}", result, fixed = TRUE)
    result <- gsub("SANITIZE.BACKSLASH", "$\\backslash$", 
        result, fixed = TRUE)
    return(result)
  }
  x <- sanitize(x)
  paste(
      "\\begin{sideways}",
      x,
      "\\end{sideways}",
      sep=""
  )
}



#' Captures the print output of a Latex xtable
#'
#' @param x A table object
#' @param caption Caption for table
#' @keywords internal
latex_table <- function(x, caption){
# align <- paste(c("l", rep("r", dim(x)[2])), collapse="")
  align <- paste(c("p{5cm}", rep("r", dim(x)[2])), collapse="")
  x <- as.table(x)
  paste(capture.output(
          print(xtable(
                  x,
                  caption=caption,
                  align=align,
                  digits=NULL,
                  table.placement="!h"),
              floating=FALSE,
              caption.placement="bottom",
              sanitize.colnames.function=function(str){latex_sideways(str)})
      ), collapse="\n")   
}



## #' Prints cbreak information in latex format
## #'
## #' Prints cbreak information in latex format
## #' 
## #' @param f A data frame with coded answers, provided by a code_* function
## #' @seealso Coding functions:
## #' \code{\link{code_single}}, 
## #' \code{\link{code_array}},
## #' \code{\link{code_text}}
## #' Summarising functions:
## #' \code{\link{stats_bin}}, 
## #' \code{\link{stats_net_score}}
## #' Plot functions: 
## #' \code{\link{plot_bar}}, 
## #' \code{\link{plot_point}} 
## #' @return data frame
## #' @keywords internal
#print_cb_code <- function(f){
#	
#	if(is.null(f)){
#		return(NULL)
#	}
#	
#	if (is.null(f$question)){
#		v <- tapply(f$value, list(f$response, f$cbreak), sum, na.rm=TRUE)
#		dimtotal1 <- 1
#		dimtotal2 <- 2
#	} else {
#		v <- tapply(f$value, list(f$response, f$question, f$cbreak), sum, na.rm=TRUE)
#		dimtotal1 <- c(1,2)
#		dimtotal2 <- c(2,1)
#	}
#	
#	vp1 <- prop.table(v, dimtotal2, na.rm=TRUE)
#	p1 <- vp1
#	#p1 <- paste_percent(vp1)
#	
#	vp2 <- prop.table(v, dimtotal1, na.rm=TRUE)
#	p2 <- vp2
#	#p2 <- paste_percent(vp2)
#	
#	v <- round(v, 1)
#	
##	align <- paste(c("l", rep("r", dim(v)[2])), collapse="")
#	
#	tmpv <- latex_table(v)
#	tmp1 <- latex_table(p1)
#	tmp2 <- latex_table(p2)
#	
#	ret <- paste(
##			"\\vspace{1 pc}",
#			"\\\\Weighted totals\\nopagebreak",
#			tmpv, 
#			
##			"\\vspace{2 pc}\n",
#			"\\\\Percentage of Row\\nopagebreak",
#			as.character(tmp1), 
#			
##			"\\vspace{2 pc}\n",
#			"\\\\Percentage of Column\\nopagebreak",
#			as.character(tmp2), 
#			
#			sep="", 
#			collapse="\n")
#	
##	ret	<- tmpv
#	ret
#}

