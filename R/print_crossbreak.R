################################################################################
###  print crossbreak functions                                              ###
################################################################################




#' Prints crossbreak information in latex format
#'
#' Prints crossbreak information in latex format
#' 
#' @param g A data frame with coded answers, provided by a stats_* function
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
#' @export
print_cb_stats <- function(g){
	
	if(is.null(g)){
		return(NULL)
	}
	
	if (!is.null(g$question) && !is.null(g$response)){
		v <- tapply(g$value, list(g$response, g$question, g$crossbreak), sum, na.rm=TRUE)
		dimtotal1 <- c(1,2)
		dimtotal2 <- c(2,1)
	} else {	
		if (is.null(g$question)){
			v <- tapply(g$value, list(g$response, g$crossbreak), sum, na.rm=TRUE)
			dimtotal1 <- 1
			dimtotal2 <- 2
		} else {
			v <- tapply(g$value, list(g$question, g$crossbreak), sum, na.rm=TRUE)
			dimtotal1 <- 1
			dimtotal2 <- 2
		}
	}
	
	v <- paste_percent(v)

	ret <- paste(
			"\\vspace{1 pc}",
			"\nWeighted totals\n\n",
			capture_table(v), 
			
			sep="", 
			collapse="\n")

	ret
}

#' Prints crossbreak information in latex format
#'
#' Prints crossbreak information in latex format
#' 
#' @param f A data frame with coded answers, provided by a code_* function
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
#' @export
print_cb_code <- function(f){
	
	if(is.null(f)){
		return(NULL)
	}
	
	if (is.null(f$question)){
		v <- tapply(f$value, list(f$response, f$crossbreak), sum, na.rm=TRUE)
		dimtotal1 <- 1
		dimtotal2 <- 2
	} else {
		v <- tapply(f$value, list(f$response, f$question, f$crossbreak), sum, na.rm=TRUE)
		dimtotal1 <- c(1,2)
		dimtotal2 <- c(2,1)
	}
	
	vp1 <- prop.table(v, dimtotal2, na.rm=TRUE)
	p1 <- vp1
	#p1 <- paste_percent(vp1)
	
	vp2 <- prop.table(v, dimtotal1, na.rm=TRUE)
	p2 <- vp2
	#p2 <- paste_percent(vp2)
	
	v <- round(v, 1)
	
#	align <- paste(c("l", rep("r", dim(v)[2])), collapse="")
	
	tmpv <- capture_table(v)
	tmp1 <- capture_table(p1)
	tmp2 <- capture_table(p2)
	
	ret <- paste(
			"\\vspace{1 pc}",
			"\nWeighted totals\n\n",
			tmpv, 
			
			"\\vspace{2 pc}\n",
			"\nPercentage of Row\n\n",
			as.character(tmp1), 
			
			"\\vspace{2 pc}\n",
			"\nPercentage of Column\n\n",
			as.character(tmp2), 
			
			sep="", 
			collapse="\n")
	
#	ret	<- tmpv
	ret
}

