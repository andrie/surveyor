#-------------------------------------------------------------------------------
###  print cbreak functions                                              ###
#-------------------------------------------------------------------------------




#' Prints cbreak information in latex format
#'
#' Prints cbreak information in latex format
#' 
#' @param g A surveyorStats object
#' @return data frame
#' @export
tableGuess <- function(g){
	
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
	
	if(g_orig$formatter=="percent") v <- apply(v, c(1, 2), formatPercent)
	
	paste(latexTable(v, "Weighted totals"), sep="", collapse="\n")

}

#' Prepares a string to be printed sideways (vertical) in LaTex
#'
#' @param x A character vector
#' @keywords internal
latexSideways <- function(x){
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
latexTable <- function(x, caption){
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
              sanitize.colnames.function=function(str){latexSideways(str)})
      ), collapse="\n")   
}



#TODO: Create unit test for flatten.pairwise.table

#' Takes pairwise comparison table and flatten it for easy printing.
#' 
#' @param x A matrix, typically the result of \code{\link[stats]{pairwise.table}}
#' @examples
#' data(airquality)
#' airquality <- within(airquality, Month <- factor(Month, labels = month.abb[5:9]))
#' x <- with(airquality, pairwise.t.test(Ozone, Month))
#' surveyor:::flatten.pairwise.table(x$p.value)
flatten.pairwise.table <- function(x){
  f <- matrix(NA, 1, ncol(x))
  rownames(f) <- colnames(x)[1]
  g <- matrix(NA, nrow(x)+1, 1)
  colnames(g) <- rownames(x)[nrow(x)]
  ans <- cbind(rbind(f, x), g)
  ans[upper.tri(ans)] <- ans[lower.tri(ans)]
  ans
}

