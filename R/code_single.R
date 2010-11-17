#' Code survey data in single question form
#'
#' @param surveyor Surveyor object
#' @param Qid Question id
#' @param multicode Boolean
#' @param remove.other Boolean
#' @param index Crossbreak variable
#' @param weight
#' @param ...
#' @seealso code_single, code_rank
#' @export
code_single <- function(Qnr,
		data=kd,
		Qtext=Qs,
		weight=TRUE,
		...){
	x <- data[, Qnr]
	x <- factor(x)
	levels(x) <- wordwrap(levels(x), 30)
	cb(x)
}

