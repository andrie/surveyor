#' Code survey data in array question form (i.e. with subquestions)
#'
#' @param surveyor Surveyor object
#' @param Qid Question id
#' @seealso code_array, code_rank
#' @export
code_single <- function(
		surveyor,
		Qid
	){
	data.frame(
			variable = str_wrap(as.character(surveyor$qdata[, Qid]), 30),
			value = rep(1, nrow(surveyor$qdata)),
			crossbreak = surveyor$crossbreak,
			weight = surveyor$weight
	)
}

