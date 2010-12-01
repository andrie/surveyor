#' Code survey data in array question form
#'
#' Code survey data in array question form (i.e. with subquestions)
#' 
#' @param surveyor Surveyor object
#' @param q_id Question id
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
code_single <- function(
		surveyor,
		q_id
){
	if(is.numeric(surveyor$q_data[, q_id])){
		response <- surveyor$q_data[, q_id]
	} else {
		response <- str_wrap(as.character(surveyor$q_data[, q_id]), 30)
	}	
	response[response=="NA"] <- NA
	
	if (all(is.na(response))){
		return(NULL)
	}		
		
	data.frame(
			crossbreak = surveyor$crossbreak,
			question = rep("1", nrow(surveyor$q_data)),
			response = response,
			weight = surveyor$weight,
			stringsAsFactors = FALSE
	)
}

#code_single <- function(
#		surveyor,
#		q_id
#){
#	data.frame(
#			variable = str_wrap(as.character(surveyor$q_data[, q_id]), 30),
#			value = rep(1, nrow(surveyor$q_data)),
#			crossbreak = surveyor$crossbreak,
#			weight = surveyor$weight
#	)
#}
