################################################################################
###  Question handling functions                                             ###
################################################################################

# TODO: Pass pattern to question handling functions using surveyor defaults

#' Identifies subquestions of a given question in a data.frame
#' 
#' Given a question id, e.g. Q4, finds all subquestions, e.g. Q4_1, Q4_2, etc. 
#'
#' @param q_data q_data frame with survey q_data
#' @param q_id The question id, e.g. Q4
## #' @examples
## #' q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
## #' q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
## #' names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
## #' surveyor:::get_q_subquestions(q_data, "Q4") 					
get_q_subquestions <- function(q_data, q_id){
	find <- grep(paste(q_id, "_[[:digit:]]*$", sep=""), names(q_data))
	if (identical(find, integer(0))){
		return(NULL)
	} else {
		tmp <- names(q_data)[find]
		s <- sort(which(names(q_data) %in% tmp))
		
		
#		return(paste(q_id, Q - min(Q) + 1, sep="_"))
		return(names(q_data)[s])
	}	
}


#' Returns unique elements of question text
#' 
#' Given a question id, e.g. Q4, finds all subquestions, e.g. Q4_1, Q4_2, etc, 
#' and returns the question text that is unique to each 
#'
#' @param q_data data frame with survey data
#' @param q_id string containing the question id, e.g. Q4. This has to
#' 		match a name in q_data
#' @param q_text Named character vector containing the question text
#' @seealso get_q_text, get_q_text_common
## #' @examples
## #' q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
## #' q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
## #' names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
## #' surveyor:::get_q_text_unique(q_data, "Q4", q_text) 					
get_q_text_unique <- function(q_data, q_id, q_text){
	Q <- get_q_subquestions(q_data, q_id)
	Q <- q_text[Q]
	
#	if(length(Q)==1){
#		return(Q)
#	}
	
	tmp <- str_reverse(str_common_unique(as.character(Q))$unique)
	Qu  <- str_reverse(str_common_unique(tmp)$unique)
	Qu
#	sub("^[0-9]+\\. ", "", Qu)
}

#' Returns common elements of question text
#' 
#' Given a question id, e.g. Q4, finds all subquestions, e.g. Q4_1, Q4_2, etc, 
#' and returns the question text that is common to all 
#'
#' @param q_data q_data frame with survey q_data
#' @param q_id The question id, e.g. Q4
#' @param q_text Named character vector containing the question text
#' @seealso get_q_text, get_q_text_unique
## #' @examples
## #' q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
## #' q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
## #' names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
## #' surveyor:::get_q_text_common(q_data, "Q4", q_text) 					
get_q_text_common <- function(q_data, q_id, q_text){
	Q <- get_q_subquestions(q_data, q_id)
	Q <- q_text[Q]
	
	tmpleft <- str_common_unique(as.character(Q))$common
	tmpright <- str_reverse(str_common_unique(as.character(str_reverse(Q)))$common)
	Qu <- paste(tmpleft, tmpright, sep="")
	tmp <- gsub("^[0-9]+\\. ", "", Qu)   # Remove leading question number
	gsub("^[[[:print:]]*] *", "", tmp)
}

#' Returns question text
#' 
#' Given a question id, e.g. "Q4", returns question text 
#'
#' @param surveyor A surveyor object
#' @param q_id The question id, e.g. Q4
#' @seealso get_q_text_common, get_q_text_unique
## #' @examples
## #' q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
## #' q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
## #' names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
## #' surveyor:::get_q_text(q_data, "Q1", q_text)
## #'' surveyor:::get_q_text(q_data, "Q4", q_text)
get_q_text <- function(surveyor, q_id){
	
	q_data <- surveyor$q_data
	q_text <- surveyor$q_text
	
	w <- which(names(q_data)==q_id)
	if (length(w)==1){
		ret <- q_text[q_id][1]
	} else {
		w <- which(names(q_data)==paste(q_id, "_1", sep=""))
		if (length(w)==1){
			ret <- get_q_text_common (q_data, q_id, q_text)
		} else {
			ret <- q_id
		}
	}
	as.character(ret)
}


