################################################################################
###  Question handling functions                                             ###
################################################################################



#' Identifies subquestions of a given question in a data.frame
#' 
#' Given a question id, e.g. Q4, finds all subquestions, e.g. Q4_1, Q4_2, etc. 
#'
#' @param Qdata Qdata frame with survey Qdata
#' @param Qid The question id, e.g. Q4
## #' @examples
## #' qdata <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
## #' qtext <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
## #' names(qtext) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
## #' surveyor:::get_q_subquestions(qdata, "Q4") 					
get_q_subquestions <- function(Qdata, Qid){
	Q <- grep(paste(Qid, "_[[:digit:]]*$", sep=""), names(Qdata))
	if (identical(Q, integer(0))){
		return(NULL)
	} else {
		Q <- Q[order(names(Qdata[, Q]))]
		return(paste(Qid, Q - min(Q) + 1, sep="_"))
	}	
}


#' Returns unique elements of question text
#' 
#' Given a question id, e.g. Q4, finds all subquestions, e.g. Q4_1, Q4_2, etc, 
#' and returns the question text that is unique to each 
#'
#' @param Qdata Qdata frame with survey Qdata
#' @param Qid The question id, e.g. Q4
#' @param Qtext Named character vector containing the question text
#' @seealso get_qtext, get_qtext_common
## #' @examples
## #' qdata <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
## #' qtext <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
## #' names(qtext) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
## #' surveyor:::get_qtext_unique(qdata, "Q4", qtext) 					
get_qtext_unique <- function(Qdata, Qid, Qtext){
	Q <- get_q_subquestions(Qdata, Qid)
	Q <- Qtext[Q]
	
	tmp <- str_reverse(str_common_unique(as.character(Q))$unique)
	Qu <- str_reverse(str_common_unique(tmp)$unique)
	sub("^[0-9]+\\. ", "", Qu)
}

#' Returns common elements of question text
#' 
#' Given a question id, e.g. Q4, finds all subquestions, e.g. Q4_1, Q4_2, etc, 
#' and returns the question text that is common to all 
#'
#' @param Qdata Qdata frame with survey Qdata
#' @param Qid The question id, e.g. Q4
#' @param Qtext Named character vector containing the question text
#' @seealso get_qtext, get_qtext_unique
## #' @examples
## #' qdata <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
## #' qtext <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
## #' names(qtext) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
## #' surveyor:::get_qtext_common(qdata, "Q4", qtext) 					
get_qtext_common <- function(Qdata, Qid, Qtext){
	Q <- get_q_subquestions(Qdata, Qid)
	Q <- Qtext[Q]
	
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
#' @param Qid The question id, e.g. Q4
#' @seealso get_qtext_common, get_qtext_unique
## #' @examples
## #' qdata <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
## #' qtext <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
## #' names(qtext) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
## #' surveyor:::get_qtext(qdata, "Q1", qtext)
## #'' surveyor:::get_qtext(qdata, "Q4", qtext)
get_qtext <- function(surveyor, Qid){
	
	Qdata <- surveyor$qdata
	Qtext <- surveyor$qtext
	
	w <- which(names(Qdata)==Qid)
	if (length(w)==1){
		ret <- Qtext[Qid][1]
	} else {
		w <- which(names(Qdata)==paste(Qid, "_1", sep=""))
		if (length(w)==1){
			ret <- get_qtext_common (Qdata, Qid, Qtext)
		} else {
			ret <- Qid
		}
	}
	as.character(ret)
}


