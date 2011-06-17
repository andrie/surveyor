################################################################################
###  Question handling functions                                             ###
################################################################################


#' Identifies subquestions of a given question in a data frame.
#' 
#' Given a question id, e.g. Q4, finds all subquestions, e.g. Q4_1, Q4_2, etc. 
#' The pattern can be set by modifying question_pattern in the surveyor defaults
#'
#' @param q_data data frame with survey data
#' @param q_id The question id, e.g. Q4
#' @param surveyor surveyor object used to set default question pattern
#' @export
#' @examples
#' q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
#' get_q_subquestions(q_data, "Q4") 					
get_q_subquestions <- function(q_data, q_id, surveyor=NULL){
	if (is.null(surveyor)){
		pattern <- "_[[:digit:]]*$"
	} else {
		pattern <- surveyor$defaults$question_pattern  #Defaults to "_[[:digit:]]*$"
	}	
	find <- grep(paste("^", q_id, pattern, sep="", collapse=""), names(q_data))
	if (identical(find, integer(0))){
		ret <- NULL
	} else {
		s <- sort(which(names(q_data) %in% names(q_data)[find]))
		ret <- names(q_data)[s]
	}	
  ret
}


#' Returns unique elements of question text.
#' 
#' Given a question id, e.g. Q4, finds all subquestions, e.g. Q4_1, Q4_2, etc, 
#' and returns the question text that is unique to each 
#'
#' @param q_data data frame with survey data
#' @param q_id string containing the question id, e.g. Q4. This has to
#' match a name in q_data
#' @param q_text Named character vector containing the question text
#' @param surveyor surveyor object, used to set default question pattern
#' @seealso \code{\link{get_q_text}}, \code{\link{get_q_text_common}}
#' @export
#' @examples
#' q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
#' get_q_text_unique(q_data, "Q4", q_text) 					
get_q_text_unique <- function(q_data, q_id, q_text, surveyor=NULL){
  Q <- as.character(q_text[get_q_subquestions(q_data, q_id, surveyor)])
  get_q_common_unique_pattern(Q)$unique
}

#get_q_text_unique <- function(q_data, q_id, q_text, surveyor=NULL){
#  if (is.null(surveyor)){
#    append  <- TRUE
#    prepend <- FALSE
#  } else {
#    append  <- surveyor$defaults$subquestion_append
#    prepend <- surveyor$defaults$subquestion_prepend
#  }
#  
#  Q <- get_q_subquestions(q_data, q_id, surveyor)
#  if(is.null(Q)){
#    return(NULL)
#  } else {
#    Q <- q_text[Q]
#    
#    tmp <- str_reverse(str_common_unique(as.character(Q))$unique)
#    Qu  <- str_reverse(str_common_unique(tmp)$unique)
#    Qu
#  } 
#}


#' Returns common elements of question text
#' 
#' Given a question id, e.g. Q4, finds all subquestions, e.g. Q4_1, Q4_2, etc, 
#' and returns the question text that is common to all 
#'
#' @param q_data q_data frame with survey q_data
#' @param q_id The question id, e.g. Q4
#' @param q_text Named character vector containing the question text
#' @param surveyor surveyor object, used to set default question pattern
#' @seealso get_q_text, get_q_text_unique
#' @export
#' @examples
#' q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
#' get_q_text_common(q_data, "Q4", q_text) 					
get_q_text_common <- function(q_data, q_id, q_text, surveyor=NULL){
	Q <- as.character(q_text[get_q_subquestions(q_data, q_id, surveyor)])
  get_q_common_unique_pattern(Q)$common
}

#get_q_text_common <- function(q_data, q_id, q_text, surveyor=NULL){
#  if (is.null(surveyor)){
#    append  <- TRUE
#    prepend <- FALSE
#  } else {
#    append  <- surveyor$defaults$subquestion_append
#    prepend <- surveyor$defaults$subquestion_prepend
#  }
#  
#  Q <- get_q_subquestions(q_data, q_id, surveyor)
#  Q <- q_text[Q]
#  
#  tmpleft <- ""
#  tmpright <- ""
#  
#  if (append){
#    tmpleft <- str_common_unique(as.character(Q))$common
#  }
#  
#  if (prepend){
#    tmpright <- str_reverse(str_common_unique(as.character(str_reverse(Q)))$common)
#  }
#  
#  if (append && prepend){
#    tmpleft <- str_common_unique(as.character(Q))$common
#    tmpright <- str_reverse(str_common_unique(as.character(str_reverse(Q)))$common)
#  }
#  
#  Qu <- paste(tmpleft, tmpright, sep="")
## tmp <- gsub("^[0-9]+\\. ", "", Qu)   # Remove leading question number
## gsub("^[[[:print:]]*] *", "", tmp)
#}


#' Get common and unique text in question based on regex pattern identification
#' 
#' @param x A character vector
get_q_common_unique_pattern <- function(x){
  pattern <- c(
      "^(.*)\\((.*)\\)$",             # Find "Please tell us" in "Email (Please tell us)"
      "^(.*): (.*)$",                 # Find "What is your choice?" in "What is your choice?: Email"
      "^(.*):\\S(.*)$",               # Find "What is your choice?" in "What is your choice?:Email"
      "^(.\\d*)\\(\\d{1,3}\\) (.*)$", # Find "What is your choice?" in "What is your choice?:Email"
      "^\\[(.*)\\] (.*)$"             # Find "What is your choice?" in "What is your choice?:Email"
  )
    most_common <- function(x){
    r <- sapply(x, function(xt)sum(grepl(xt, x, fixed=TRUE)))
    sort(r, decreasing=TRUE)[1]
  }
  pattern_sum <- unname(vapply(pattern, function(p)sum(grepl(p, x)), 0))
  if(max(pattern_sum) >= 1){
    which_patterns <- order(pattern_sum, decreasing=TRUE)[1]
    test_pattern <- pattern[which_patterns]
    xt <- str_match(x, test_pattern)
    r1 <- most_common(xt[, 2])
    r2 <- most_common(xt[, 3])
    
    if(unname(r1) > unname(r2)){
      t <- list(common=names(r1)[1], unique=str_trim(xt[, 3]))
    } else {  
      t <- list(common=names(r2)[1], unique=str_trim(xt[, 2]))
    }
    nNa <- sum(is.na(t$unique))  
    if(nNa > 0) t$unique[is.na(t$unique)] <- paste("NA_", seq_len(nNa), sep="")
  } else {
    t <- str_common_unique(x)
  }  
  t
}



#' Returns question text
#' 
#' Given a question id, e.g. "Q4", returns question text 
#'
#' @param surveyor A surveyor object
#' @param q_id The question id, e.g. Q4
#' @seealso get_q_text_common, get_q_text_unique
#' @export 
#' @examples
#' q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
#' get_q_text(q_data, "Q1")
#' get_q_text(q_data, "Q4")
get_q_text <- function(surveyor, q_id){
	
	q_data <- surveyor$q_data
	q_text <- surveyor$q_text
	
	w <- which(names(q_data)==q_id)
	if (length(w)==1){
		ret <- q_text[q_id][1]
	} else {
		w <- which(names(q_data)==paste(q_id, "_1", sep=""))
		if (length(w)==1){
			ret <- get_q_text_common (q_data, q_id, q_text, surveyor)
		} else {
			ret <- q_id
		}
	}
	as.character(ret)
}


