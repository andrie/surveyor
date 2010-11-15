################################################################################
###  Question handling functions                                             ###
################################################################################


idQuestionGroup <- function(data, Qnumber, Qtext){
	Q <- grep(paste(Qnumber, "_[[:digit:]]*$", sep=""), names(data))
	Q <- Q[order(names(kd[, Q]))]
	paste(Qnumber, Q - min(Q) + 1, sep="_")
}


getQunique <- function(data, Qnumber, Qtext){
	Q <- idQuestionGroup(data, Qnumber, Qtext)
	Q <- Qtext[Q]
	
	tmp <- reverse_string(strDiff(as.character(Q))$unique)
	Qu <- reverse_string(strDiff(tmp)$unique)
	sub("^[0-9]+\\. ", "", Qu)
}

getQcommon <- function(data, Qnumber, Qtext){
	Q <- idQuestionGroup(data, Qnumber, Qtext)
	Q <- Qtext[Q]
	
	tmpleft <- strDiff(as.character(Q))$common
	tmpright <- reverse_string(strDiff(as.character(reverse_string(Q)))$common)
	Qu <- paste(tmpleft, tmpright, sep="")
	tmp <- gsub("^[0-9]+\\. ", "", Qu)   # Remove leading question number
	gsub("^[[[:print:]]*] *", "", tmp)
}

getQtext <- function(x){
	w <- which(names(kd)==x)
	if (length(w)==1){
#    w <- Qs[which(names(kd)==x)]
		ret <- Qs[x][1]
	} else {
		w <- which(names(kd)==paste(x, "_1", sep=""))
		if (length(w)==1){
			ret <- getQcommon (kd, x, Qs)
		} else {
			ret <- x
		}
	}
	as.character(ret)
}


cb <- function(x){
	data.frame(crossbreak=kd$crossbreak, x=x, weight=kd$weight)
}

