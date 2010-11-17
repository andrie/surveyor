################################################################################
###  print crossbreak functions                                              ###
################################################################################


printCB <- function(x, Qtext=x){
#  x <- cb("L")
	if (is.na(match("variable", names(x)))){
		v <- tapply(x$weight, list(x$x, x$crossbreak), sum, na.rm=TRUE)
		dimtotal1 <- 1
		dimtotal2 <- 2
	} else {
		v <- tapply(x$weight, list(x$variable, x$value, x$crossbreak), sum, na.rm=TRUE)
		dimtotal1 <- c(1,2)
		dimtotal2 <- c(2,1)
	}
	
	vp1 <- prop.table(v, dimtotal1, na.rm=TRUE)
	attrp <- attributes(vp1)
	p1 <- paste(round(vp1*100, digits=1), "%", sep="")
	attributes(p1) <- attrp
	
	vp2 <- prop.table(v, dimtotal2, na.rm=TRUE)
	attrp <- attributes(vp2)
	p2 <- paste(round(vp2*100, digits=1), "%", sep="")
	attributes(p2) <- attrp
	
	v <- round(v, 1)
	align <- paste(c("l", rep("r", dim(v)[2])), collapse="")
	
	cat("\\clearpage")
	printQLatex(get_qtext(Qtext))
	
#  cat("\n\n")
	cat("\\vspace{1 pc}\n")
	cat("\nWeighted totals\n\n")
	caption <- "Weighted totals"
	
	print(xtable(as.table(v),
					table.placement="!h",
					align=align,
					digits=1),
			caption.placement="top",
			floating=FALSE)
	cat("\\vspace{2 pc}\n")
	
	cat("\nPercentage of Row\n\n")
	caption <- "Percentage of Row"
	print(xtable(as.table(p1),
					align=align,
					digits=1,
					table.placement="!h"),
			caption.placement="top",
			floating=FALSE)
	cat("\\vspace{2 pc}\n")
	
	cat("\nPercentage of Column\n\n")
	caption <- "Percentage of Column"
	print(xtable(as.table(p2),
					align=align,
					digits=1,
					table.placement="!h"),
			caption.placement="top",
			floating=FALSE)
	cat("\\vspace{2 pc}\n")
	
}

