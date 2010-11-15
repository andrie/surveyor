



################################################################################
###  Import and export to Excel via Clipboard
################################################################################



write.excel <- function(tab, ...) write.table( tab, "clipboard", sep="\t", row.names=F)




