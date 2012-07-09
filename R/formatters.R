

#' Formats numeric value as percentage.
#' 
#' Multiplies value by 100 and appends % sign, e.g. 0.23 -> "23%"
#'
#' @param x Numeric vector
#' @param digits Numeric. Number of digits.
#' @export 
#' @family Formatting functions
formatPercent <- function(x, digits=1){
  format <- paste("%.", digits, "f%%", sep="")
  xt <- sprintf(format, x*100)
  xt[xt=="NA%"] <- ""
  xt
}


#' Finds first significant digit.
#' 
#' Finds first significant digit
#' 
#' @param x Numeric vector
#' @family Formatting functions
firstSignif <- function(x){
  p <- 10:-10
  sapply(x, function(xt)
        ifelse(xt==0, 
          0,
          p[which(abs(xt) >= 10^(p-1) & abs(xt)<10^(p))]
      )
  )
}

#' Rounds to level of first significant digit.
#'
#' Rounds to level of first significant digit
#'  
#' @param x Numeric vector
#' @param f Rounding function, e.g. round, trunc, floor or ceiling
#' @family Formatting functions
roundFirstSignif <- function(x, f=round){
  round_any(x, 10^(firstSignif(x)-1), f)
}


#' Rounds to specified level of precision.
#'
#' By default, rounds to one digit.
#'  
#' @param x Numeric vector
#' @param digits Number of digits, defaults to 1
#' @export 
#' @family Formatting functions
formatRound <- function(x, digits=1){
  round(x, digits)
}

#' Rounds to specified level of precision and prefixes a currency symbol.
#'
#' @param x Numeric vector
#' @param digits Passed to format
#' @param nsmall Passed to format
#' @param symbol Prepended to formatted string
#' @export 
#' @keywords internal
#' @family Formatting functions
formatPound <- function(x, digits=2, nsmall=2, symbol="£"){
  paste(symbol, format(x, digits=digits, nsmall=nsmall))
}
