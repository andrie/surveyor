

#' Formats numeric value as percentage.
#' 
#' Multiplies value by 100 and appends % sign, e.g. 0.23 -> "23%"
#'
#' @param x Numeric vector
#' @param digits Numeric. Number of digits.
#' @export 
#' @keywords string
paste_percent <- function(x, digits=1){
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
#' @keywords internal
first_signif <- function(x){
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
#' @keywords internal
round_first_signif <- function(x, f=round){
  round_any(x, 10^(first_signif(x)-1), f)
}


#' Rounds to specified level of precision.
#'
#' By default, rounds to one digit.
#'  
#' @param x Numeric vector
#' @param n Number of digits, defaults to 1
#' @export 
#' @keywords internal
format_round <- function(x, n=1){
  round(x, n)
}
