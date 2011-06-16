

#' Appends a percentage sign to numeric vector
#' 
#' Turns a numeric vector into a character vector
#'
#' @keywords internal
#' @keywords string
#' @param x Numeric vector
paste_percent <- function(x){
  xt <- sprintf("%.1f%%", x*100)
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
  sapply(x, function(xt){
        if(xt==0){
          0
        } else {
          p <- 10:-10
          upper <- 10^(p)
          lower <- 10^(p-1)
          p[which(abs(xt) >=lower & abs(xt)<upper)]
        }
      }
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
#' @keywords internal
format_round <- function(x, n=1){
  round(x, n)
}
