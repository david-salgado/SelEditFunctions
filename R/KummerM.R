#' \code{KummerM} is a wrapper for the Kummer function in package fAsianOptions
#'
#' @param x Numeric vector.
#'
#' @param KummerLim Numeric value to control passing large numeric arguments to \code{kummerM}.
#'
#' @return Numeric vector with the value of the Kummer M function for each statistical unit.
#'
#'
#' @examples
#' KummerM(-23.2)
#' KummerM(-1200)
#'
#' @export
KummerM <- function(x, KummerLim = 50){

    output <- rep(NA_real_, length(x))
    output[!is.na(x) & abs(x) < KummerLim] <- Re(fAsianOptions::kummerM(x[!is.na(x) & abs(x) < KummerLim], -0.5, 0.5))
    output[!is.na(x) & abs(x) >= KummerLim] <- sqrt(pi * abs(x)[!is.na(x) & abs(x) >= KummerLim])
    return(output)
}