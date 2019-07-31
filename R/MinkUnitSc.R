#' \code{MinkUnitSc} computes the unit scores according to a Minkowskian function
#'
#' @param x Matrix with the item scores of each variable (columns) of each statistical unit (rows).
#'
#' @param alpha Integer vector of length 1 with the degree of the Minkowskian function.
#'
#' @param Weights Numeric vector with the weights of each item score.
#'
#' @return Numeric vector with the value of the Minkowskian function for each statistical unit.
#'
#'
#' @examples
#'
#' x <- cbind(c(0.434, 0.543, 1.445, 0.485), c(0.984, 1.032, 1.902, 0.840))
#' MinkUnitSc(x, 1, 1)
#'
#' @export
MinkUnitSc <- function(x, alpha = 1L, Weights = 1L){

    if (length(alpha) != 1) stop('[SelEditFunctions:: MinkUnitSc] The input alpha must be an integer vector of lenght 1.')

    output <- apply(x, 1, function(y){sum((Weights * y ^ alpha)) ^ (1 / alpha)})

    return(output)

}