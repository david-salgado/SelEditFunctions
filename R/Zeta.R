#' \code{zeta} computes the function \eqn{\zeta}
#'
#' @param p Numeric vector with the probabilities of measurement error of each statistical unit.
#'
#' @param nu Numeric vector with the prediction error standard deviation of each statistical unit.
#'
#' @param sigma Numeric vector with the observation error standard deviation of each statistical
#' unit.
#'
#' @param u Numeric vector with the predicted measurement error (observed value minus predicted
#' value) for each statistical unit.
#'
#' @return Numeric vector with the value of the zeta function for each statistical unit.
#'
#'
#' @examples
#' \dontrun{
#'
#' AbsLossPar <- new(Class = 'AbsLossErrorMomentParam',
#'                   VarNames =  c("CifraNeg_13.___", "Personal_07.__2.__"),
#'                   Homoskedastic = c(FALSE, FALSE),
#'                   UnitWeightNames = character(0))
#'
#' ComputeErrorMoments(ObsPredPar, AbsLossPar)
#'
#' }
#'
#' @export
Zeta <- function(p, nu, sigma, u){

    Lengths <- c(length(p), length(nu), length(sigma), length(u))

    if (length(unique(Lengths)) != 1) stop('[SelEditFunctions: Zeta] All arguments must have the same length.\n')

    output <- 1/(1 + (1 - p)/p * sqrt((sigma^2 + nu^2) / nu^2) * exp(-(sigma^2 * u^2) / (sigma^2 + nu^2)))
    output[abs(sigma) <= .Machine$double.eps] <- p[abs(sigma) <= .Machine$double.eps]
    output[abs(p) <= .Machine$double.eps] <- 1
    output[abs(nu) <= .Machine$double.eps] <- 1
    return(output)

}