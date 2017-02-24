#' \code{AbsLossErrorMoment} computes the conditional error moments
#'
#'
#' @param Var Numeric vector with the raw values of the object variable.
#'
#' @param Pred Numeric vector with the predicted values for the object variable
#'
#' @param nu Numeric vector with the prediction error standard deviation of each statistical unit.
#'
#' @param sigma Numeric vector with the observation error standard deviation of each statistical
#' unit.
#'
#' @param w Numeric vector with the design weights.
#'
#' @param p Numeric vector with the probabilities of measurement error of each statistical unit.
#'
#' @param Homoskedastic \code{TRUE} (default) or \code{FALSE} indicating whether to use a
#' homoskedastic or heteroskedastic model.
#'
#' @return Numeric vector with the values moments for each statistical unit.
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
#' @include Zeta.R KummerM.R
#'
#' @export
AbsLossErrorMoment <- function(Var, Pred, nu, sigma, w, p, Homoskedastic = TRUE){

    u.Zeta <- (Var - Pred) / nu
    zetaValues <- Zeta(p, nu, sigma, u.Zeta)

    u2.Kummer <- if (Homoskedastic) {

        -(Var - Pred)^2 / (2 * nu^2)

    } else {

        -(w^2 * (Var - Pred)^2 / (2 * nu^2))
    }
    output <- if (Homoskedastic) {

        w *  sqrt(2 / pi) * nu * KummerM(u2.Kummer) * zetaValues

    } else {

        sqrt(2 / pi) * nu * KummerM(u2.Kummer) * zetaValues

    }
    return(output)
}