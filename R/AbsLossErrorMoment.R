#' \code{AbsLossErrorMoment} computes the conditional error moments
#'
#'
#' @param Raw Numeric vector with the raw values of the object variable.
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
#'
#' AbsLossErrorMoment(10000, 12000, 2500, 5000, 1.9, 0.75)
#'
#' @include Zeta.R KummerM.R
#'
#' @export
AbsLossErrorMoment <- function(Raw, Pred, nu, sigma, w, p, Homoskedastic = TRUE){

    output <- numeric(length(Raw))

    u.Zeta <- if (Homoskedastic) {

      (Raw - Pred) / nu

    } else {

      w * (Raw - Pred) / nu
    }
    zetaValues <- Zeta(p, nu, sigma, u.Zeta)


    u2.Kummer <- if (Homoskedastic) {

        -(sigma^2 / (sigma^2 + nu^2))^2 * (Raw - Pred)^2 / (2 * nu^2)

    } else {

        -(w^2 * (sigma^2 / (sigma^2 + nu^2))^2 * (Raw - Pred)^2 / (2 * nu^2))
    }

    aux <- if (Homoskedastic) {

        w *  sqrt(2 / pi) * nu * KummerM(u2.Kummer) * zetaValues

    } else {

        sqrt(2 / pi) * nu * KummerM(u2.Kummer) * zetaValues

    }

    output[nu > .Machine$double.xmin & !is.na(nu)] <- aux[nu > .Machine$double.xmin & !is.na(nu)]

    return(output)
}