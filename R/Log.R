#' Cálculo del logarítmo para vectores.
#'
#' \code{log} calcula el logaritmo en la base indicada en el input \code{base} de cada una de las
#' componentes del vector especificado en el parámetro de entrada \code{x}.
#'
#' @param x \code{Vector} de tipo \code{numeric} con los datos para los que queremos calcular el
#' logaritmo.
#' 
#' @param base \code{Vector} de tipo \code{numeric} de longitud 1 con la base en la que queremos
#' calcular el logaritmo. Por defecto se toma el logaritmo natural.
#' 
#' @return \code{Vector} de tipo \code{numeric} de longitud igual al vector de entrada \code{x} con
#' los logaritmos en base \code{base} de cada uno de los elementos en dicho vector.
#'
#' @examples
#' Log(c(1, 3, 5, 0, -1, 1))
#' 
#' @export
Log <- function(x, base = exp(1)){
    x[!is.na(x) && x < 0] <- 1
    output <- log(x, base) 
    return(output)
}
