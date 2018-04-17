#' Función logit.
#'
#' \code{Logit} calcula el logit de cada una de las componentes del vector especificado en el
#' parámetro de entrada \code{p}. El parámetro de entrada \code{adjust} reduce en esta cantidad la
#' longitud del dominio [0,1] para evitar las singularidades en 0 y en 1.
#'
#' @param p \code{Vector} de tipo \code{numeric} con los datos para los que queremos calcular el
#' logit.
#' 
#' @param adjust \code{Vector} de tipo \code{numeric} de longitud 1. Por defecto toma el valor
#' \code{10^-15}.
#' 
#' @return \code{Vector} de tipo \code{numeric} de longitud igual al vector de entrada \code{p} con
#' los logits de cada uno de los elementos en dicho vector.
#'
#' @examples
#' Logit(c(0.1, 0.3, 0.5, 0, -0.1, 0.1))
#' 
#' @export
Logit <- function(p, adjust = 1e-15){
    range.p <- range(p, na.rm = TRUE)
    if (all(is.na(p))) {
      
      output <- rep(NA_real_, length(p))
      
    } else {
      
      if (min(range.p, na.rm = T) < 0 | max(range.p, na.rm = T) > 1) stop('[Logit] Sólo se permiten valores de p en el intervalo [0,1].') 
      if (length(adjust) !=1) stop('[Logit] El parámetro adjust debe tener longitud 1.')
      a <- 1 - 2 * adjust
      output<-Log((0.5 + a * (p - 0.5))/(1 - (0.5 + a * (p - 0.5))))
      
    }
    
    return(output)
}
