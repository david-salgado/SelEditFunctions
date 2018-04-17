#' Cálculo del (coseno al cuadrado del) ángulo entre dos vectores.
#'
#' \code{Angle} calcula el coseno cuadrado del ángulo entre dos vectores.
#'
#' @param vector1 \code{Vector} de tipo \code{numeric} con las componentes del primer vector.
#' 
#' @param vector2 \code{Vector} de tipo \code{numeric} con las componentes del segundo vector.
#' 
#' @return \code{vector} de tipo \code{numeric} de longitud 1 con el valor del coseno al cuadrado
#' del ángulo entre los vectores \code{vector1} y \code{vector2}.
#' 
#' @examples
#' Angle(c(1, 3, 5), c(0, -1, 1))
#' 
#' @export
Angle <- function(vector1, vector2){
    
    if (length(vector1) != length(vector2)) stop("[Angle vector] vector1 y vector2 deben tener la misma longitud.")
    output <- 1
    
    vector1 <- as.numeric(vector1)
    vector2 <- as.numeric(vector2)
    
    den1 <- sum(vector1 * vector1, na.rm = T)
    den2 <- sum(vector2 * vector2, na.rm = T)
    num <- sum(vector1 * vector2, na.rm = T)^2
    
    OnlyANullFactor <- ( abs(den1 * den2) <= .Machine$double.eps & ( abs(den1) > .Machine$double.eps | abs(den2) > .Machine$double.eps ) )
    if (OnlyANullFactor) return(1)
    
    NoNull <- (abs(den1 * den2) > .Machine$double.eps) 
    if (!NoNull) return(1)
    aux <- num / (den1 * den2)
    
    return(aux)
}
