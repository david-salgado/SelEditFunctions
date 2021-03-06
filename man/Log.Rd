% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Log.R
\name{Log}
\alias{Log}
\title{Cálculo del logarítmo para vectores.}
\usage{
Log(x, base = exp(1))
}
\arguments{
\item{x}{\code{Vector} de tipo \code{numeric} con los datos para los que queremos calcular el
logaritmo.}

\item{base}{\code{Vector} de tipo \code{numeric} de longitud 1 con la base en la que queremos
calcular el logaritmo. Por defecto se toma el logaritmo natural.}
}
\value{
\code{Vector} de tipo \code{numeric} de longitud igual al vector de entrada \code{x} con
los logaritmos en base \code{base} de cada uno de los elementos en dicho vector.
}
\description{
\code{log} calcula el logaritmo en la base indicada en el input \code{base} de cada una de las
componentes del vector especificado en el parámetro de entrada \code{x}.
}
\examples{
Log(c(1, 3, 5, 0, -1, 1))

}
