% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_analisis.R
\name{norm_df}
\alias{norm_df}
\title{Funcion de normalizacion de las columnas de un data frame}
\usage{
norm_df(df)
}
\arguments{
\item{df}{será un data frame de clase numeric}
}
\value{
Un data frame de clase numeric
}
\description{
Esta funcion normaliza las columnas de un data frame
}
\details{
utiliza la funcion \code{\link{norm}} internamente
}
\examples{
df <- data.frame(columna1 = c(10, 20, 30, 40, 50),
columna2 = c(5, 15, 25, 35, 45),
columna3 = c(2, 4, 6, 8, 10))
norm_df(df)
}
