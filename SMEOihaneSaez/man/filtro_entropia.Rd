% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entropia.R
\name{filtro_entropia}
\alias{filtro_entropia}
\title{Funcion que partiendo de un dataset obtiene uno nuevo donde todas las variables cumplen un requisito}
\usage{
filtro_entropia(x, H_umbral)
}
\arguments{
\item{x}{será un dataframe de clase factor}

\item{H_umbral}{es un valor de clase numeric}
}
\value{
un dataframe de clase factor
}
\description{
Esta funcion consigue un nuevo dataset donde las variables tengan una entropia mayor que la que hemos fijado
}
\details{
utiliza la funcion \code{\link{entropy_df}} internamente
}
\examples{
data_frame <- data.frame(Color = factor(c("Rojo", "Verde", "Azul", "Rojo", "Verde", "Verde")),
Forma = factor(c("Círculo", "Cuadrado", "Círculo", "Cuadrado", "Cuadrado", "Círculo")),
Tamaño = factor(c("Grande", "Pequeño", "Pequeño", "Grande", "Grande", "Pequeño")))
filtro_entropia(data_frame, 1)
}
