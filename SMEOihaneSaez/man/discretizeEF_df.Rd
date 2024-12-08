% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discretizacion.R
\name{discretizeEF_df}
\alias{discretizeEF_df}
\title{Funcion de discretizacion Equal Frecuency para cada columna de un data frame}
\usage{
discretizeEF_df(x, num.bins)
}
\arguments{
\item{x}{será un data frame de clase numeric}

\item{num.bins}{es un numero que indica cuantos intervalos queremos de clase numeric}
}
\value{
Un data frame de clase factor
}
\description{
Esta funcion discretiza las columnas numericas de un data frame en intervalos que contengan el mismo numero de elementos
}
\examples{
data <- data.frame(Variable1 = c(2.5, 3.6, 5.1, 8.4, 9.7, 2.3, 7.5, 6.2),
Variable2 = c(15.2, 18.4, 20.1, 14.5, 12.3, 17.6, 21.8, 19.2))
discretizeEF_df(data, num.bins = 3)
}
