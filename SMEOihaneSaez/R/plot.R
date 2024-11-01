#' Funcion para visualizar la curva ROC
#'
#' @description Esta funcion calcula la Tasa de Falsos positivos (FPR) y la Tasa de Verdaderos Positivos (TPR)
#' para cada valor de corte y los grafica junto con la curva del random chance
#' @param x será un data frame con una columna lógica o binaria y las demás numéricas
#' @details Utiliza la funcion \code{\link{AUC}} dentro de la funcion para la visualizacion [mira la documentación correspondiente]
#' @return Un gráfico de la curva ROC
#' @examples
#' set.seed(123)
#' data_frame <- data.frame(
#' Variable1 = rnorm(100),  # 100 números aleatorios
#' VariableBinaria = factor(sample(c(0, 1), 100, replace = TRUE)))
#' plot_AUC(data_frame)

plot_AUC <- function(x) { #x será una matriz/ data frame
  data_AUC <- AUC(x)
  plot(data_AUC$result, type="l", col="blue", main="La curva ROC", xlab="Tasa de Falsos Positivos (FPR)",
       ylab="Tasa de Verdaderos Positivos (TPR)", xlim=c(0, 1), ylim=c(0,1))
  abline(a=0, b=1, col="red", lty=2) #la linea que define la probabilidad del 50% (es decir random)
  legend("bottomright",               # Especificar la posición de la leyenda
         legend = c("Curva ROC", "Random Chance"), # Textos de la leyenda
         col = c("blue", "red"),      # Colores de las líneas
         lty = c(1, 2),                # Estilos de las líneas (1 para sólida, 2 para punteada)
         lwd = 2)                     # Ancho de las líneas

}

#' Funcion para visualizar la entropia normalizada de las variables de un data set
#'
#' @description Esta funcion calcula la entropia normalizada para luego poder hacer un grafico de barras con cada variable
#' @param df será un data frame con variables categóricas de tipo factor
#' @details Utiliza la funcion \code{\link{norm_entropy_df}} dentro de la funcion para la visualizacion [mira la documentación correspondiente]
#' @return Un gráfico de barras para la entropia normalizada de cada variable
#' @examples
#' data <- data.frame(Columna_A = as.factor(c("Rojo", "Rojo", "Azul", "Verde", "Rojo", "Azul", "Verde", "Verde")),
#' Columna_B = as.factor(c("Alto", "Medio", "Medio", "Bajo", "Alto", "Bajo", "Alto", "Medio")),
#' Columna_C = as.factor(c("Bajo", "Alto", "Medio", "Bajo", "Medio", "Bajo", "Alto", "Medio")))
#' plot_entropy(data)

plot_entropy <- function(df) { #data frame de categorias
  H_norm <- norm_entropy_df(df)
  n<- length(H_norm)
  names_bar <- seq(n)
  barplot(H_norm, xlab="Número de variables", ylab="Entropía normalizada",
          main="Diagrama de barras para la entropía normalizada de cada variable",
          space=0, col="orange", las=1, names.arg=names_bar)

}

#' Funcion para visualizar el heatmap de una matriz de correlaciones
#'
#' @description Esta funcion calcula la matriz de correlaciones para luego poder hacer un mapa de calor
#' @param x será un data frame / matriz con variables numéricas
#' @details Utiliza la funcion \code{\link{cor_fun}} dentro de la funcion para la visualizacion [mira la documentación correspondiente]
#' @return Un mapa de calor para la matriz de correlaciones
#' @examples
#' set.seed(123)
#' datos_ejemplo <- data.frame(
#' Var1 = rnorm(100),
#' Var2 = rnorm(100),
#'  Var3 = rnorm(100),
#'  Var4 = rnorm(100))
#'  plot_cor(datos_ejemplo)
plot_cor <- function(x) { #x es un conjunto de datos matriz / data frame
  correlaciones <- cor_fun(x)
  nombres_columnas <- colnames(x)
  heatmap(correlaciones, main="Mapa de calor para la matriz de correlaciones",
          Rowv = NA, Colv = NA, labRow = nombres_columnas, labCol = nombres_columnas )
}


#' Funcion para visualizar la informacion mutua de un data set
#'
#' @description Esta funcion calcula la matriz de la información mutua para luego poder hacer un mapa de calor
#' @param x será un data frame / matriz con variables categóricas de tipo factor
#' @details Utiliza la funcion \code{\link{info_mutua}} dentro de la funcion para la visualizacion [mira la documentación correspondiente]
#' @return Un mapa de calor para la matriz de informacion mutua
#' @examples
#' data <- data.frame(Columna_A = as.factor(c("Rojo", "Rojo", "Azul", "Verde", "Rojo", "Azul", "Verde", "Verde")),
#' Columna_B = as.factor(c("Alto", "Medio", "Medio", "Bajo", "Alto", "Bajo", "Alto", "Medio")),
#' Columna_C = as.factor(c("Bajo", "Alto", "Medio", "Bajo", "Medio", "Bajo", "Alto", "Medio")))
#' plot_infomutua(data)
plot_infomutua <- function(x) {#x es un conjunto de datos categoricos
  matriz_im <- info_mutua(x)
  nombres_columnas <- colnames(x)
  heatmap(matriz_im, main="Mapa de calor para la matriz de información mutua",
          Rowv = NA, Colv = NA, labRow = nombres_columnas, labCol = nombres_columnas )
}
