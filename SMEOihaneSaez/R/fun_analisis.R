#' Funcion de estandarizacion de un vector
#'
#' @description Esta funcion estandariza un vector numerico
#' @param v será un vector de clase numeric
#' @return Un vector de clase numeric
#' @examples
#' vector <- c(10, 20, 30, 40, 50)
#' estandar(vector)

estandar <- function(v) {
  if (!is.numeric(v)) {
    stop("Error: El vector no es numérico.")
  }

  v_est <- (v-mean(v))/sd(v)
  return(v_est)
}

#' Funcion de estandarizacion de las columnas de un data frame
#'
#' @description Esta funcion estandariza las columnas de un data frame
#' @param df será un data frame de clase numeric
#' @details utiliza la funcion \code{\link{estandar}} internamente
#' @return Un data frame de clase numeric
#' @examples
#' df <- data.frame(columna1 = c(10, 20, 30, 40, 50),
#' columna2 = c(5, 15, 25, 35, 45),
#' columna3 = c(2, 4, 6, 8, 10))
#' estandar_df(df)
estandar_df <- function(df) {
  df_estandar <- as.data.frame(apply(df, MARGIN=2, FUN=estandar))
  return(df_estandar)
}

#' Funcion de normalización de un vector
#'
#' @description Esta funcion normaliza un vector numerico
#' @param v será un vector de clase numeric
#' @return Un vector de clase numeric
#' @examples
#' vector <- c(10, 20, 30, 40, 50)
#' norm(vector)

norm <- function(v) {
  if (!is.numeric(v)) {
    stop("Error: El vector no es numérico.")
  }
  v_norm <- (v-min(v))/(max(v)-min(v))
  return(v_norm)
}

#' Funcion de normalizacion de las columnas de un data frame
#'
#' @description Esta funcion normaliza las columnas de un data frame
#' @param df será un data frame de clase numeric
#' @details utiliza la funcion \code{\link{norm}} internamente
#' @return Un data frame de clase numeric
#' @examples
#' df <- data.frame(columna1 = c(10, 20, 30, 40, 50),
#' columna2 = c(5, 15, 25, 35, 45),
#' columna3 = c(2, 4, 6, 8, 10))
#' norm_df(df)

norm_df <- function(df) {
  df_norm <- as.data.frame(apply(df, MARGIN=2, FUN=norm))
  return(df_norm)
}

#' Funcion de la varianza de un vector
#'
#' @description Esta funcion calcula la varianza de un vector numerico
#' @param x será un vector de clase numeric
#' @return Un valor de clase numeric
#' @examples
#' vector <- c(10, 20, 30, 40, 50)
#' var_vector(vector)

var_vector <- function(x) {
  if (!is.numeric(x)) {
    stop("Error: El vector no es numérico.")
  }

  #calcular la media
  media <- mean(x)

  #restar la media con cada elemento
  ken_media <- x-media

  #calcular la varianza
  varianza <- sum(ken_media^2)/(length(x)-1)

  return(varianza)
}

#' Funcion de la varianza de las columnas de un data frame/matriz
#'
#' @description Esta funcion calcula la varianza de las columnas de un data frame/matriz
#' @param x será un data frame/matriz de clase numeric
#' @return Un vector de clase numeric
#' @examples
#' df <- data.frame(columna1 = c(10, 20, 30, 40, 50),
#' columna2 = c(5, 15, 25, 35, 45),
#' columna3 = c(2, 4, 6, 8, 10))
#' var(df)

var <- function(x){ #x es una matriz/df
  for (i in 1:ncol(x)) {
    if (!is.numeric(x[,i])) {
      stop("Error: La matriz no es numérica.")
    }
  }
  nrow<- nrow(x)
  ncol <- ncol(x)

  #calcular la media de cada columna (nos devuelve un vector)
  mean_col <- apply(x, MARGIN=2, FUN=mean) #nos devuelve vector de medias de cada columna

  #restar la media de cada columna a cada elemento de esa columna
  ken_media <- sweep(x, MARGIN=2, mean_col, FUN="-")

  #calcular la varianza
  var <- colSums(ken_media^2)/(nrow-1)

  return(var)
}

#' Funcion de la covarianza de las columnas de un data frame/matriz
#'
#' @description Esta funcion calcula la covarianza de las columnas de un data frame/matriz
#' @param x será un data frame/matriz de clase numeric
#' @return Una matriz de clase numeric (la matriz de varianzas-covarianzas)
#' @examples
#' df <- data.frame(var1 = c(10, 20, 30, 40, 50),
#' var2 = c(5, 15, 25, 35, 45),
#' var3 = c(2, 4, 6, 8, 10))
#' cov_fun(df)
cov_fun <- function(x) { #nos devuelve una matriz de varianza-covarianza
  n_col <- ncol(x)
  n_row<- nrow(x)

  #calcular la media de cada columna (nos devuelve un vector)
  mean_col <- apply(x, MARGIN=2, FUN=mean) #nos devuelve vector de medias de cada columna

  #restar la media de cada columna a cada elemento de esa columna
  ken_media <- sweep(x, MARGIN=2, mean_col, FUN="-")

  matriz_covarianzas <- matrix(0, nrow = n_col, ncol = n_col)  # Inicializar matriz

  for (i in 1:n_col) {
    for (j in 1:n_col) {
      matriz_covarianzas[i, j] <- sum(ken_media[,i]*ken_media[,j])/(n_row-1) #calcular la covarianza
    }
  }
  return(matriz_covarianzas)
}

#' Funcion de la correlación de las columnas de un data frame/matriz
#'
#' @description Esta funcion calcula la correlación de las columnas de un data frame/matriz
#' @param matriz_dat será un data frame/matriz de clase numeric
#' @details utiliza la funcion \code{\link{cov_fun}} internamente
#' @return Una matriz de clase numeric (la matriz de correlaciones)
#' @examples
#' df <- data.frame(var1 = c(10, 20, 30, 40, 50),
#' var2 = c(5, 15, 25, 35, 45),
#' var3 = c(2, 4, 6, 8, 10))
#' cor_fun(df)

cor_fun <- function(matriz_dat) {
  n <- ncol(matriz_dat)  # Número de variables
  matriz_cov <- cov_fun(matriz_dat)
  matriz_correlaciones <- matrix(0, nrow = n, ncol = n)  # Inicializar matriz de correlaciones

  for (i in 1:n) {
    for (j in 1:n) {
      # Extraer la covarianza y las varianzas
      cov_xy <- matriz_cov[i, j]                   # Cov(X, Y)
      var_x <- matriz_cov[i, i]                    # Var(X)
      var_y <- matriz_cov[j, j]                    # Var(Y)

      # Calcular la correlación
      matriz_correlaciones[i, j] <- cov_xy / sqrt(var_x * var_y)
    }
  }

  return(matriz_correlaciones)
}

#' Funcion de la información mutua de las columnas de un data frame/matriz
#'
#' @description Esta funcion calcula la información mutua de las columnas de un data frame/matriz
#' @param x será un data frame/matriz de clase factor
#' @details utiliza la funcion \code{\link{entropy_df}} internamente para calcular las entropias de cada columna
#' @return Una matriz de clase numeric (la matriz de información mutua)
#' @examples
#' data <- data.frame(
#' Columna_A = as.factor(c("Rojo", "Rojo", "Azul", "Verde", "Rojo", "Azul", "Verde", "Verde")),
#' Columna_B = as.factor(c("Alto", "Medio", "Medio", "Bajo", "Alto", "Bajo", "Alto", "Medio")),
#' Columna_C = as.factor(c("Bajo", "Alto", "Medio", "Bajo", "Medio", "Bajo", "Alto", "Medio")))
#' info_mutua(data)
info_mutua <- function(x) {#correlación para variables categoricas x es un data frame / matriz categorica de factores
  #info mutua se calcula de la siguiente manera
  #I(X;Y)= sum_xsum_y p(x, y)log(p(x, y)/p(x)p(y))
  #donde X e Y son dos variables o columnas de un data set
  # o si lo expresamos mediante entropia
  #I(X;Y)= H(X)+H(Y)-H(X,Y)
  #donde H(x, y)= -sum_xsum_y p(x, y)log(p(x, y))

  n_col <- ncol(x) # número de variables
  n_row <- nrow(x)
  matriz_infomutua <- matrix(0, nrow=n_col, ncol=n_col) # inicializamos la matriz de resultados

  # Calculamos la entropía de cada variable
  H_variable <- entropy_df(x) # nos devuelve un vector de entropías de cada columna

  for (i in 1:n_col) {
    categ1 <- levels(x[[i]]) # los niveles o categorías en la columna i
    for (j in 1:n_col) {
      categ2 <- levels(x[[j]]) # los niveles o categorías en la columna j

      # Inicializar la matriz de probabilidades conjuntas
      p_x_y <- matrix(0, nrow=length(categ1), ncol=length(categ2))

      # Contar las ocurrencias de cada combinación de categoría
      for (m in 1:n_row) {
        cat1_index <- which(categ1 == x[m, i])  # Índice de la categoría en la columna i (un vector con el indice de la categoria en cada elemento)
        cat2_index <- which(categ2 == x[m, j])  # Índice de la categoría en la columna j

        # Asegurarse de que ambos índices sean válidos
        if (length(cat1_index) > 0 && length(cat2_index) > 0) {#compara no todos con todos si no el primer elemento con el primero el segundo con el segundo etc.
          p_x_y[cat1_index, cat2_index] <- p_x_y[cat1_index, cat2_index] + 1 # sumamos en cada sitio
        }
      }

      # Calcular las probabilidades conjuntas
      p_x_y <- p_x_y / n_row

      # Calcular H(X, Y) usando probabilidad conjunta
      H_XY <- -sum(p_x_y * log(p_x_y + 1e-10), na.rm = TRUE)  # Sumar una pequeña constante para evitar log(0)

      # Calcular I(X; Y) = H(X) + H(Y) - H(X, Y)

      matriz_infomutua[i, j] <- H_variable[i] + H_variable[j] - H_XY

    }
  }

  return(matriz_infomutua)
}
