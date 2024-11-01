#' Funcion para calcular la entropia
#'
#' @description Esta funcion calcula la entropia de un vector de clase factor
#' @param x será un vector de clase factor
#' @return un valor de clase numeric
#' @examples
#' data_factor <- factor(c("A", "B", "A", "C", "B", "A", "D", "D", "C", "A"))
#' entropy(data_factor)

entropy <- function(x) { # x es un factor
  niveles <- levels(x)
  p <- c() #vector de probabilidades
  for (i in 1:length(niveles)) {
    counter <- 0
    for (j in 1:length(x)) {
      if (niveles[i]==x[j]) {
        counter <- counter +1
      }
    }
    p <- c(p, counter/length(x))
  }

  H<- 0
  for (i in 1:length(p)) {
    H <- H -p[i]*log2(p[i]+1e-10)
  }
  return(H)
}

#' Funcion para calcular la entropia de cada columna de un data frame
#'
#' @description Esta funcion calcula la entropia de cada columna de un data frame de clase factor
#' @param x será un dataframe de clase factor
#' @details utiliza la funcion \code{\link{entropy}} internamente
#' @return un vector de clase numeric
#' @examples
#' data_frame <- data.frame(Color = factor(c("Rojo", "Verde", "Azul", "Rojo", "Verde", "Verde")),
#' Forma = factor(c("Círculo", "Cuadrado", "Círculo", "Cuadrado", "Cuadrado", "Círculo")),
#' Tamaño = factor(c("Grande", "Pequeño", "Pequeño", "Grande", "Grande", "Pequeño")))
#' entropy_df(data_frame)

entropy_df <- function(x) { #x es un data frame
  v_entropy <- sapply(x, FUN=entropy)
  return(v_entropy)
}


#' Funcion para calcular la entropia normalizada
#'
#' @description Esta funcion calcula la entropia normalizada de un vector de clase factor
#' @param x será un vector de clase factor
#' @details utiliza la función \code{\link{entropy}} internamente para calcular la entropia del vector
#' @return un valor de clase numeric
#' @examples
#' data_factor <- factor(c("A", "B", "A", "C", "B", "A", "D", "D", "C", "A"))
#' norm_entropy(data_factor)
norm_entropy <- function(x) { #x es un factor
  H <- entropy(x)
  n <- length(x)
  H_norm <- H/log2(n)
  return(H_norm)
}

#' Funcion para calcular la entropia normalizada de cada columna de un data frame
#'
#' @description Esta funcion calcula la entropia normalizada de cada columna de un data frame de clase factor
#' @param x será un dataframe de clase factor
#' @details utiliza la funcion \code{\link{norm_entropy}} internamente
#' @return un vector de clase numeric
#' @examples
#' data_frame <- data.frame(Color = factor(c("Rojo", "Verde", "Azul", "Rojo", "Verde", "Verde")),
#' Forma = factor(c("Círculo", "Cuadrado", "Círculo", "Cuadrado", "Cuadrado", "Círculo")),
#' Tamaño = factor(c("Grande", "Pequeño", "Pequeño", "Grande", "Grande", "Pequeño")))
#' norm_entropy_df(data_frame)
norm_entropy_df <- function(df) {
  df_entropy_norm <- as.numeric(sapply(df, FUN=norm_entropy))
  return(df_entropy_norm)
}

#' Funcion que partiendo de un dataset obtiene uno nuevo donde todas las variables cumplen un requisito
#'
#' @description Esta funcion consigue un nuevo dataset donde las variables tengan una entropia mayor que la que hemos fijado
#' @param x será un dataframe de clase factor
#' @param H_umbral es un valor de clase numeric
#' @details utiliza la funcion \code{\link{entropy_df}} internamente
#' @return un dataframe de clase factor
#' @examples
#' data_frame <- data.frame(Color = factor(c("Rojo", "Verde", "Azul", "Rojo", "Verde", "Verde")),
#' Forma = factor(c("Círculo", "Cuadrado", "Círculo", "Cuadrado", "Cuadrado", "Círculo")),
#' Tamaño = factor(c("Grande", "Pequeño", "Pequeño", "Grande", "Grande", "Pequeño")))
#' filtro_entropia(data_frame, 1)
filtro_entropia <- function(x, H_umbral) {#x es un data set y H_umbral es el valor umbral de la entropia
  entropy_x <- entropy_df(x) #calcula las entropias de cada columna y nos devuelve un vector
  n_col <- ncol(x)

  #ver que columnas estan por encima del umbral
  true_cols <- entropy_x > H_umbral #nos devuelve un vector logico: TRUE si es mas grande y FALSE si no

  #conseguir los indices de los TRUE
  indices <- c() #inicializar el vector de los indices
  for (i in 1:n_col) {
    if (true_cols[i]==TRUE) {
      indices <- c(indices, i)
    }
  }

  #filtrar el data set
  x_filtrado <- x[,indices]

  return(x_filtrado)
}
