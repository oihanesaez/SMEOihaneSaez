#' Funcion para el calculo de la varianza y AUC cuando la variable es numerica y la entropia si es categorica
#'
#' @description Esta funcion calcula el area debajo de la curva (AUC) y la varianza cuando la variable es numerica y calcula la entropia si la variable es categorica
#' @param df será un data frame que puede contener columnas de clase numeric y de clase factor
#' @details utiliza las funciones \code{\link{var_vector}}, \code{\link{AUC}} y \code{\link{entropy}} internamente
#' @return Un data frame de clase numeric
#' @examples
#' data <- data.frame(Score = as.numeric(c(0.1, 0.4, 0.35, 0.6, 0.3, 0.75, 0.55, 0.8)),
#' Outcome = as.factor(c("Negative", "Negative", "Negative", "Positive", "Negative", "Positive", "Positive", "Positive")))
#' calcular_metricas(data)

calcular_metricas <- function(df) {
  # Inicializar listas para resultados
  varianzas <- rep(NA, ncol(df))
  auc_values <- rep(NA, ncol(df))
  entropias <- rep(NA, ncol(df))

  # Iterar sobre cada columna del dataframe
  for (i in seq_along(df)) {
    col <- df[[i]]

    # Calcular varianza si la columna es numérica
    if (is.numeric(col)) {
      varianzas[i] <- var_vector(col)

      # Calcular AUC si hay una columna binaria
      bin_col_index <- which(sapply(df, function(x) is.factor(x) && length(levels(x)) == 2))[1]
      if (!is.na(bin_col_index)) {
        bin_col <- df[[bin_col_index]]
        df_auc <- data.frame(num_col = col, bin_col = bin_col)
        auc_values[i] <- AUC(df_auc)$area
      }
    }

    # Calcular entropía si la columna es categórica
    if (is.factor(col)) {
      entropias[i] <- entropy(col)
    }
  }

  # Crear el dataframe de resultados
  resultados <- data.frame(
    Variable = names(df),
    Varianza = varianzas,
    AUC = auc_values,
    Entropía = entropias
  )

  return(resultados)
}

#' Funcion para el calculo de la correlacion entre pares cuando las variables son numericas y la informacion mutua si son categoricas
#'
#' @description Esta funcion calcula la correlacion entre pares de variables cuando las variables son numericas y calcula la informacion mutua si las variables son categoricas.
#' Si una columna es numerica y otra categorica lo ignora y el resultado es NA
#' @param df será un data frame que puede contener columnas de clase numeric y de clase factor
#' @details utiliza las funciones \code{\link{cor_fun}} y \code{\link{info_mutua}} internamente
#' @return Una matriz de clase numeric
#' @examples
#' dataset <- data.frame(var_num1 = rnorm(100),                 # Variable numérica
#' var_num2 = rnorm(100, mean = 5),       # Otra variable numérica
#' var_cat1 = factor(sample(letters[1:3], 100, replace = TRUE)),  # Variable categórica con 3 categorías
#' var_cat2 = factor(sample(letters[1:4], 100, replace = TRUE)))   # Variable categórica con 4 categorías)
#' cor_infomutua(dataset)
cor_infomutua <- function(df) {#df es un data frame

  # Identifica el número de columnas y el tipo de cada columna
  n_col <- ncol(df)
  tipos <- sapply(df, class)

  # Matriz de resultados
  result_matrix <- matrix(NA, nrow = n_col, ncol = n_col)
  colnames(result_matrix) <- colnames(df)
  rownames(result_matrix) <- colnames(df)

  # Calcular correlaciones e información mutua entre cada par de columnas
  for (i in 1:n_col) {
    for (j in 1:n_col) {
      # Si ambas variables son numéricas, calcula correlación
      if (tipos[i] == "numeric" && tipos[j] == "numeric") {
        matriz_dat <- df[, c(i, j)]  # Matriz de datos para este par
        result_matrix[i, j] <- cor_fun(matriz_dat)[1,2]  # Correlación
      }
      # Si ambas variables son factores, calcula información mutua
      else if (tipos[i] == "factor" && tipos[j] == "factor") {
        matriz_categ <- df[, c(i, j)]
        result_matrix[i, j] <- info_mutua(matriz_categ)[1, 2]  # Información mutua
      }
      else { #cuando una columna es factor y otra numerica los ignoramos
        result_matrix[i,j] <- NA
      }
    }
  }

  return(result_matrix)
}

