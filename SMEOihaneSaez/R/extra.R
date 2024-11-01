#' Funcion para dividir dataset en entrenamiento y prueba
#'
#' @description Esta funcion divide el dataset en dos uno para el entrenamiento y otro para prueba
#' @param data será un dataframe/matriz de clase numeric
#' @param train_ratio indica el porcentaje de los datos originales que iran para el set de entrenamiento el default es 0.7
#' @param seed valor de tipo numeric para reproducibilidad
#' @return una lista de clase numeric
#' @examples
#' data <- data.frame(
#' ID = 1:10,
#' Value = c(5, 3, 8, 2, 6, 9, 1, 7, 4, 10))
#' split_data(data, train_ratio = 0.7, seed = 123)
split_data <- function(data, train_ratio = 0.7, seed = NULL) {
  # Si se especifica, establecer la semilla para reproducibilidad
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Número total de observaciones
  n <- nrow(data)

  # Determinar el número de muestras para el conjunto de entrenamiento
  train_size <- floor(train_ratio * n)

  # Obtener índices aleatorios para el conjunto de entrenamiento
  train_indices <- sample(seq_len(n), size = train_size)

  # Dividir el data frame
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]

  # Devolver los conjuntos como una lista
  return(list(train = train_data, test = test_data))
}

#' Funcion para encontrar y eliminar filas duplicadas en un data frame
#'
#' @description Esta funcion encuentra y elimina filas del dataset que están repetidas
#' @param data será un dataframe de cualquier clase
#' @return una dataframe de la misma clase que hemos proporcionado
#' @examples
#' data_with_duplicates <- data.frame(
#' ID = c(1, 2, 2, 3, 4, 4, 5),
#' Value = c("A", "B", "B", "C", "D", "D", "E"))
#' remove_duplicates(data_with_duplicates)
remove_duplicates <- function(data) {
  # Encontrar filas duplicadas
  duplicated_rows <- duplicated(data)

  # Filtrar el data frame para conservar solo filas únicas
  data_unique <- data[!duplicated_rows, ]

  # Devolver el data frame sin duplicados
  return(data_unique)
}
