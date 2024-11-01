#' Funcion para el calculo del AUC y ROC
#'
#' @description Esta funcion calcula el area debajo de la curva (AUC) y los puntos de la curva ROC
#' @param df será un data frame/matriz que contendrá al menos una columna de clase numeric y otra columna binaria de clase factor
#' @return Una lista de clase numeric (el area y los pares de puntos del ROC)
#' @examples
#' set.seed(123)
#' data_frame <- data.frame(
#' Variable1 = rnorm(100),  # 100 números aleatorios
#' VariableBinaria = factor(sample(c(0, 1), 100, replace = TRUE)))
#' AUC(data_frame)
AUC <- function(df) {
  # df: primera col --> vector numérico; segunda col --> vector lógico/binario
  
  # Identificar la columna binaria (la que tiene dos niveles)
  bin_col_index <- which(sapply(df, function(x) is.factor(x) && length(levels(x)) == 2))[1]
  
  # Si no hay una columna binaria, retornar un error
  if (is.na(bin_col_index)) {
    stop("No hay una columna binaria en el dataframe.")
  }
  
  # Ordenar el data frame de menor a mayor valor de la primera col
  df_orden <- df[order(df[, 1]), ]
  
  # Convertir la columna binaria a valores lógicos
  clase_col <- df_orden[[bin_col_index]]
  clase_logica <- as.logical(ifelse(clase_col == levels(clase_col)[1], FALSE, TRUE))  # Asignar FALSE y TRUE
  
  
  
  # Crear un vector de valores de corte (usamos todos los valores únicos de la primera columna)
  val_corte <- unique(df_orden[, 1])
  
  # Inicializar matriz para guardar los pares de (FPR, TPR)
  result <- matrix(nrow = length(val_corte), ncol = 2)
  
  for (i in seq_along(val_corte)) {
    # Conseguir un vector lógico comparando cada valor de la columna con el valor de corte
    v_logico <- df_orden[, 1] > val_corte[i]
    
    # Calcular True Positives (TP)
    TP <- sum(v_logico & clase_logica)
    
    # Calcular True Negatives (TN)
    TN <- sum(!v_logico & !clase_logica)
    
    # Calcular False Negatives (FN)
    FN <- sum(!v_logico & clase_logica)
    
    # Calcular False Positives (FP)
    FP <- sum(v_logico & !clase_logica)
    
    # Calcular True Positive Rate (TPR)
    TPR <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
    
    # Calcular False Positive Rate (FPR)
    FPR <- ifelse((FP + TN) == 0, 0, FP / (FP + TN))
    
    # Guardar el TPR y FPR en la matriz
    result[i, ] <- c(FPR, TPR)
  }
  
  # Añadir (0, 0) y (1, 1) si no están presentes
  if (!any(result[, 1] == 0 & result[, 2] == 0)) {
    result <- rbind(c(0, 0), result)
  }
  
  if (!any(result[, 1] == 1 & result[, 2] == 1)) {
    result <- rbind(result, c(1, 1))
  }
  
  #ordenar los resultados segun el valor de la primera columna de menor a mayor
  
  result <- result[order(result[, 1]), ]
  # Calcular el AUC utilizando la regla del trapecio
  area <- 0
  for (k in 2:nrow(result)) {
    # Calcular la anchura del intervalo
    h <- abs(result[k-1, 1] - result[k, 1])
    # Aplicar la regla del trapecio
    area <- area + 0.5 * (result[k-1, 2] + result[k, 2]) * h
    
  }
  
  return(list(result = result, area = area))
}