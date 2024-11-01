## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
discretizeEW <- function(x, num.bins) {
  # Encontrar el valor mínimo y máximo de x
  num_max <- max(x)
  num_min <- min(x)

  # Calcular el tamaño del intervalo
  length_lim <- (num_max - num_min) / num.bins

  # Crear los puntos de corte
  puntos_de_corte <- num_min + (1:(num.bins - 1)) * length_lim

  # Inicializar el vector categórico
  vector_categ <- character(length(x))

  # Asignar categorías
  for (i in 1:length(x)) {
    if (x[i] <= min(puntos_de_corte)) {
      vector_categ[i] <- paste0("I(-infty,", min(puntos_de_corte), "]")
    } else if (x[i] >= max(puntos_de_corte)) {
      vector_categ[i] <- paste0("I[",max(puntos_de_corte), ", +infty)")
    } else {
      # Aquí se ajusta el bucle para incluir el punto de corte
      for (j in 2:(num.bins - 1)) {
        if (x[i] <= puntos_de_corte[j]) {
          vector_categ[i] <- paste0("I(", round(puntos_de_corte[j - 1], 2), ",", round(puntos_de_corte[j], 2), "]")
          break
        }
      }
    }
  }

  # Devuelve el vector categorico
  vector_categ<- as.factor(vector_categ)
  return(vector_categ)
}

## -----------------------------------------------------------------------------
a <- c(100, 50, 60, 10, 80, 20)
discretizeEW(a, 4)

## -----------------------------------------------------------------------------
discretizeEW_df <- function(x, num.bins) {
  df_dis<- as.data.frame(apply(x, num.bins, MARGIN=2, FUN=discretizeEW))
  return(df_dis)
}

## -----------------------------------------------------------------------------
data <- data.frame(Variable1 = c(2.5, 3.6, 5.1, 8.4, 9.7, 2.3, 7.5, 6.2),
Variable2 = c(15.2, 18.4, 20.1, 14.5, 12.3, 17.6, 21.8, 19.2))
discretizeEW_df(data, num.bins = 3)

## -----------------------------------------------------------------------------
discretizeEF <- function(x, num.bins) {

  #calcular el número de valores en cada intervalo
  num_val<- ceiling(length(x)/num.bins) #ceiling() para redondear el número al valor más grande
  x_orden <- sort(x) #ordeno los valores de menor a mayor

  # Inicializar el vector categórico
  vector_categ <- character(length(x))

  # Conseguir puntos de corte
  puntos_de_corte <- c()
  for (i in seq(num_val, length(x), by = num_val)) {
    if (length(puntos_de_corte)==num.bins-1){
      break
    }
    else {
      puntos_de_corte <- c(puntos_de_corte, x_orden[i])
    }
  }
  # Asignar categorías
  for (i in 1:length(x)) {
    if (x[i] <= min(puntos_de_corte)) {
      vector_categ[i] <- paste0("I(-infty,", min(puntos_de_corte), "]")
    } else if (x[i] > max(puntos_de_corte)) {
      vector_categ[i] <- paste0("I[",max(puntos_de_corte), ", +infty)")
    } else {
      # Aquí se ajusta el bucle para incluir el punto de corte
      for (j in 2:(num.bins - 1)) {
        if (x[i] <= puntos_de_corte[j]) {
          vector_categ[i] <- paste0("I(", round(puntos_de_corte[j - 1], 2), ",", round(puntos_de_corte[j], 2), "]")
          break
        }
      }
    }
  }

  # Devuelve vector categorico
  vector_categ<- as.factor(vector_categ)
  return(vector_categ)
}

## -----------------------------------------------------------------------------
a <- c(100, 50, 60, 10, 80, 20)
discretizeEF(a, 3)

## -----------------------------------------------------------------------------
discretizeEF_df <- function(x, num.bins) {
  df_dis<- as.data.frame(apply(x, num.bins, MARGIN=2, FUN=discretizeEF))
  return(df_dis)
}

## -----------------------------------------------------------------------------
data <- data.frame(Variable1 = c(2.5, 3.6, 5.1, 8.4, 9.7, 2.3, 7.5, 6.2),
Variable2 = c(15.2, 18.4, 20.1, 14.5, 12.3, 17.6, 21.8, 19.2))
discretizeEF_df(data, num.bins = 3)

## -----------------------------------------------------------------------------
discretize <- function(x, cut.points) {
  puntos_de_corte <- as.numeric(unlist(cut.points))

  # Inicializar el vector categórico
  vector_categ <- character(length(x))

  # Asignar categorías
  for (i in 1:length(x)) {
    if (x[i] <= min(puntos_de_corte)) {
      vector_categ[i] <- paste0("I(-infty,", min(puntos_de_corte), "]")
    } else if (x[i] >= max(puntos_de_corte)) {
      vector_categ[i] <- paste0("I[",max(puntos_de_corte), ", +infty)")
    } else {
      # Aquí se ajusta el bucle para incluir el punto de corte
      for (j in 2:length(puntos_de_corte)) {
        if (x[i] <= puntos_de_corte[j]) {
          vector_categ[i] <- paste0("I(", round(puntos_de_corte[j - 1], 2), ",", round(puntos_de_corte[j], 2), "]")
          break
        }
      }
    }
  }

  vector_categ<- as.factor(vector_categ)
  return(vector_categ)

}

## -----------------------------------------------------------------------------
a <- c(100, 50, 60, 10, 80, 20)
puntoscorte<- list(32.5, 55.0, 77.5)
discretize(a, puntoscorte)

## -----------------------------------------------------------------------------
estandar <- function(v) {
  if (!is.numeric(v)) {
    stop("Error: El vector no es numérico.")
  }

  v_est <- (v-mean(v))/sd(v)
  return(v_est)
}

## -----------------------------------------------------------------------------
vector <- c(10, 20, 30, 40, 50)
estandar(vector)

## -----------------------------------------------------------------------------
estandar_df <- function(df) {
  df_estandar <- as.data.frame(apply(df, MARGIN=2, FUN=estandar))
  return(df_estandar)
}

## -----------------------------------------------------------------------------
df <- data.frame(columna1 = c(10, 20, 30, 40, 50),
columna2 = c(5, 15, 25, 35, 45),
columna3 = c(2, 4, 6, 8, 10))
estandar_df(df)

## -----------------------------------------------------------------------------
norm <- function(v) {
  if (!is.numeric(v)) {
    stop("Error: El vector no es numérico.")
  }
  v_norm <- (v-min(v))/(max(v)-min(v))
  return(v_norm)
}

## -----------------------------------------------------------------------------
vector <- c(10, 20, 30, 40, 50)
norm(vector)

## -----------------------------------------------------------------------------
norm_df <- function(df) {
  df_norm <- as.data.frame(apply(df, MARGIN=2, FUN=norm))
  return(df_norm)
}

## -----------------------------------------------------------------------------
df <- data.frame(columna1 = c(10, 20, 30, 40, 50),
columna2 = c(5, 15, 25, 35, 45),
columna3 = c(2, 4, 6, 8, 10))
norm_df(df)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
vector <- c(10, 20, 30, 40, 50)
var_vector(vector)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
df <- data.frame(columna1 = c(10, 20, 30, 40, 50),
columna2 = c(5, 15, 25, 35, 45),
columna3 = c(2, 4, 6, 8, 10))
var(df)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
df <- data.frame(var1 = c(10, 20, 30, 40, 50),
var2 = c(5, 15, 25, 35, 45),
var3 = c(2, 4, 6, 8, 10))
cov_fun(df)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
df <- data.frame(var1 = c(10, 20, 30, 40, 50),
var2 = c(5, 15, 24, 35, 45),
var3 = c(2, 4, 6, 8, 9))
cor_fun(df)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
data_factor <- factor(c("A", "B", "A", "C", "B", "A", "D", "D", "C", "A"))
entropy(data_factor)

## -----------------------------------------------------------------------------
entropy_df <- function(x) { #x es un data frame
  v_entropy <- sapply(x, FUN=entropy)
  return(v_entropy)
}

## -----------------------------------------------------------------------------
data_frame <- data.frame(Color = factor(c("Rojo", "Verde", "Azul", "Rojo", "Verde", "Verde")),
Forma = factor(c("Círculo", "Cuadrado", "Círculo", "Cuadrado", "Cuadrado", "Círculo")),
Tamaño = factor(c("Grande", "Pequeño", "Pequeño", "Grande", "Grande", "Pequeño")))
entropy_df(data_frame)

## -----------------------------------------------------------------------------
norm_entropy <- function(x) { #x es un factor
  H <- entropy(x)
  n <- length(x)
  H_norm <- H/log2(n)
  return(H_norm)
}

## -----------------------------------------------------------------------------
data_factor <- factor(c("A", "B", "A", "C", "B", "A", "D", "D", "C", "A"))
norm_entropy(data_factor)

## -----------------------------------------------------------------------------
norm_entropy_df <- function(df) {
  df_entropy_norm <- as.numeric(sapply(df, FUN=norm_entropy))
  return(df_entropy_norm)
}

## -----------------------------------------------------------------------------
data_frame <- data.frame(Color = factor(c("Rojo", "Verde", "Azul", "Rojo", "Verde", "Verde")),
Forma = factor(c("Círculo", "Cuadrado", "Círculo", "Cuadrado", "Cuadrado", "Círculo")),
Tamaño = factor(c("Grande", "Pequeño", "Pequeño", "Grande", "Grande", "Pequeño")))
norm_entropy_df(data_frame)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
data_frame <- data.frame(Color = factor(c("Rojo", "Verde", "Azul", "Rojo", "Verde", "Verde")),
Forma = factor(c("Círculo", "Cuadrado", "Círculo", "Cuadrado", "Cuadrado", "Círculo")),
Tamaño = factor(c("Grande", "Pequeño", "Pequeño", "Grande", "Grande", "Pequeño")))
filtro_entropia(data_frame, 1)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
data <- data.frame(
Columna_A = as.factor(c("Rojo", "Rojo", "Azul", "Verde", "Rojo", "Azul", "Verde", "Verde")),
Columna_B = as.factor(c("Alto", "Medio", "Medio", "Bajo", "Alto", "Bajo", "Alto", "Medio")),
Columna_C = as.factor(c("Bajo", "Alto", "Medio", "Bajo", "Medio", "Bajo", "Alto", "Medio")))
info_mutua(data)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
set.seed(123)
data_frame <- data.frame(
Variable1 = rnorm(100),  # 100 números aleatorios
VariableBinaria = factor(sample(c(0, 1), 100, replace = TRUE)))
AUC(data_frame)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
data <- data.frame(Score = as.numeric(c(0.1, 0.4, 0.35, 0.6, 0.3, 0.75, 0.55, 0.8)),
Outcome = as.factor(c("Negative", "Negative", "Negative", "Positive", "Negative", "Positive", "Positive", "Positive")))
calcular_metricas(data)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
dataset <- data.frame(var_num1 = rnorm(100),                 # Variable numérica
var_num2 = rnorm(100, mean = 5),       # Otra variable numérica
var_cat1 = factor(sample(letters[1:3], 100, replace = TRUE)),  # Variable categórica con 3 categorías
var_cat2 = factor(sample(letters[1:4], 100, replace = TRUE)))   # Variable categórica con 4 categorías)
cor_infomutua(dataset)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
set.seed(123)
data_frame <- data.frame(
Variable1 = rnorm(100),  # 100 números aleatorios
VariableBinaria = factor(sample(c(0, 1), 100, replace = TRUE)))
plot_AUC(data_frame)

## -----------------------------------------------------------------------------
plot_entropy <- function(df) { #data frame de categorias
  H_norm <- norm_entropy_df(df)
  n<- length(H_norm)
  names_bar <- seq(n)
  barplot(H_norm, xlab="Número de variables", ylab="Entropía normalizada",
          main="Diagrama de barras para la entropía normalizada de cada variable",
          space=0, col="orange", las=1, names.arg=names_bar)

}

## -----------------------------------------------------------------------------
data <- data.frame(Columna_A = as.factor(c("Rojo", "Rojo", "Azul", "Verde", "Rojo", "Azul", "Verde", "Verde")),
Columna_B = as.factor(c("Círculo", "Cuadrado", "Círculo", "Cuadrado", "Cuadrado", "Círculo", "Triángulo", "Círculo")),
Columna_C = as.factor(c("Bajo", "Alto", "Medio", "Bajo", "Medio", "Bajo", "Alto", "Medio")))
plot_entropy(data)

## -----------------------------------------------------------------------------
plot_cor <- function(x) { #x es un conjunto de datos matriz / data frame
  correlaciones <- cor_fun(x)
  nombres_columnas <- colnames(x)
  heatmap(correlaciones, main="Mapa de calor para la matriz de correlaciones",
          Rowv = NA, Colv = NA, labRow = nombres_columnas, labCol = nombres_columnas )
}

## -----------------------------------------------------------------------------
set.seed(123)
datos_ejemplo <- data.frame(
Var1 = rnorm(100),
Var2 = rnorm(100),
Var3 = rnorm(100),
Var4 = rnorm(100))
plot_cor(datos_ejemplo)

## -----------------------------------------------------------------------------
plot_infomutua <- function(x) {#x es un conjunto de datos categoricos
  matriz_im <- info_mutua(x)
  nombres_columnas <- colnames(x)
  heatmap(matriz_im, main="Mapa de calor para la matriz de información mutua",
          Rowv = NA, Colv = NA, labRow = nombres_columnas, labCol = nombres_columnas )
}

## -----------------------------------------------------------------------------
data <- data.frame(Columna_A = as.factor(c("Rojo", "Rojo", "Azul", "Verde", "Rojo", "Azul", "Verde", "Verde")),
Columna_B = as.factor(c("Alto", "Medio", "Medio", "Bajo", "Alto", "Bajo", "Alto", "Medio")),
Columna_C = as.factor(c("Bajo", "Alto", "Medio", "Bajo", "Medio", "Bajo", "Alto", "Medio")))
plot_infomutua(data)

## -----------------------------------------------------------------------------
remove_duplicates <- function(data) {
  # Encontrar filas duplicadas
  duplicated_rows <- duplicated(data)
  
  # Filtrar el data frame para conservar solo filas únicas
  data_unique <- data[!duplicated_rows, ]
  
  # Devolver el data frame sin duplicados
  return(data_unique)
}

## -----------------------------------------------------------------------------
data_with_duplicates <- data.frame(
ID = c(1, 2, 2, 3, 4, 4, 5),
Value = c("A", "B", "B", "C", "D", "D", "E"))
remove_duplicates(data_with_duplicates)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
data <- data.frame(
ID = 1:10,
Value = c(5, 3, 8, 2, 6, 9, 1, 7, 4, 10))
split_data(data, train_ratio = 0.7, seed = 123)

