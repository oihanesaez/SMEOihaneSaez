#' Funcion de discretizacion Equal Width
#'
#' @description Esta funcion discretiza un vector numerico en intervalos de longitud iguales
#' @param x será un vector de clase numeric
#' @param num.bins es un numero que indica cuantos intervalos queremos de clase numeric
#' @return Un vector de clase factor
#' @examples
#' a <- c(100, 50, 60, 10, 80, 20)
#' discretizeEW(a, 4)
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

#' Funcion de discretizacion Equal Width para cada columna de un data frame
#'
#' @description Esta funcion discretiza las columnas numericas de un data frame en intervalos de longitud iguales
#' @param x será un data frame de clase numeric
#' @param num.bins es un numero que indica cuantos intervalos queremos de clase numeric
#' @return Un data frame de clase factor
#' @examples
#' data <- data.frame(Variable1 = c(2.5, 3.6, 5.1, 8.4, 9.7, 2.3, 7.5, 6.2),
#' Variable2 = c(15.2, 18.4, 20.1, 14.5, 12.3, 17.6, 21.8, 19.2))
#' discretizeEW_df(data, num.bins = 3)
discretizeEW_df <- function(x, num.bins) {
  df_dis<- as.data.frame(apply(x, num.bins, MARGIN=2, FUN=discretizeEW))
  return(df_dis)
}


#' Funcion de discretizacion Equal Frecuency
#'
#' @description Esta funcion discretiza un vector numerico en intervalos que contengan el mismo numero de elementos
#' @param x será un vector de clase numeric
#' @param num.bins es un numero que indica cuantos intervalos queremos de clase numeric
#' @return Un vector de clase factor
#' @examples
#' a <- c(100, 50, 60, 10, 80, 20)
#' discretizeEF(a, 4)
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

#' Funcion de discretizacion Equal Frecuency para cada columna de un data frame
#'
#' @description Esta funcion discretiza las columnas numericas de un data frame en intervalos que contengan el mismo numero de elementos
#' @param x será un data frame de clase numeric
#' @param num.bins es un numero que indica cuantos intervalos queremos de clase numeric
#' @return Un data frame de clase factor
#' @examples
#' data <- data.frame(Variable1 = c(2.5, 3.6, 5.1, 8.4, 9.7, 2.3, 7.5, 6.2),
#' Variable2 = c(15.2, 18.4, 20.1, 14.5, 12.3, 17.6, 21.8, 19.2))
#' discretizeEF_df(data, num.bins = 3)
discretizeEF_df <- function(x, num.bins) {
  df_dis<- as.data.frame(apply(x, num.bins, MARGIN=2, FUN=discretizeEF))
  return(df_dis)
}

#' Funcion de discretizacion dado los puntos de corte
#'
#' @description Esta funcion discretiza un vector numerico en intervalos utilizando los puntos de corte dados
#' @param x será un vector de clase numeric
#' @param cut.points es una lista de puntos de corte de clase numeric
#' @return Un vector de clase factor
#' @examples
#' a <- c(100, 50, 60, 10, 80, 20)
#' puntoscorte<- list(32.5, 55.0, 77.5)
#' discretize(a, puntoscorte)
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
