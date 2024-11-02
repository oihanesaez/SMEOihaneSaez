import numpy as np
import pandas as pd

def discretize_ew(x, num_bins):
    """
    Funcion que discretiza un vector numerico en intervalos de longitud iguales

    Parameters
    ----------
    x (float array): un vector numerico a discretizar
    num_bins (int) : el valor del número de intervalos que queremos

    Returns
    -------
    vector_categ (pd.Categorical): un vector de tipo categórico con los elementos del vector numerico discretizados
    
    Example:
        # Definir el vector de entrada
        a = np.array([100, 50, 60, 10, 80, 20])

        # Llamar a la función
        result = discretize_ew(a, 4)

        # Mostrar el resultado
        print(result)

    """
    # Encontrar el valor mínimo y máximo de x
    num_max = np.max(x)
    num_min = np.min(x)

    # Calcular el tamaño del intervalo
    length_lim = (num_max - num_min) / num_bins

    # Crear los puntos de corte
    puntos_de_corte = num_min + np.arange(1, num_bins) * length_lim

    # Inicializar el vector categórico
    vector_categ = []

    # Asignar categorías
    for value in x:
        if value <= puntos_de_corte[0]:
            vector_categ.append(f"I(-infty, {puntos_de_corte[0]:.2f}]")
        elif value >= puntos_de_corte[-1]:
            vector_categ.append(f"I[{puntos_de_corte[-1]:.2f}, +infty)")
        else:
            # Ajuste para incluir el punto de corte adecuado
            for j in range(1, len(puntos_de_corte)):
                if value <= puntos_de_corte[j]:
                    vector_categ.append(f"I({puntos_de_corte[j - 1]:.2f}, {puntos_de_corte[j]:.2f}]")
                    break

    # Convertir a factor/categoría
    vector_categ = pd.Categorical(vector_categ)
    return vector_categ


def discretize_ew_df(df, num_bins):
    """
    Funcion de discretizacion Equal Width para cada columna de un data frame

    Parameters
    ----------
    df (pd.DataFrame): data frame de clase numérico
    num_bins (int): el valor del número de intervalos que queremos

    Returns
    -------
    df_dis (pd.DataFrame): data frame de tipo categórico con los valores discretizados
    
    Example:
        # Crear el DataFrame
        data = pd.DataFrame({
            'Variable1': [2.5, 3.6, 5.1, 8.4, 9.7, 2.3, 7.5, 6.2],
            'Variable2': [15.2, 18.4, 20.1, 14.5, 12.3, 17.6, 21.8, 19.2]
        })

        # Llamar a la función para discretizar el DataFrame
        result_df = discretize_ew_df(data, num_bins=3)

        # Mostrar el resultado
        print(result_df)

    """
    # Aplicar la función discretize_ew a cada columna del DataFrame
    df_dis = df.apply(lambda x: discretize_ew(x, num_bins))
    return df_dis


def discretize_ef(x, num_bins):
    """
    Esta funcion discretiza un vector numerico en intervalos que contengan el mismo numero de elementos

    Parameters
    ----------
    x (array, float): es un vector numérico a discretizar
    num_bins (int): el valor del número de intervalos que queremos

    Returns
    -------
    vector_categ (array, pd.Categorical): es un vector categórico con los valores discretizados.
    
    Example:
        # Definir el vector de entrada
        a = np.array([100, 50, 60, 10, 80, 20])

        # Llamar a la función
        result = discretize_ef(a, 3)

        # Mostrar el resultado
        print(result)
    """
    # Calcular el número de valores en cada intervalo
    num_val = int(np.ceil(len(x) / num_bins))  # ceiling para redondear al valor más alto
    x_sorted = np.sort(x)  # Ordenar los valores de menor a mayor

    # Inicializar el vector categórico
    vector_categ = [''] * len(x)

    # Conseguir puntos de corte
    puntos_de_corte = []
    for i in range(num_val - 1, len(x_sorted), num_val):
        if len(puntos_de_corte) == num_bins - 1:
            break
        else:
            puntos_de_corte.append(x_sorted[i])

    # Asignar categorías
    for i in range(len(x)):
        if x[i] <= min(puntos_de_corte):
            vector_categ[i] = f"I(-infty, {min(puntos_de_corte):.2f}]"
        elif x[i] > max(puntos_de_corte):
            vector_categ[i] = f"I[{max(puntos_de_corte):.2f}, +infty)"
        else:
            # Ajustar el bucle para incluir el punto de corte
            for j in range(1, len(puntos_de_corte)):
                if x[i] <= puntos_de_corte[j]:
                    vector_categ[i] = f"I({puntos_de_corte[j - 1]:.2f}, {puntos_de_corte[j]:.2f}]"
                    break

    # Convertir a factor/categoría
    vector_categ = pd.Categorical(vector_categ)
    return vector_categ


def discretize_ef_df(df, num_bins):
    """
    Funcion de discretizacion Equal Frecuency para cada columna de un data frame

     Parameters
     ----------
     df (pd.DataFrame): data frame de clase numérico
     num_bins (int): el valor del número de intervalos que queremos

     Returns
     -------
     df_dis (pd.DataFrame): data frame de tipo categórico con los valores discretizados
     
     Example:
        # Crear el DataFrame
        data = pd.DataFrame({
            'Variable1': [2.5, 3.6, 5.1, 8.4, 9.7, 2.3, 7.5, 6.2],
            'Variable2': [15.2, 18.4, 20.1, 14.5, 12.3, 17.6, 21.8, 19.2]
        })

        # Llamar a la función para discretizar el DataFrame
        result_df = discretize_ef_df(data, num_bins=3)

        # Mostrar el resultado
        print(result_df) 

    """
    # Aplicar la función discretize_ef a cada columna del DataFrame
    df_dis = df.apply(lambda col: discretize_ef(col, num_bins))
    return df_dis


def discretize(x, cut_points):
    """
    Funcion de discretizacion dado los puntos de corte

    Parameters
    ----------
    x (array, float): un vector de tipo numérico a discretizar 
    cut_points (list): una lista con valores de los puntos de cortes que se deben utilizar para la discretización

    Returns
    -------
    vector_categ (array, pd.Categorical): un vector categórico  con los calores discretizados
    
    Example:
        # Datos de entrada
        a = np.array([100, 50, 60, 10, 80, 20])
        puntoscorte = [32.5, 55.0, 77.5]  # Lista de puntos de corte

        # Llamar a la función discretize
        result = discretize(a, puntoscorte)

        # Mostrar el resultado
        print(result)
    """
    # Convertir los puntos de corte a un array numérico
    puntos_de_corte = np.array(cut_points, dtype=float)

    # Inicializar el vector categórico
    vector_categ = [''] * len(x)

    # Asignar categorías
    for i in range(len(x)):
        if x[i] <= min(puntos_de_corte):
            vector_categ[i] = f"I(-infty, {min(puntos_de_corte):.2f}]"
        elif x[i] >= max(puntos_de_corte):
            vector_categ[i] = f"I[{max(puntos_de_corte):.2f}, +infty)"
        else:
            # Ajustar el bucle para incluir el punto de corte
            for j in range(1, len(puntos_de_corte)):
                if x[i] <= puntos_de_corte[j]:
                    vector_categ[i] = f"I({puntos_de_corte[j - 1]:.2f}, {puntos_de_corte[j]:.2f}]"
                    break

    # Convertir a tipo categórico de pandas
    vector_categ = pd.Categorical(vector_categ)
    return vector_categ

