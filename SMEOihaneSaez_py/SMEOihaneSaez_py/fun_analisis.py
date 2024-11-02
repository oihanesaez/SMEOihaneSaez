import numpy as np
import pandas as pd

def estandar(v):
    """
    Funcion de estandarizacion de un vector

    Parameters
    ----------
    v (array, float): un vector numérico a estandarizar

    Raises
    ------
    ValueError
        La función no se ejecuta si el vector no es numérico

    Returns
    -------
    v_est (array, float): un vector numérico con los valores estandarizados.
    
    Example:
       vector = np.array([10, 20, 30, 40, 50])
       resultado = estandar(vector)

       # Mostrar el resultado
       print(resultado) 

    """
    # Verificar si el vector es numérico
    if not np.issubdtype(v.dtype, np.number):
        raise ValueError("Error: El vector no es numérico.")

    # Calcular la media y la desviación estándar
    v_est = (v - np.mean(v)) / np.std(v)
    return v_est


def estandar_df(df):
    """
    Funcion de estandarizacion de las columnas de un data frame

    Parameters
    ----------
    df (pd.DataFrame): data frame de tipo numérico a estandarizar

    Returns
    -------
    df_estandar (pd.DataFrame): un data frame con los valores de cada columna estandarizados
    
    Example:
        data = {
            'Variable1': [10, 20, 30, 40, 50],
            'Variable2': [5, 15, 25, 35, 45],
            'Variable3': [2, 4, 6, 8, 10]
        }

        df = pd.DataFrame(data)

        # Llamar a la función estandar_df
        resultado_df = estandar_df(df)

        # Mostrar el resultado
        print(resultado_df)

    """
    # Aplicar la función estandar a cada columna del DataFrame
    df_estandar = df.apply(estandar, axis=0)
    return df_estandar

def norm(v):
    """
    Funcion de normalización de un vector

    Parameters
    ----------
    v (array, float): un vector numérico a normalizar

    Raises
    ------
    ValueError
        La función no se ejecuta si el vector no es numérico

    Returns
    -------
    v_norm (array, float): un vector numérico con los valores normalizados.
    
    Example:
        vector = np.array([10, 20, 30, 40, 50])
        resultado = norm(vector)

        # Mostrar el resultado
        print(resultado)
    """
    # Verificar si el vector es numérico
    if not np.issubdtype(v.dtype, np.number):
        raise ValueError("Error: El vector no es numérico.")

    # Normalizar el vector utilizando el escalado mínimo-máximo
    v_norm = (v - np.min(v)) / (np.max(v) - np.min(v))
    return v_norm


def norm_df(df):
    """
    Funcion de normalizacion de las columnas de un data frame

    Parameters
    ----------
    df (pd.DataFrame): un data frame numérico a normalizar

    Returns
    -------
    df_norm (pd.DataFrame): un data frame numérico con los valores de cada columna normalizadas.
    
    Example:
        data = {
            'columna1': [10, 20, 30, 40, 50],
            'columna2': [5, 15, 25, 35, 45],
            'columna3': [2, 4, 6, 8, 10]
        }

        df = pd.DataFrame(data)

        # Llamar a la función norm_df
        resultado_df = norm_df(df)

        # Mostrar el resultado
        print(resultado_df)

    """
    # Aplicar la función norm a cada columna del DataFrame
    df_norm = df.apply(norm, axis=0)
    return df_norm



def var_vector(x):
    """
    Funcion del cálculo de la varianza de un vector

    Parameters
    ----------
    x (array, float): un vector numérico al que calcular la varianza

    Raises
    ------
    ValueError
        La función no se ejecuta si el vector no es numérico

    Returns
    -------
    varianza (float): el valor de la varianza
    
    Example:
        vector = np.array([10, 20, 30, 40, 50])
        resultado = var_vector(vector)

        # Mostrar el resultado
        print(resultado)

    """
    # Verificar si el vector es numérico
    if not np.issubdtype(x.dtype, np.number):
        raise ValueError("Error: El vector no es numérico.")

    # Calcular la media
    media = np.mean(x)

    # Restar la media de cada elemento
    ken_media = x - media

    # Calcular la varianza
    varianza = np.sum(ken_media**2) / (len(x) - 1)

    return varianza


def var(x):
    """
    Funcion del cálculo de la varianza de las columnas de un data frame

    Parameters
    ----------
    x (pd.DataFrame): un data frame de tipo numérico

    Raises
    ------
    ValueError
        La función no se ejecuta si el data frame no es numérico

    Returns
    -------
    var (array, float): un vector numérico con las varianzas calculadas de cada columna del df
    
    Example:
        data = {
            'columna1': [10, 20, 30, 40, 50],
            'columna2': [5, 15, 25, 35, 45],
            'columna3': [2, 4, 6, 8, 10]
        }

        df = pd.DataFrame(data)

        # Llamar a la función norm_df
        resultado_df = var(df)

        # Mostrar el resultado
        print(resultado_df)

    """
    # Verificar si todas las columnas son numéricas
    if not np.all([np.issubdtype(dtype, np.number) for dtype in x.dtypes]):
        raise ValueError("Error: La matriz no es numérica.")

    nrow = x.shape[0]  # Número de filas
    ncol = x.shape[1]  # Número de columnas

    # Calcular la media de cada columna
    mean_col = x.mean(axis=0)  # Vector de medias de cada columna

    # Restar la media de cada columna a cada elemento de esa columna
    ken_media = x - mean_col  # Aplicar la resta de la media

    # Calcular la varianza
    var = (ken_media**2).sum(axis=0) / (nrow - 1)

    return var


def cov_fun(x):
    """
    Esta funcion calcula la covarianza de las columnas de un data frame

    Parameters
    ----------
    x (pd.DataFrame): un data frame de valores numéricos

    Returns
    -------
    matriz_covarianzas (matrix): una matriz numérica (la matriz de varianzas-covarianzas)
    
    Example:
        data = {
            'columna1': [10, 20, 30, 40, 50],
            'columna2': [5, 15, 25, 35, 45],
            'columna3': [2, 4, 6, 8, 10]
        }

        df = pd.DataFrame(data)

        # Calcular la matriz de covarianzas
        matriz_cov = cov_fun(df)

        print("Matriz de varianza-covarianza:")
        print(matriz_cov)

    """
    n_col = x.shape[1]  # Número de columnas
    n_row = x.shape[0]  # Número de filas

    # Calcular la media de cada columna (vector de medias de cada columna)
    mean_col = x.mean(axis=0)

    # Restar la media de cada columna a cada elemento de esa columna
    ken_media = x - mean_col  # Aplicar la resta de la media

    # Inicializar la matriz de covarianzas
    matriz_covarianzas = np.zeros((n_col, n_col))

    for i in range(n_col):
        for j in range(n_col):
            matriz_covarianzas[i, j] = np.sum(ken_media.iloc[:, i] * ken_media.iloc[:, j]) / (n_row - 1)  # Calcular la covarianza

    return matriz_covarianzas


def cor_fun(matriz_dat):
    """
    Esta funcion calcula la correlación de las columnas de un data frame

    Parameters
    ----------
    matriz_dat (pd.DataFrame): un data frame de valores numéricos

    Returns
    -------
    matriz_correlaciones (matrix): una matriz numérica con los valores de las correlaciones calculadas.
    
    Example:
        data = {
            'var1': [10, 20, 30, 40, 50],
            'var2': [5, 15, 24, 35, 45],
            'var3': [2, 4, 6, 8, 9]
        }

        df = pd.DataFrame(data)

        # Calcular la matriz de correlaciones
        matriz_cor = cor_fun(df)

        print("Matriz de correlaciones:")
        print(matriz_cor)

    """
    n = matriz_dat.shape[1]  # Número de variables
    matriz_cov = cov_fun(matriz_dat)  # Calcular la matriz de covarianzas
    matriz_correlaciones = np.zeros((n, n))  # Inicializar matriz de correlaciones

    for i in range(n):
        for j in range(n):
            # Extraer la covarianza y las varianzas
            cov_xy = matriz_cov[i, j]  # Cov(X, Y)
            var_x = matriz_cov[i, i]    # Var(X)
            var_y = matriz_cov[j, j]    # Var(Y)

            # Calcular la correlación
            matriz_correlaciones[i, j] = cov_xy / np.sqrt(var_x * var_y)

    return matriz_correlaciones


