import numpy as np
import pandas as pd

def entropy(x):
    """
    Funcion para calcular la entropia

    Parameters:
        x (pd.Categorical/pd.Series): es un vector (array) de tipo categórico

    Returns:
        H (float): el valor de la entropía del vector
    
    Example:
        # Crear el factor en Python
        data_factor = pd.Series(["A", "B", "A", "C", "B", "A", "D", "D", "C", "A"]).astype('category')

        # Calcular la entropía
        H = entropy(data_factor)
        print("Entropía:", H)

    """
    # Asegurarse de que x es un objeto categórico usando la nueva sintaxis recomendada
    if not isinstance(x.dtype, pd.CategoricalDtype):
        x = pd.Series(x).astype('category')  # Convertir a tipo categórico si no lo es

    # Calcular la frecuencia de cada nivel y dividir por la longitud total para obtener probabilidades
    p = x.value_counts(normalize=True).values  # `normalize=True` da las probabilidades directamente

    # Calcular la entropía usando log2, manejando probabilidad cero con un pequeño valor (1e-10)
    H = -np.sum(p * np.log2(p + 1e-10))

    return H


def entropy_df(df):
    """
    Funcion para calcular la entropia de cada columna de un data frame

    Parameters
    ----------
    df (pd.DataFrame): un data frame de datos de tipo categórico

    Returns
    -------
    v_entropy (float array): es un vector numérico con el valor de la entropía de cada columna
    
    Example:
        # Crear el DataFrame
        data_frame = pd.DataFrame({
            'Color': ['Rojo', 'Verde', 'Azul', 'Rojo', 'Verde', 'Verde'],
            'Forma': ['Círculo', 'Cuadrado', 'Círculo', 'Cuadrado', 'Cuadrado', 'Círculo'],
            'Tamaño': ['Grande', 'Pequeño', 'Pequeño', 'Grande', 'Grande', 'Pequeño']
        })

        # Convertir columnas a tipo categórico
        data_frame['Color'] = data_frame['Color'].astype('category')
        data_frame['Forma'] = data_frame['Forma'].astype('category')
        data_frame['Tamaño'] = data_frame['Tamaño'].astype('category')

        # Calcular la entropía para cada columna
        entropy_values = entropy_df(data_frame)
        print("Entropía por columna:\n", entropy_values)

    """
    # Aplicar la función de entropía a cada columna del DataFrame
    v_entropy = df.apply(entropy)
    return v_entropy


def norm_entropy(x):
    """
    Funcion para calcular la entropia normalizada

    Parameters
    ----------
    x (pd.Categorical array): es un vector de tipo categórico

    Returns
    -------
    H_norm (float): el valor de la entropía normalizada
    
    Example:
       data_factor = pd.Series(['A', 'B', 'A', 'C', 'B', 'A', 'D', 'D', 'C', 'A']).astype('category')
       normalized_entropy = norm_entropy(data_factor)
       print("Entropía normalizada:", normalized_entropy) 

    """
    # Calcular la entropía
    H = entropy(x)
    n = len(x)  # Longitud del vector
    H_norm = H / np.log2(n)  # Normalizar la entropía
    return H_norm


def norm_entropy_df(df):
    """
    Funcion para calcular la entropia normalizada de cada columna de un data frame

    Parameters
    ----------
    df (pd.DataFrame) : un data frame de tipo categórico

    Returns
    -------
    df_entropy_norm (float array): es un vector numérico con los valores de la entropía normalizada de cada columna del df
    
    Example:
        data_frame = pd.DataFrame({
            'Color': ['Rojo', 'Verde', 'Azul', 'Rojo', 'Verde', 'Verde'],
            'Forma': ['Círculo', 'Cuadrado', 'Círculo', 'Cuadrado', 'Cuadrado', 'Círculo'],
            'Tamaño': ['Grande', 'Pequeño', 'Pequeño', 'Grande', 'Grande', 'Pequeño']
        })

        # Convertir columnas a tipo categórico
        data_frame['Color'] = data_frame['Color'].astype('category')
        data_frame['Forma'] = data_frame['Forma'].astype('category')
        data_frame['Tamaño'] = data_frame['Tamaño'].astype('category')

        # Calcular la entropía normalizada para cada columna
        normalized_entropies = norm_entropy_df(data_frame)
        print("Entropía normalizada por columna:", normalized_entropies)

    """
    # Aplicar la función norm_entropy a cada columna del DataFrame y devolver como un vector numérico
    df_entropy_norm = df.apply(norm_entropy)  
    return df_entropy_norm


def filtro_entropia(x, H_umbral):
    """
    Funcion que partiendo de un dataset obtiene uno nuevo donde todas las variables cumplen un requisito
    

    Parameters
    ----------
    x (pd.DataFrame): es un data frame de tipo categórico
    H_umbral (float): es el valor umbral de la entropía que tienen que cumplir las variables del df

    Returns
    -------
    x_filtrado (pd.DataFrame): un nuevo data frame sólamente con las variables que son mayores que la entropía umbral
    
    Example:
        data_frame = pd.DataFrame({
            'Color': pd.Categorical(["Rojo", "Verde", "Azul", "Rojo", "Verde", "Verde"]),
            'Forma': pd.Categorical(["Círculo", "Cuadrado", "Círculo", "Cuadrado", "Cuadrado", "Círculo"]),
            'Tamaño': pd.Categorical(["Grande", "Pequeño", "Pequeño", "Grande", "Grande", "Pequeño"])
        })

        # Aplicar filtro de entropía
        H_umbral = 1  # Umbral de entropía
        data_frame_filtrado = filtro_entropia(data_frame, H_umbral)

        # Mostrar el DataFrame filtrado
        print("DataFrame filtrado:\n", data_frame_filtrado)

    """
    # Calcular las entropías de cada columna
    entropy_x = entropy_df(x)  # vector de entropías para cada columna
    true_cols = entropy_x > H_umbral  # vector lógico: True si es mayor que el umbral

    # Filtrar el DataFrame según el umbral
    x_filtrado = x.loc[:, true_cols]  # Usar .loc para seleccionar columnas

    return x_filtrado


def info_mutua(df):
    """
    Funcion para el cálculo de la información mutua de las columnas de un data frame

    Parameters
    ----------
    df (pd.DataFrame): data frame de datos categóricos

    Returns
    -------
    matriz_infomutua (matrix): una matriz numérica con los valores de la información mutua
    
    Example:
        data = pd.DataFrame({
            'Columna_A': pd.Categorical(["Rojo", "Rojo", "Azul", "Verde", "Rojo", "Azul", "Verde", "Verde"]),
            'Columna_B': pd.Categorical(["Alto", "Medio", "Medio", "Bajo", "Alto", "Bajo", "Alto", "Medio"]),
            'Columna_C': pd.Categorical(["Bajo", "Alto", "Medio", "Bajo", "Medio", "Bajo", "Alto", "Medio"])
        })

        # Calcular la información mutua
        matriz_info_mutua = info_mutua(data)

        # Mostrar la matriz de información mutua
        print("Matriz de Información Mutua:\n", matriz_info_mutua)

    """
    n_col = df.shape[1]  # Número de variables
    n_row = df.shape[0]  # Número de filas
    matriz_infomutua = np.zeros((n_col, n_col))  # Inicializamos la matriz de resultados

    # Calculamos la entropía de cada variable
    H_variable = entropy_df(df)  # Vector de entropías de cada columna

    for i in range(n_col):
        categ1 = df.iloc[:, i].cat.categories  # Categorías en la columna i
        for j in range(n_col):
            categ2 = df.iloc[:, j].cat.categories  # Categorías en la columna j

            # Inicializar la matriz de probabilidades conjuntas
            p_x_y = np.zeros((len(categ1), len(categ2)))

            # Contar las ocurrencias de cada combinación de categoría
            for m in range(n_row):
                cat1_index = np.where(categ1 == df.iloc[m, i])[0][0]  # Índice de la categoría en la columna i
                cat2_index = np.where(categ2 == df.iloc[m, j])[0][0]  # Índice de la categoría en la columna j

                # Asegurarse de que ambos índices sean válidos
                if cat1_index >= 0 and cat2_index >= 0:  # Verificación básica de índice
                    p_x_y[cat1_index, cat2_index] += 1  # Contar ocurrencias

            # Calcular las probabilidades conjuntas
            p_x_y /= n_row

            # Calcular H(X, Y) usando probabilidad conjunta
            H_XY = -np.sum(p_x_y * np.log(p_x_y + 1e-10))  # Sumar una pequeña constante para evitar log(0)

            # Calcular I(X; Y) = H(X) + H(Y) - H(X, Y)
            matriz_infomutua[i, j] = H_variable[i] + H_variable[j] - H_XY

    return matriz_infomutua

