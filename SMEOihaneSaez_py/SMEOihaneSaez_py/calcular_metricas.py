import numpy as np
import pandas as pd
from fun_analisis import var_vector, cor_fun
from AUC import AUC
from entropia import entropy, info_mutua

#CORREGIR
def calcular_metricas(df):
    """
    Calcula varianza, AUC y entropía para cada columna de un DataFrame.
    
    Args:
        df (pd.DataFrame): data frame de datos con columnas numéricas, binarias y/o categóricas.
        
    Returns:
        pd.DataFrame: la varianza y AUC en caso de variable numérica y entropía en caso de variable categórica.
    
    Example:
        data = pd.DataFrame({
            'Score': [0.1, 0.4, 0.35, 0.6, 0.3, 0.75, 0.55, 0.8],
            'Outcome': ["Negative", "Negative", "Negative", "Positive", 
                                       "Negative", "Positive", "Positive", "Positive"]
        })

        #Convertir en tipo categórico
        data['Outcome']=data['Outcome'].astype('category')

        # Llamar a la función calcular_metricas
        resultados = calcular_metricas(data)
        print(resultados)
    
    """
    # Inicializar listas para resultados
    varianzas = [np.nan] * df.shape[1]
    auc_values = [np.nan] * df.shape[1]
    entropias = [np.nan] * df.shape[1]

    # Iterar sobre cada columna del DataFrame
    for i in range(df.shape[1]):
        col = df.iloc[:, i]

        # Calcular varianza si la columna es numérica
        if pd.api.types.is_numeric_dtype(col):
            varianzas[i] = var_vector(col)

            # Calcular AUC si hay una columna binaria
            bin_col_index = next((j for j in range(df.shape[1]) if df.iloc[:, j].nunique() == 2), None)
            if bin_col_index is not None:
                bin_col = df.iloc[:, bin_col_index]
                df_auc = pd.DataFrame({'num_col': col, 'bin_col': bin_col})
                auc_values[i] = AUC(df_auc)['area']

        # Calcular entropía si la columna es categórica
        if isinstance(col.dtype, pd.CategoricalDtype):
            entropias[i] = entropy(col)

    # Crear el DataFrame de resultados
    resultados = pd.DataFrame({
        'Variable': df.columns,
        'Varianza': varianzas,
        'AUC': auc_values,
        'Entropía': entropias
    })

    return resultados


def cor_infomutua(df):
    """
    Calcula la correlación y la información mutua entre las columnas de un DataFrame.
    
    Args:
        df (pd.DataFrame): data frame de datos con columnas numéricas y/o categóricas
        
    Returns:
        matriz numérica: la correlación o información mutua entre variables
        
    Example:
        # Semilla para reproducibilidad
        np.random.seed(123)

        # Crear el DataFrame con variables numéricas y categóricas como strings
        dataset = pd.DataFrame({
            'var_num1': np.random.normal(size=100),                       # Variable numérica
            'var_num2': np.random.normal(loc=5, size=100),               # Otra variable numérica
            'var_cat1': np.random.choice(['A', 'B', 'C'], size=100),     # Variable categórica con 3 categorías (como strings)
            'var_cat2': np.random.choice(['D', 'E', 'F', 'G'], size=100) # Variable categórica con 4 categorías (como strings)
        })

        #Convertir columnas en tipo categórico
        dataset['var_cat1']=dataset['var_cat1'].astype('category')
        dataset['var_cat2']=dataset['var_cat2'].astype('category')

        # Calcular correlaciones e información mutua
        resultados = cor_infomutua(dataset)
        print("Matriz de Correlación e Información Mutua:")
        print(resultados)    
    """
    
    # Identificar el número de columnas y el tipo de cada columna
    n_col = df.shape[1]
    tipos = df.dtypes

    # Matriz de resultados
    result_matrix = np.empty((n_col, n_col), dtype=object)  # Usar tipo object para permitir NA
    result_matrix[:] = np.nan  # Inicializar con NaN
    

    # Calcular correlaciones e información mutua entre cada par de columnas
    for i in range(n_col):
        for j in range(n_col):
            # Si ambas variables son numéricas, calcula correlación
            if pd.api.types.is_numeric_dtype(df.iloc[:, i]) and pd.api.types.is_numeric_dtype(df.iloc[:, j]):
                matriz_dat = df.iloc[:, [i, j]]  # Matriz de datos para este par
                result_matrix[i,j] = cor_fun(matriz_dat)[0,1]

            # Si ambas variables son categóricas (tipo string), calcula información mutua
            elif isinstance(df.iloc[:, i].dtype, pd.CategoricalDtype) and isinstance(df.iloc[:, j].dtype, pd.CategoricalDtype):
                matriz_categ = df.iloc[:, [i, j]]
                result_matrix[i, j] = info_mutua(matriz_categ)[0,1]  # Información mutua
            else:  # Ignorar cuando una columna es categórica y la otra numérica
                result_matrix[i, j] = np.nan

    return result_matrix


