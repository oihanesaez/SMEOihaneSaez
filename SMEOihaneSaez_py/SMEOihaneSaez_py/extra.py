import numpy as np

def remove_duplicates(data):
    """
    Encontrar y eliminar filas duplicadas de un DataFrame.

    Parameters:
    data (pd.DataFrame): El DataFrame del cual se eliminarán los duplicados.

    Returns:
    pd.DataFrame: Un nuevo DataFrame sin filas duplicadas.
    
    Example:
        data_with_duplicates = pd.DataFrame({
            'ID': [1, 2, 2, 3, 4, 4, 5],
            'Value': ['A', 'B', 'B', 'C', 'D', 'D', 'E']
        })

        # Llamar a la función remove_duplicates
        data_sin_duplicados = remove_duplicates(data_with_duplicates)

        # Mostrar el DataFrame resultante
        print(data_sin_duplicados)
    """
    # Filtrar el DataFrame para conservar solo filas únicas
    data_unique = data.drop_duplicates()

    # Devolver el DataFrame sin duplicados
    return data_unique



def split_data(data, train_ratio=0.7, seed=None):
    """
    Divide un DataFrame en conjuntos de entrenamiento y prueba.

    Parameters:
    data (pd.DataFrame): El DataFrame a dividir.
    train_ratio (float): Proporción del conjunto de entrenamiento (valor entre 0 y 1).
    seed (int, optional): Semilla para la aleatorización, para reproducibilidad.

    Returns:
    dict: Un diccionario con los conjuntos 'train' y 'test'.
    
    Example:
        data = pd.DataFrame({
            'ID': range(1, 11),
            'Value': [5, 3, 8, 2, 6, 9, 1, 7, 4, 10]
        })

        # Llamar a la función para dividir los datos
        result = split_data(data, train_ratio=0.7, seed=123)

        # Mostrar los resultados
        print("Conjunto de Entrenamiento:")
        print(result['train'])
        print("\nConjunto de Prueba:")
        print(result['test'])
    """
    # Si se especifica, establecer la semilla para reproducibilidad
    if seed is not None:
        np.random.seed(seed)

    # Número total de observaciones
    n = len(data)

    # Determinar el número de muestras para el conjunto de entrenamiento
    train_size = int(np.floor(train_ratio * n))

    # Obtener índices aleatorios para el conjunto de entrenamiento
    train_indices = np.random.choice(range(n), size=train_size, replace=False)

    # Dividir el DataFrame
    train_data = data.iloc[train_indices]
    test_data = data.drop(train_indices)

    # Devolver los conjuntos como un diccionario
    return {'train': train_data, 'test': test_data}

