import numpy as np
import pandas as pd

def AUC(df):
    """Calcula el área bajo la curva (AUC) para un DataFrame con una columna numérica y una columna binaria.
    
    Args:
        df (pd.DataFrame): DataFrame con la primera columna numérica y la segunda columna binaria.

    Returns:
        dict: Un diccionario con la matriz de resultados (FPR, TPR) y el área (AUC).
        
    Example:
        # Generar datos aleatorios
        np.random.seed(123)  # Establecer la semilla para reproducibilidad
        n = 100  # Número de observaciones
        variable1 = np.random.normal(size=n)  # 100 números aleatorios de una normal
        variable_binaria = np.random.choice([0, 1], size=n)  # 100 valores binarios aleatorios
        variable_binaria = pd.Categorical(variable_binaria)  # Convertir a tipo categórico

        # Crear un DataFrame
        data_frame = pd.DataFrame({
            'Variable1': variable1,
            'VariableBinaria': variable_binaria
        })

        # Calcular el AUC
        resultado = AUC(data_frame)

        # Mostrar el resultado
        print("Matriz de resultados (FPR, TPR):")
        print(resultado['result'])
        print("Área bajo la curva (AUC):", resultado['area'])

    """
    # Identificar la columna binaria (la que tiene dos niveles)
    bin_col_index = next((i for i in range(df.shape[1]) if df.iloc[:, i].dtype.name == 'category' and len(df.iloc[:, i].cat.categories) == 2), None)

    # Si no hay una columna binaria, retornar un error
    if bin_col_index is None:
        raise ValueError("No hay una columna binaria en el DataFrame.")

    # Ordenar el DataFrame de menor a mayor valor de la primera columna
    df_orden = df.sort_values(by=df.columns[0])

    # Convertir la columna binaria a valores lógicos
    clase_col = df_orden.iloc[:, bin_col_index]
    clase_logica = (clase_col == clase_col.cat.categories[1]).astype(bool)  # Asignar TRUE y FALSE

    # Crear un vector de valores de corte (usamos todos los valores únicos de la primera columna)
    val_corte = df_orden.iloc[:, 0].unique()

    # Inicializar matriz para guardar los pares de (FPR, TPR)
    result = np.zeros((len(val_corte), 2))

    for i, corte in enumerate(val_corte):
        # Conseguir un vector lógico comparando cada valor de la columna con el valor de corte
        v_logico = df_orden.iloc[:, 0] > corte

        # Calcular True Positives (TP)
        TP = np.sum(v_logico & clase_logica)

        # Calcular True Negatives (TN)
        TN = np.sum(~v_logico & ~clase_logica)

        # Calcular False Negatives (FN)
        FN = np.sum(~v_logico & clase_logica)

        # Calcular False Positives (FP)
        FP = np.sum(v_logico & ~clase_logica)

        # Calcular True Positive Rate (TPR)
        TPR = TP / (TP + FN) if (TP + FN) > 0 else 0

        # Calcular False Positive Rate (FPR)
        FPR = FP / (FP + TN) if (FP + TN) > 0 else 0

        # Guardar el TPR y FPR en la matriz
        result[i] = [FPR, TPR]

    # Añadir (0, 0) y (1, 1) si no están presentes
    if not any((result[:, 0] == 0) & (result[:, 1] == 0)):
        result = np.vstack(([0, 0], result))
    
    if not any((result[:, 0] == 1) & (result[:, 1] == 1)):
        result = np.vstack((result, [1, 1]))

    # Ordenar los resultados según el valor de la primera columna de menor a mayor
    result = result[np.argsort(result[:, 0])]

    # Calcular el AUC utilizando la regla del trapecio
    area = 0
    for k in range(1, result.shape[0]):
        # Calcular la anchura del intervalo
        h = abs(result[k-1, 0] - result[k, 0])
        # Aplicar la regla del trapecio
        area += 0.5 * (result[k-1, 1] + result[k, 1]) * h

    return {'result': result, 'area': area}



