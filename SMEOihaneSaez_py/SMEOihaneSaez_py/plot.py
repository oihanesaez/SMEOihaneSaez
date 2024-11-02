import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import AUC
from AUC import AUC
from entropia import norm_entropy_df, info_mutua
from fun_analisis import cor_fun

def plot_AUC(df):
    """
    Graficar la curva ROC utilizando la función AUC definida en el módulo AUC.
    
    Args:
        df: DataFrame que debe contener una columna numérica y una columna binaria.
        
    Returns: 
        un gráfico de la curva ROC
        
    Example:
        # Establecer semilla para reproducibilidad
        np.random.seed(123)

        # Crear un DataFrame con una variable numérica y una variable binaria
        data_frame = pd.DataFrame({
            'Variable1': np.random.normal(size=100),  # 100 números aleatorios
            'VariableBinaria': pd.Categorical(np.random.choice([0, 1], size=100))  # Variable binaria
        })

        # Graficar la curva ROC
        plot_AUC(data_frame)
    """
    # Asegúrate de que la columna binaria sea un factor
    if df[df.columns[1]].dtype.name != 'category':
        df[df.columns[1]] = df[df.columns[1]].astype('category')

    # Obtener los resultados del AUC
    data_AUC = AUC(df)
    
    # Graficar la curva ROC
    plt.figure(figsize=(8, 6))
    plt.plot(data_AUC['result'][:, 0], data_AUC['result'][:, 1], color='blue', label='Curva ROC')
    plt.plot([0, 1], [0, 1], color='red', linestyle='--', label='Probabilidad al azar (50%)')
    plt.xlim([0.0, 1.0])
    plt.ylim([0.0, 1.0])
    plt.xlabel('Tasa de Falsos Positivos (FPR)')
    plt.ylabel('Tasa de Verdaderos Positivos (TPR)')
    plt.title('La curva ROC')
    plt.legend(loc='lower right')
    plt.grid()
    plt.show()


def plot_entropy(df):
    """
    Genera un diagrama de barras para la entropía normalizada de cada variable en un DataFrame de categorías.
    
    Args: 
        df (pd.DataFrame): un data frame de tipo categórico
        
    Returns:
        Un gráfico de barras para la entropía normalizada.
        
    Example:
        data = pd.DataFrame({
            'Columna_A': ["Rojo", "Rojo", "Azul", "Verde", "Rojo", "Azul", "Verde", "Verde"],
            'Columna_B': ["Círculo", "Cuadrado", "Círculo", "Cuadrado", "Cuadrado", "Círculo", "Triángulo", "Círculo"],
            'Columna_C': ["Bajo", "Alto", "Medio", "Bajo", "Medio", "Bajo", "Alto", "Medio"]
        })

        # Asegurarse de que las columnas son de tipo 'category'
        for col in data.columns:
            data[col] = data[col].astype('category')

        # Llamar a la función plot_entropy para graficar la entropía normalizada de cada columna
        plot_entropy(data)
    """
    
    # Calcula la entropía normalizada de cada columna en el DataFrame
    H_norm = norm_entropy_df(df)  # Asegúrate de tener una función `norm_entropy_df` que calcule la entropía normalizada
    
    # Número de variables
    n = len(H_norm)
    names_bar = range(1, n + 1)  # Genera etiquetas para cada variable
    
    # Crear el diagrama de barras
    plt.figure(figsize=(10, 6))
    plt.bar(names_bar, H_norm, color="orange")
    plt.xlabel("Número de variables")
    plt.ylabel("Entropía normalizada")
    plt.title("Diagrama de barras para la entropía normalizada de cada variable")
    plt.xticks(names_bar)  # Coloca los números de variable en el eje x
    plt.show()


def plot_cor(x):
    """
    calcula la matriz de correlaciones para luego poder hacer un mapa de calor
    
    Args:
        x (pd.DataFrame): un data frame de datos numéricos
        
    Returns:
        Un mapa de calor para la matriz de correlaciones
        
    Example:
        np.random.seed(123)

        # Crear un DataFrame de ejemplo con 100 filas y 4 variables aleatorias
        datos_ejemplo = pd.DataFrame({
            'Var1': np.random.normal(size=100),
            'Var2': np.random.normal(size=100),
            'Var3': np.random.normal(size=100),
            'Var4': np.random.normal(size=100)
        })

        plot_cor(datos_ejemplo)
    """
    # Calcular la matriz de correlaciones
    correlaciones = cor_fun(x)
    
    # Crear el mapa de calor
    plt.figure(figsize=(10, 8))
    plt.imshow(correlaciones, cmap='coolwarm', interpolation='nearest')
    plt.colorbar()  # Muestra la barra de color al lado
    
    # Configurar etiquetas usando los nombres del DataFrame original
    plt.xticks(ticks=np.arange(len(x.columns)), labels=x.columns, rotation=45)
    plt.yticks(ticks=np.arange(len(x.columns)), labels=x.columns)

    # Añadir título
    plt.title("Mapa de calor para la matriz de correlaciones")
    
    # Mostrar el gráfico
    plt.tight_layout()  # Ajustar el layout para que no se superpongan las etiquetas
    plt.show()
    


def plot_infomutua(x):
    """
    Funcion para visualizar la informacion mutua de un data set
    
    Args:
        x (pd.DataFrame): un data frame de datos categóricos
        
    Returns:
        Un mapa de calor para la matriz de informacion mutua
        
    Example:
        data = pd.DataFrame({
            'Columna_A': pd.Categorical(["Rojo", "Rojo", "Azul", "Verde", "Rojo", "Azul", "Verde", "Verde"]),
            'Columna_B': pd.Categorical(["Alto", "Medio", "Medio", "Bajo", "Alto", "Bajo", "Alto", "Medio"]),
            'Columna_C': pd.Categorical(["Bajo", "Alto", "Medio", "Bajo", "Medio", "Bajo", "Alto", "Medio"])
        })

        # Llamar a la función para graficar
        plot_infomutua(data)

    """
    # Calcular la matriz de información mutua
    matriz_im = info_mutua(x)
    
    # Crear el mapa de calor
    plt.figure(figsize=(10, 8))
    plt.imshow(matriz_im, cmap='coolwarm', interpolation='nearest')
    
    # Mostrar la barra de color al lado
    plt.colorbar()
    
    # Obtener los nombres de las columnas
    nombres_columnas = x.columns
    
    # Configurar etiquetas usando los nombres del DataFrame original
    plt.xticks(ticks=np.arange(len(nombres_columnas)), labels=nombres_columnas, rotation=45)
    plt.yticks(ticks=np.arange(len(nombres_columnas)), labels=nombres_columnas)
    
    # Añadir título
    plt.title("Mapa de calor para la matriz de información mutua")
    
    # Mostrar el gráfico
    plt.tight_layout()  # Ajustar el layout para que no se superpongan las etiquetas
    plt.show()


