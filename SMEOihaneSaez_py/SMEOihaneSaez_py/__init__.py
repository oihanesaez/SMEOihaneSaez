"""
SMEOihaneSaez_py

Este paquete contiene funciones básicas para el analisis y gestión de datos

"""

from AUC import AUC
from discretizacion import discretize_ew, discretize_ef, discretize_ew_df, discretize_ef_df, discretize_ef
from extra import remove_duplicates, split_data
from plot import plot_AUC, plot_entropy, plot_cor, plot_infomutua
from calcular_metricas import calcular_metricas, cor_infomutua
from entropia import entropy, entropy_df, norm_entropy, norm_entropy_df, filtro_entropia, info_mutua
from fun_analisis import estandar, estandar_df, norm, norm_df, var_vector, var, cov_fun, cor_fun