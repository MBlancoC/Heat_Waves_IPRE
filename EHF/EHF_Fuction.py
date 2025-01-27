import pandas as pd
import numpy as np

def calculate_ehf(data, tmean_col, t95, t30_window=30):
    """
    Calcula el Excess Heat Factor (EHF) basado en las temperaturas promedio diarias.

    Parámetros:
    - data: DataFrame con una columna de temperaturas promedio diarias.
    - tmean_col: Nombre de la columna con la temperatura promedio diaria.
    - t95: Valor del percentil 95 de la temperatura promedio diaria.
    - t30_window: Ventana para calcular la temperatura promedio de aclimatación (por defecto 30 días).

    Retorna:
    - DataFrame con columnas de EHIsig, EHIaccl y EHF.
    """
    # Asegurar que los datos estén ordenados por fecha
    data = data.sort_index()

    # Calcular T3D (promedio de 3 días consecutivos)
    data['T3D'] = data[tmean_col].rolling(window=3).mean()

    # Calcular EHIsig
    data['EHIsig'] = data['T3D'] - t95

    # Calcular T30D (promedio de 30 días previos)
    data['T30D'] = data[tmean_col].rolling(window=t30_window).mean()

    # Calcular EHIaccl
    data['EHIaccl'] = data['T3D'] - data['T30D']

    # Calcular EHF
    data['EHF'] = data['EHIsig'] * np.maximum(1, data['EHIaccl'])

    return data[['EHIsig', 'EHIaccl', 'EHF']]
