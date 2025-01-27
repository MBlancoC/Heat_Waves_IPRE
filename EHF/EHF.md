El Excess Heat Factor (EHF) es una métrica diseñada para evaluar la intensidad de olas de calor, considerando tanto la anomalía de temperatura a largo plazo como la falta de aclimatación a cambios recientes en la temperatura.

### Concepto del EHF
El EHF combina dos componentes principales:
1. **EHIsig (Significance Excess Heat Index):** Evalúa cuán cálida es una temperatura promedio diaria de tres días (T3D) comparada con el percentil 95 (T95) del clima de referencia. Un EHIsig positivo indica condiciones de calor extremo relativo al clima local.
   \[
   EHIsig = T3D - T95
   \]

2. **EHIaccl (Acclimatization Excess Heat Index):** Mide la diferencia entre T3D y la temperatura promedio de los últimos 30 días (T30D), considerando que las personas se aclimatan al clima reciente.
   \[
   EHIaccl = T3D - T30D
   \]

El EHF se calcula como:
\[
EHF = EHIsig \cdot \max(1, EHIaccl)
\]
Un EHF positivo indica la presencia de una ola de calor, y valores más altos reflejan mayor severidad. Esta métrica es útil para monitorear y predecir olas de calor en un contexto de salud pública y adaptaciones al cambio climático.

---

### Código en Python

```python
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
