# Programa de Investigación en Pregrado (IPRE): Heat Waves in Santiago de Chile

## CLIMATE CHANGE AND URBAN HEALTH: HOW AIR POLLUTION, TEMPERATURE, AND CITY STRUCTURE RELATE TO PRETERM BIRTH
**FONDECYT N°11240322**

### Autor:
- **Manuel Blanco** – Estudiante College Ciencias Naturales y Matemáticas, Major en Ingeniería de Software, Pontificia Universidad Católica de Chile.

### Mentora:
- **Estela Blanco** – College UC y Escuela de Salud Pública, Pontificia Universidad Católica de Chile.

---

## Objetivo del Repositorio
Este repositorio almacena los datos y códigos utilizados en el análisis de olas de calor en Santiago de Chile, con el objetivo de entender su relación con factores urbanos y su impacto en la salud.

---

## Objetivo del IPRE
Evaluar cómo el cambio climático y la estructura de la ciudad influyen en la frecuencia e intensidad de olas de calor en Santiago de Chile.

(incluir objetivo general y especificos)

---

## Fuente de los Datos
Estos datos corresponden a mediciones climáticas y datos de temperatura máxima agregados a nivel comunal en la Región Metropolitana.
- Los datos se encuentran en la carpeta **`data`**.
- Los polígonos de las comunas de la Región Metropolitana están disponibles en la carpeta **`Polígonos`**.

---

## Principales Productos
El análisis realizado en este proyecto genera los siguientes productos principales:
1. **Tablas:**
   - Cantidad de olas de calor por comuna.
   - Cálculos de percentiles (90, 95 y 99) para la temperatura máxima.
   - Resultados de indicadores como el **Excess Heat Factor (EHF)**.
2. **Gráficos:**
   - Mapas temáticos que muestran la distribución de olas de calor por comuna según diferentes umbrales (>=30°C, >=percentil 90, >=percentil 95, >=percentil 99).
   - Resaltado de las comunas con mayor incidencia de olas de calor.
3. **Funciones:**
   - Identificación de olas de calor basadas en percentiles.
   - Implementación del cálculo de **Excess Heat Factor (EHF)**.
4. **Código:** Implementaciones en Python para el procesamiento de datos y generación de visualizaciones.

---

## Estructura del Repositorio
- **`data/`**: Contiene los datos climáticos y comunales utilizados en el análisis.
- **`Polígonos/`**: Archivos JSON con las geometrías de las comunas de la Región Metropolitana.
- **Jupyter Notebook**: Scripts utilizados para el análisis y generación de resultados.
- **Resultados Visuales**: Mapas y gráficos generados.

---

## Contacto
Para preguntas o colaboración, contactar a Manuel Blanco a través de manuelblanco@uc.cl.
