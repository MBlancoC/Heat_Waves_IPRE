import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt

# Cargar el shapefile
shapefile_path = "shape_RM.shp"
gdf = gpd.read_file(shapefile_path)

# Cargar el archivo Excel
excel_path = "comunas_info.xlsx"
df = pd.read_excel(excel_path)

# Unir los datos del shapefile con los datos del Excel
merged = gdf.merge(df, left_on="Comuna", right_on="Comuna")

# Crear un gráfico de ejemplo
fig, ax = plt.subplots(1, 1, figsize=(10, 10))
merged.plot(column='Population', ax=ax, legend=True, cmap='OrRd')
plt.title('Population by Comuna')
plt.show()
