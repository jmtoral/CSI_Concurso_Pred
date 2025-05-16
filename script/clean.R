# Carga de paquetes
library(tidyverse)
library(cluster)       # Para silhouette
library(factoextra)    # Para visualización
library(Rtsne)         # Para visualización t-SNE

# Paso 1: Leer los datos
datos <- read_csv("data/raw/TradeBalance_5_15_2025_21_57_56.csv")

# Paso 2: Preprocesamiento y feature engineering
datos <- datos %>%
  filter(
    partnerISO != "World",
    motDesc != "TOTAL MOT"
  )

# Agregación anual previa para promediar perfiles por país
agg_anual <- datos %>%
  group_by(partnerISO, refYear,cmdDesc) %>%
  summarise(
    total_exports = sum(if_else(primaryValueX != 0, primaryValueX, 0)),
    total_imports = sum(if_else(primaryValueM != 0, primaryValueM, 0)),
    n_productos = n_distinct(cmdCode),
    prop_aereo = mean(motDesc == "Air", na.rm = TRUE),
    prop_terrestre = mean(motDesc == "Land", na.rm = TRUE),
    prop_maritimo = mean(motDesc == "Sea", na.rm = TRUE),
    prop_tren = mean(motDesc == "Railway", na.rm = TRUE),
    prop_otro = mean(!motDesc %in% c("Air", "Land", "Sea", "Railway"), na.rm = TRUE),
    .groups = "drop"
  )

# Promediar los valores por país (eliminar efecto temporal)
agg <- agg_anual %>%
  select(-refYear) %>% 
  group_by(partnerISO) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

# Paso 3: Normalización de variables numéricas
vars_norm <- agg %>%
  select(-partnerISO) %>%
  drop_na() %>%
  scale()

# Paso 4: Clustering con KMeans
set.seed(123)
k <- 3
kmeans_res <- kmeans(vars_norm, centers = k, nstart = 25)

# Paso 5: Evaluar Silhouette Score
sil <- silhouette(kmeans_res$cluster, dist(vars_norm))
mean_sil <- mean(sil[, 3])
print(paste("Silhouette promedio:", round(mean_sil, 3)))

