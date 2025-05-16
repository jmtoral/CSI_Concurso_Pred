# ---------- 1. Paquetes ----------
# Instala la primera vez si hace falta:
# install.packages(c("tidyverse", "factoextra", "cluster"))

library(tidyverse)   # dplyr, ggplot2, etc.
library(factoextra)  # visualizaciones de clustering
library(cluster)     # silhouette(), clusGap()
library(here)

source(here("script", "clean.R"))

# ---------- 2. Cargar / preparar los datos ----------
# Suponiendo que agg_anual ya está en memoria; de lo contrario, leer CSV o RDS
# Seleccionamos solo variables numéricas
agg_anual <- agg_anual %>% 
  sample_n(100)

agg_num <- agg_anual %>% 
  select(where(is.numeric)) %>%        # total_exports, prop_aereo, etc.
  drop_na()                            # elimina filas con NA en cualquiera

# ---------- 3. Estandarizar ----------
# Muy importante porque k-means usa distancias euclidianas
agg_scaled <- scale(agg_num)

# ---------- 4. Elegir k usando tres métricas distintas ----------

## 4a. Método del codo (total within-cluster sum of squares)
fviz_nbclust(agg_scaled,
             FUN = kmeans,
             method = "wss",           # within-sum-of-squares
             k.max = 10) +             # probar k = 1…10
  ggtitle("Método del codo")

## 4b. Silueta promedio
fviz_nbclust(agg_scaled,
             FUN = kmeans,
             method = "silhouette",
             k.max = 10) +
  ggtitle("Ancho promedio de silueta")

## 4c. Gap statistic (computacionalmente más pesado)
set.seed(123)                          # replicabilidad

agg_scaled <- scale(agg_num)
gap <- clusGap(agg_scaled,
               FUN = kmeans,
               K.max = 10,
               nstart = 25,            # reinicios por k
               B = 50)                 # nº de muéstreos bootstrap

fviz_gap_stat(gap) +
  ggtitle("Gap statistic")

# ---------- 5. Ajustar el modelo final ----------
# Elige el k “óptimo” a partir de las gráficas anteriores
k_opt <- 4                             # <-- cambia manualmente según el análisis

set.seed(2025)
km_fit <- kmeans(agg_scaled,
                 centers = k_opt,
                 nstart = 25)

# ---------- 6. Añadir clúster al dataset original ----------
agg_clusters <- agg_anual %>%
  drop_na() %>%                        # mismo filtrado que en agg_scaled
  mutate(cluster = factor(km_fit$cluster))

# ---------- 7. Explorar resultados ----------
# 7a. Distribución de tamaño
table(agg_clusters$cluster)

# 7b. Estadísticas de cada clúster
agg_clusters %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric),
                   list(media = mean, mediana = median),
                   .names = "{.col}_{.fn}"),
            .groups = "drop")

# 7c. Visualización 2D con PCA + k-means
fviz_cluster(km_fit,
             data  = agg_scaled,
             ellipse.type = "convex",
             repel = TRUE,
             ggtheme = theme_minimal())

library(tidyverse)
library(ggrepel)

# 1. PCA
pca <- prcomp(agg_scaled)

# 2. Dataset con componentes y clúster
pca_df <- as.data.frame(pca$x[, 1:2]) %>%
  mutate(cluster = factor(km_fit$cluster),
         partnerISO = agg_clusters$partnerISO)  # ISO 3

# 3. Visualización con etiquetas
ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.6, size = 2.5) +
  geom_text_repel(aes(label = partnerISO), size = 3, max.overlaps = 200) +  # Etiquetas ISO
  stat_ellipse(type = "norm", linetype = "dashed", alpha = 0.3) +
  labs(title = "Clustering con K-means y PCA",
       subtitle = "Etiquetas: partnerISO",
       x = "Componente Principal 1",
       y = "Componente Principal 2",
       color = "Clúster") +
  theme_minimal(base_size = 13)


#============ TOP 10

library(tidyverse)
library(ggrepel)

# ---------- 1. Hacer PCA ----------
pca <- prcomp(agg_scaled)

# ---------- 2. Construir data.frame con componentes y clúster ----------
pca_df <- as.data.frame(pca$x[, 1:2]) %>%
  mutate(cluster = factor(km_fit$cluster),
         partnerISO = agg_clusters$partnerISO)

# ---------- 3. Identificar los ISO más representados ----------
top_iso <- pca_df %>%
  count(partnerISO, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  pull(partnerISO)

# ---------- 4. Visualización final ----------
ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.6, size = 2.5) +
  geom_text_repel(
    data = filter(pca_df, partnerISO %in% top_iso),
    aes(label = partnerISO),
    size = 3.5,
    max.overlaps = Inf,
    box.padding = 0.5
  ) +
  stat_ellipse(type = "norm", linetype = "dashed", alpha = 0.3) +
  labs(title = "Visualización de Clustering con K-means y PCA",
       subtitle = "Se muestran etiquetas de los 10 países más frecuentes (ISO 3)",
       x = "Componente Principal 1",
       y = "Componente Principal 2",
       color = "Clúster") +
  theme_minimal(base_size = 14)


## Métricas

for (k in c(4, 6, 10)) {
  set.seed(42)
  km <- kmeans(agg_scaled, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(agg_scaled))
  cat(glue::glue("k = {k} → Silueta promedio: {round(mean(sil[, 'sil_width']), 3)}\n"))
}




## Interpretación


agg_clusters %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop")


agg_clusters %>%
  filter(cluster == 1) %>%
  count(partnerISO, sort = TRUE)

agg_clusters %>%
  filter(cluster == 1) %>%
  count(cmdDesc, sort = TRUE)


agg_clusters %>%
  group_by(cluster) %>%
  summarise(across(starts_with("prop_"), mean))
