library(readxl)
library(dplyr)
library(factoextra)
library(knitr)
library(psych)
library(cowplot)
library(ggdendro)
library(readxl)
library(skimr)
library(cluster)

datos <- read_excel("datos/datoscereales (1).xls")

datos <- datos |>
  mutate(
    marca = factor(marca),
    publico = factor(publico),
    paquete = factor(paquete),
    nombre = factor(nombre)
  )

str(datos)
dim(datos)
head(datos)
summary(datos)
skim(datos)

# Asignar el 'id' de los cereales como nombre de las filas
rownames(datos) <- datos$id

# seleccionar solo las variables de las características nutricionales
vars_to_cluster <- c(
  "calorias", "proteinas", "carbohidratos", "fibras",
  "lipidos", "sodio", "potasio", "vitamina", "hierro"
)

datos_to_cluster <- datos %>%
  select(all_of(vars_to_cluster))

summary(datos_to_cluster)


# Estandarizar las variable como paso previo
datos_to_cluster <- scale(datos_to_cluster)
summary(datos_to_cluster)

# Las distancias que se pueden calcular cambiando el argumento de ‘method’, son:
#   “maximum”, “manhattan”, “canberra”, “binary” (complemento de Jaccard), y “minkowski”
# (es necesario agregar a las opciones la potencia y raiz. Si p=2 -euclidea-)
#
# Otras sentencias para calculo de distancias/similaridades:
#
#   Utilizando la función ‘dist.binary()’ del paquete ‘ade4’ distintas distancias para variables dicotómicas:
#   distancia <- dist.binary(df, method = 2, diag = FALSE, upper = FALSE)
#
# Para elegir la funcion de distancia, se seleccionar un nro de 1 a 10 para el argumento ‘method’ (si pongo NULL me sale la ayuda)
#
# 1. JACCARD s1 = a/(a+b+c) –> d = sqrt(1 - s)
#
# 2. SM, SOCKAL & MICHENER s2 = (a+d)/(a+b+c+d) –> d = sqrt(1 - s)
#
# 3. SOCKAL & SNEATH s3 = a/(a+2(b+c)) –> d = sqrt(1 - s)
#
# 4. ROGERS & TANIMOTO s4 = (a+d)/(a+2(b+c)+d) –> d = sqrt(1 - s)
#
# 5. CZEKANOWSKI or SORENSEN s5 = 2*a/(2*a+b+c) –> d = sqrt(1 - s)
#
# 6. s6 = (a-(b+c)+d)/(a+b+c+d) –> d = sqrt(1 - s)
#
# 7. OCHIAI s7 = a/sqrt((a+b)(a+c)) –> d = sqrt(1 - s)
#
# 8. SOKAL & SNEATH s8 = ad/sqrt((a+b)(a+c)(d+b)(d+c)) –> d = sqrt(1 - s)
#
# 9. Phi of PEARSON s9 = ad-bc)/sqrt((a+b)(a+c)(b+d)(d+c)) –> d = sqrt(1 - s)
#
# 10. s10 = a/(a+b+c+d) –> d = sqrt(1 - s) and unit self-similarity

# distancia <- daisy(df, metric = “gower”, type = list(symm = c(6,7,8,9)))

# Calcular las distancias
distancia <- dist(datos_to_cluster, method = "euclidean")
distancia_canberra <- dist(datos_to_cluster, method = "canberra")
distancia_minkowski <- dist(datos_to_cluster, method = "minkowski", p = 5)


# Realizar el agrupamiento jerárquico
fit <- hclust(distancia, method = "ward.D")
fit_canberra <- hclust(distancia_canberra, method = "ward.D")
fit_minkowki <- hclust(distancia_minkowski, method = "ward.D")

# Las opciones de aglomeración para ‘method’, son:
#
#   “single”, “complete”, “average”, “mcquitty”, “median”, “centroid”, “ward.D”, “ward.D2”

# =======================================================
# 1.4 ¿ Cuantos grupos se identifican en el dendograma? ¿Existe alguna manera de determinar el número óptimo de clusters a considerar?

plot(fit, main = "Dendograma para distancia Enclidea.")
plot(fit_minkowki, main = "Dendograma para distancia Minkowski.")
plot(fit_canberra, main = "Dendograma para distancia Canberra")
# Otra forma de presentar el dendograma
# ggdendrogram(fit, rotate = TRUE, size = 2)

# Indentificar los distintos clusters elegidos
rect.hclust(fit, k = 4, border = "green")
rect.hclust(fit_canberra, k = 4, border = "orange")
rect.hclust(fit_minkowki, k = 4, border = "blue")

rect.hclust(fit, k = 5, border = "grey")

# =====================================================================================
# 1.5 ¿Existe alguna manera de determinar el número óptimo de clusters a considerar?

# Indice Silhouette  ---
fviz_nbclust(datos_to_cluster, kmeans, method = "silhouette") +
  labs(
    title = "Número óptimo de clusters a considerar",
    subtitle = "Indice Silhouette"
  )

# Suma de cuadrado error (o within)---
wss <- (nrow(datos_to_cluster) - 1) * sum(apply(datos_to_cluster, 2, var))
for (i in 2:10) {
  wss[i] <- sum(kmeans(datos_to_cluster, centers = i)$withinss)
}
plot(1:10, wss,
  type = "b", xlab = "Number of Clusters",
  ylab = "Suma de cuadrados dentro de los clusters (within)"
)

# =======================================================================================
# 1.6 ¿ Qué características tienen los grupos formados?

groups <- cutree(fit, k = 4)
groups_5 <- cutree(fit, k = 5)
groups_canberra <- cutree(fit_canberra, k = 4)
groups_minkowski <- cutree(fit_minkowki, k = 4)
groups
groups_canberra
groups_minkowski
# Unir los clusters formado con los datos para poder describir los grupos

datos_to_cluster <- data.frame(datos_to_cluster)
datos_to_cluster$cluster <- groups # Guarda los cluster en conjunto de datos estandarizdo que se utilizó para agrupar
datos_to_cluster$cluster_5 <- groups_5 # Guarda los cluster en conjunto de datos estandarizdo que se utilizó para agrupar
datos$cluster_5 <- groups_5 # Guarda los cluster en conjunto de datos original
datos$cluster <- groups # Guarda los cluster en conjunto de datos original


# Medias de las distintas variables para los clusters elegidos.
# calcular medias por grupos para todas las variables
df_means <- data.frame(
  datos %>%
    group_by(cluster) %>%
    summarise_at(vars(vars_to_cluster), list(name = mean))
)

df_means_5 <- data.frame(
  datos %>%
    group_by(cluster_5) %>%
    summarise_at(vars(vars_to_cluster), list(name = mean))
)


# Transponer el df de las medias
df_means_t <- setNames(data.frame(t(df_means[, -1])), df_means[, 1])
df_means_t_5 <- setNames(data.frame(t(df_means_5[, -1])), df_means_5[, 1])


kable(df_means_t, digits = 1)
kable(df_means_t_5, digits = 1)

# Distribución de las distintas variables según los clusters elegidos.

generar_boxplots_por_grupos <- function(df, var_interes, grupos = "cluster") {
  # Convertir a cuadro de datos
  df <- data.frame(df)

  # Definir etiquetas que con la cantidad de observaciones por grupos
  etiquetas <- paste(
    levels(factor(df[, grupos])), "\n(N = ", table(df[, grupos]), ")",
    sep = ""
  )

  # Generar gráfico
  boxplot <- ggplot(
    df,
    aes(
      x = factor(get(grupos)), y = get(var_interes),
      fill = factor(get(grupos))
    )
  ) +
    geom_boxplot() +
    theme(legend.position = "none") +
    scale_x_discrete(name = paste0(grupos), labels = etiquetas) +
    scale_y_continuous(name = paste0(var_interes)) +
    geom_hline(yintercept = median(df[, var_interes])) +
    theme(axis.text.x = element_text(size = rel(0.75)))

  return(boxplot)
}

# Distribución de las distintas variables según los clusters elegidos.


generar_boxplots_por_grupos_5 <- function(df, var_interes, grupos = "cluster_5") {
  # Convertir a cuadro de datos
  df <- data.frame(df)

  # Definir etiquetas que con la cantidad de observaciones por grupos
  etiquetas <- paste(
    levels(factor(df[, grupos])), "\n(N = ", table(df[, grupos]), ")",
    sep = ""
  )

  # Generar gráfico
  boxplot <- ggplot(
    df,
    aes(
      x = factor(get(grupos)), y = get(var_interes),
      fill = factor(get(grupos))
    )
  ) +
    geom_boxplot() +
    theme(legend.position = "none") +
    scale_x_discrete(name = paste0(grupos), labels = etiquetas) +
    scale_y_continuous(name = paste0(var_interes)) +
    geom_hline(yintercept = median(df[, var_interes])) +
    theme(axis.text.x = element_text(size = rel(0.75)))

  return(boxplot)
}
g1 <- generar_boxplots_por_grupos(df = datos, var_interes = "calorias")
g2 <- generar_boxplots_por_grupos(df = datos, var_interes = "proteinas")
g3 <- generar_boxplots_por_grupos(df = datos, var_interes = "carbohidratos")
g4 <- generar_boxplots_por_grupos(df = datos, var_interes = "fibras")
g5 <- generar_boxplots_por_grupos(df = datos, var_interes = "lipidos")
g6 <- generar_boxplots_por_grupos(df = datos, var_interes = "sodio")
g7 <- generar_boxplots_por_grupos(df = datos, var_interes = "potasio")
g8 <- generar_boxplots_por_grupos(df = datos, var_interes = "vitamina")
g9 <- generar_boxplots_por_grupos(df = datos, var_interes = "hierro")

plot_grid(g1, g2, g3, g4, g5, g6, g7, g8, g9, ncol = 3)


g1 <- generar_boxplots_por_grupos_5(df = datos, var_interes = "calorias")
g2 <- generar_boxplots_por_grupos_5(df = datos, var_interes = "proteinas")
g3 <- generar_boxplots_por_grupos_5(df = datos, var_interes = "carbohidratos")
g4 <- generar_boxplots_por_grupos_5(df = datos, var_interes = "fibras")
g5 <- generar_boxplots_por_grupos_5(df = datos, var_interes = "lipidos")
g6 <- generar_boxplots_por_grupos_5(df = datos, var_interes = "sodio")
g7 <- generar_boxplots_por_grupos_5(df = datos, var_interes = "potasio")
g8 <- generar_boxplots_por_grupos_5(df = datos, var_interes = "vitamina")
g9 <- generar_boxplots_por_grupos_5(df = datos, var_interes = "hierro")

plot_grid(g1, g2, g3, g4, g5, g6, g7, g8, g9, ncol = 3)


# Caracterizar los segmentos encontrados (utilizando las variables originales)
formula_para_describir <- as.formula(
  paste0(paste(vars_to_cluster, collapse = " + "), " ~ cluster")
)

tabla_resumen <- describeBy(
  formula_para_describir,
  mat = TRUE,
  data = datos
)

formula_para_describir_5 <- as.formula(
  paste0(paste(vars_to_cluster, collapse = " + "), " ~ cluster_5")
)

tabla_resumen_5 <- describeBy(
  formula_para_describir_5,
  mat = TRUE,
  data = datos
)

# Medidas datos#Medidas resúmenes (variables originales) para los clusters elegidos.
kable(
  tabla_resumen %>%
    dplyr::mutate(
      variable = rownames(.),
      cv = 100 * sd / abs(mean)
    ) %>%
    dplyr::rename(cluster = group1) %>%
    dplyr::select(variable, cluster, n, mean, median, cv, min, max) %>%
    arrange(as.numeric(as.character(cluster))),
  digits = 2
)

kable(
  tabla_resumen_5 %>%
    dplyr::mutate(
      variable = rownames(.),
      cv = 100 * sd / abs(mean)
    ) %>%
    dplyr::rename(cluster_5 = group1) %>%
    dplyr::select(variable, cluster_5, n, mean, median, cv, min, max) %>%
    arrange(as.numeric(as.character(cluster_5))),
  digits = 2
)

# # En función de las medias, las distribuciones gráficas y la medidas resúmenes por grupos analizadas,
# podemos resumir que los cuatro grupos de cereales se caracterizan por:
# #
# #   Cluster 1:
# #   Está formado por sólo 3 productos que presentan valores promedios considerablemente más bajos
#   que el resto de los productos en: calorías, carbohidratos y vitaminas, y además presentan valores
# medios considerablemente más altos en fibras y potasio.
# #
# # Cluster 2:
# #   Está formado por 14 productos que se destacan principalmente por tener valores altos en calorías
# y proteínas y bajos en hierro y sodio.
# #
# # Cluster 3:
# #   Lo conforman 13 productos con valores bajos de proteínas, fibras y lípidos y valores altos de
# vitaminas y carbohidratos. Esto grupo también se destaca por la baja dispersión en todas las variables,
# es decir, que los productos que forman este grupo son muy parecidos en cuanto a su características nutricionales.
# #
# # Cluster 4:
# #   Lo forman 13 productos que se diferencian principalmente por tener mayores valores de sodio y
# valores bajos de potasio. Además, poseen valores altos de hierro y carbohidratos y valores bajos de lipidos.


# 1.7 ¿Qué sucede si elegiéramos trabajar con un cluster más?
# ¿Cuáles de los grupos se divide? ¿en qué se diferencian los nuevos grupos formados?
# Se divide el cluster 2. Se abre en dos grupos de 5 y 9 individuos. Separa los individuos mas extremos.

datos |>
  filter(cluster_5 == 2)

datos |>
  filter(cluster_5 == 3)

# 1.8 ¿Cómo se distribuyen las variables categóricas ‘marca’, ’publico’ y ‘paquete’ en los clusters encontrados? ¿Qué se puede interpretar?

table(datos$cluster, datos$publico)
table(datos$cluster_5, datos$publico)


table(datos$cluster, datos$marca)
table(datos$cluster_5, datos$marca)

table(datos$cluster, datos$paquete)
table(datos$cluster_5, datos$paquete)

# ========================================================== PCA
# 1.9 A través del análisis de componentes principales realizado previamente, ¿se podía intuir la presencia de distintos grupos y sus características?


listofpackages <- c(
  "psych", "ggplot2", "knitr",
  "pastecs", "FactoMineR", "grid",
  "gridExtra", "ggfortify", "factoextra",
  "corrplot", "dplyr"
)
newPackages <- listofpackages[!(listofpackages %in% installed.packages()[, "Package"])]
if (length(newPackages)) install.packages(newPackages)
for (paquete in listofpackages) {
  suppressMessages(library(paquete, character.only = TRUE))
}

datos_to_cluster <- datos %>%
  select(all_of(vars_to_cluster))

datos_to_cluster <- scale(datos_to_cluster)

res <- pastecs::stat.desc(datos_to_cluster) %>%
  as.matrix() %>%
  as.data.frame() %>%
  round(2)

res <- format(res, scientific = FALSE)

knitr::kable(data.frame(res), digits = 2)

pairs.panels(datos_to_cluster,
  method = "pearson", # correlation method
  hist.col = "#00AFBB",
  density = TRUE, # show density plots
  ellipses = F # show correlation ellipses
)

mediPca <- prcomp(as.matrix(datos_to_cluster), center = T, scale. = T)
summary(mediPca)

plot(mediPca)
knitr::kable(as.data.frame(unclass(mediPca$rotation)))


knitr::kable((mediPca$x))
# biplot

autoplot(mediPca,
  data = datos,
  colour = "cluster_5",
  loadings = TRUE,
  loadings.label = TRUE,
  loadings.label.size = 4,
  size = 4
)

# ================================= FactoMineR =========================================

mundoPca <- FactoMineR::PCA(X = datos_to_cluster, scale.unit = T, ncp = ncol(datos), graph = T)

var <- get_pca_var(mundoPca)
# Coordinates (vectores propios)
knitr::kable(var$coord)
knitr::kable(head(var$cos2))
knitr::kable(var$contrib)
fviz_contrib(mundoPca, choice = "ind", axes = 1:2)


# 1.10 Repetir el análisis con otras medida de distancia de Canberra y de Minkowski con p = 5? .
# Cambian los grupos formados? Qué esperaría que ocurra con los ‘outliers’?
# Los grupos cambian. Por ejemplo
# groups euclidean
# [1] 1 2 2 3 4 1 2 2 3 3 3 2 2 3 3 3 4 4 3 3 1 2 3 3 2 2 2 2 3 2 3 2 2 2 3 3 3 4 2 3 3 3 2
#> groups_canberra. Cambia.
# [1] 1 2 2 3 3 1 2 2 3 3 3 4 4 3 3 3 3 3 3 3 1 3 3 3 4 2 2 4 3 4 3 2 2 2 3 3 3 3 3 3 3 3 3
#> groups_minkowski = eucli
# [1] 1 2 2 3 4 1 2 2 3 3 3 3 3 3 3 3 4 4 3 3 1 2 3 3 2 2 2 3 3 3 3 2 2 2 3 3 3 4 2 3 3 3 2

# Los outliers mantienen sus grupos. Son penadas las grandes distancias en eucli y minkowski

# 1.11 ¿Cómo se pueden incorporar esas variables categóricas al análisis?
# ¿Cómo varía el agrupamiento?
# Podemos usar "distancia de gower" para que tenga en cuenta las categoricas.
# Gower solo toma tipos de dato factor y numericos. Ojo que nombre tiene 41 factores distintos.

# ================================================= GOWER CON VARS CATEGORICAS

datos <- select(datos, marca, publico, paquete, preciox350g, calorias, proteinas, carbohidratos, fibras, lipidos, sodio, potasio, vitamina, hierro)
distancia_gower <- daisy(datos, metric = "gower")
fit_gower <- hclust(distancia_gower, method = "ward.D")
grupos_gower <- cutree(fit, k = 4)
datos$cluster_gower <- grupos_gower
plot(fit_gower)
rect.hclust(fit_gower, k = 4, border = "green")
grupos_gower <- cutree(fit_gower, k = 4)

df_means_gower <- data.frame(
  datos %>%
    group_by(cluster_gower) %>%
    summarise_at(vars(vars_to_cluster), list(name = mean))
)

kable(df_means_t_gower, digits = 1)

df_means_t_gower <- setNames(data.frame(t(df_means_gower[, -1])), df_means_gower[, 1])
generar_boxplots_por_grupos_gower <- function(df, var_interes, grupos = "cluster_gower") {
  # Convertir a cuadro de datos
  df <- data.frame(df)

  # Definir etiquetas que con la cantidad de observaciones por grupos
  etiquetas <- paste(
    levels(factor(df[, grupos])), "\n(N = ", table(df[, grupos]), ")",
    sep = ""
  )

  # Generar gráfico
  boxplot <- ggplot(
    df,
    aes(
      x = factor(get(grupos)), y = get(var_interes),
      fill = factor(get(grupos))
    )
  ) +
    geom_boxplot() +
    theme(legend.position = "none") +
    scale_x_discrete(name = paste0(grupos), labels = etiquetas) +
    scale_y_continuous(name = paste0(var_interes)) +
    geom_hline(yintercept = median(df[, var_interes])) +
    theme(axis.text.x = element_text(size = rel(0.75)))

  return(boxplot)
}

g1 <- generar_boxplots_por_grupos_gower(df = datos, var_interes = "calorias")
g2 <- generar_boxplots_por_grupos_gower(df = datos, var_interes = "proteinas")
g3 <- generar_boxplots_por_grupos_gower(df = datos, var_interes = "carbohidratos")
g4 <- generar_boxplots_por_grupos_gower(df = datos, var_interes = "fibras")
g5 <- generar_boxplots_por_grupos_gower(df = datos, var_interes = "lipidos")
g6 <- generar_boxplots_por_grupos_gower(df = datos, var_interes = "sodio")
g7 <- generar_boxplots_por_grupos_gower(df = datos, var_interes = "potasio")
g8 <- generar_boxplots_por_grupos_gower(df = datos, var_interes = "vitamina")
g9 <- generar_boxplots_por_grupos_gower(df = datos, var_interes = "hierro")

plot_grid(g1, g2, g3, g4, g5, g6, g7, g8, g9, ncol = 3)

table(datos$cluster_gower, datos$publico)
table(datos$cluster_gower, datos$marca)
table(datos$cluster_gower, datos$paquete)

# [1] 1 1 1 1 2 1 1 1 2 2 3 3 4 2 2 2 2 4 4 2 1 4 3 3 4 1 1 4 4 4 4 1 4 4 3 3 2 4 3 3 3 3 3

# COMPARACION DE LAS DISTINTAS AGRUPACIONES CON SILUETTE.
#
sil_gower <- silhouette(grupos_gower, distancia_gower)
sil_eucl <- silhouette(groups, distancia)
sil_canbera <- silhouette(groups_canberra, distancia_canberra)
sil_minkowski <- silhouette(groups_minkowski, distancia_minkowski)

mean(sil_gower[, 3]) # Promedio global Gower
mean(sil_eucl[, 3]) # Promedio global Euclidiana
mean(sil_canbera[, 3]) # Es el mas alto, igual es una estructura debil
mean(sil_minkowski[, 3])

#“El clustering con Canberra tiene una silueta media de 0.3205995, la más alta que encontramos.
# Esto sugiere que produce una estructura de grupos más coherente para estos datos.”

plot(sil_gower, main = "Silueta - Distancia de Gower")
plot(sil_eucl, main = "Silueta - Distancia Euclidiana")
plot(sil_canbera, main = "Silueta - Distancia Canberra")
plot(sil_minkowski, main = "Silueta - Distancia Minkowski p 5")

# Cada barra representa una observación y su valor de silueta 
# s(i), que puede ir desde -1 a +1.
# Las observaciones están agrupadas por cluster.

# ANALISIS DE LOS OUTLIERS
outlier
outliers <- which(sil_canbera[, 3] < 0.1) # por ejemplo, silueta < 0.1
table(datos[outliers, ])

outliers <- which(sil_gower[, 3] < 0.1) # por ejemplo, silueta < 0.1
datos[outliers, ]
