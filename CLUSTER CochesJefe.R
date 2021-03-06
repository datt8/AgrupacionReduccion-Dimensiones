

# Fase 1

## Lectura de los datos del archivo

library(foreign)
data.original <- data.frame(read.spss("Datos/tterreno.sav"))

# Para el posterior reparto (como resumen) se saca a formato .csv los coches. Luego se colocarán los garajes
data.original[, 1:2] -> resumen 
resumen$coche <- paste(resumen$marca, "-", resumen$modelo)

write.csv(resumen, "resumen.csv")

## Estructura de los datos
summary(data.original) # Se observa que existen varias variables que tienen valores ausentes (NA)
str(data.original) # Se observa también que existen algunas variables de carácter numérico y otras de tipo factor

## Estructura de la tabla

data <- data.original
data$coche <- paste(data$marca, "-", data$modelo) # Se crea una variable que es la unión de marca y modelo
data[,c(1,2)] <- NULL # Se eliminan las dos primeras variables (pues ya no tienen información)
data$coche <- with(data, make.unique(as.character(coche))) # Se crean registros únicos con la nueva variable
data <- data.frame(data[,-14], row.names = data[,14]) # Se coloca la nueva variable como nombre de la fila

## Limpieza de los datos

data$cilindro <- as.numeric(as.character(data$cilindro))
data$plazas <- as.numeric(as.character(data$plazas))
str(data) # Queda una variable factor, acel2, que no se cambiará pues será eliminada para el análisis
summary(data$acel2) # Casi todos están en la misma categoría
data <- data[,-13]
data$pvp <- round(data$pvp/166.386, -2) # Se pasa a euros y se redondea a la centena

## Correlación

library(corrplot)
corrplot(cor(na.omit(data))) # A pesar de haber omitido los ausentes, en el gráfico de correlación se ve gran relación entre algunas variables
# De esta manera, se eliminará la aceleración (muy correlacionada con la velocidad de forma negativa). Además tiene muchos ausentes.
# También se eliminará el cons120, pues un todoterreno no está dirigido hacia la conducción por carretera a altas velocidades

data.def <- data[, -c(9, 12)] # Estas serán las variables con las que se trabajará finalmente

## Tratamiento valores ausentes

library(tidyr)
library(dplyr)
summary(data.def) # Existen valores ausentes en: peso, cons90, consurb y velocida

### Ausentes de peso
data.def$peso <- replace_na(data.def$peso, 1800) # Tras ver el dataset se determina que un posible peso podría ser 1800, pues esos dos coches son muy parecidos a los otros Ford Maverick (aunque con menos valor en las variables)

### Ausentes de consumo 90
cons90_marca <- data.original %>%
  group_by(marca) %>%
  dplyr::summarize(media90 = mean(cons90, na.rm = TRUE))

data.def$cons90.2 <- ifelse(test = (data.original$marca == "NISSAN" & is.na(data.original$cons90)), yes = 8.4, no = data.original$cons90) # Se sustituye por la media de la marca
data.def$cons90.3 <- ifelse(test = (data.original$marca == "SSANGYONG" & is.na(data.original$cons90)), yes = 8.17, no = data.def$cons90.2) # Se sustituye por la media de la marca
data.def$cons90.4 <- ifelse(test = (data.original$marca == "UAZ" & is.na(data.original$cons90)), yes = 8, no = data.def$cons90.3) # Se observa el SSANGYONG Family RV (muy similar a los UAZ) y se determina poner 8 litros

data.def$cons90 <- round(data.def$cons90.4, 1)
data.def[, 11:13] <- NULL

### Ausentes consumo urbano

data.def$consurb.2 <- ifelse(test = (data.original$marca == "JEEP" & is.na(data.original$consurb)), yes = 9.8, no = data.original$consurb) # Se sustituye por el mismo valor que la version JAMB
data.def$consurb.3 <- ifelse(test = (data.original$marca == "NISSAN" & is.na(data.original$consurb)), yes = 12, no = data.def$consurb.2) # Tras observar todos los NISSAN se determina ese valor (Terrano 11.6 y Patrol 12.4)
data.def$consurb.4 <- ifelse(test = (data.original$marca == "TOYOTA" & is.na(data.original$consurb)), yes = 10.4, no = data.def$consurb.3) # Se sustituye por el valor del RAV4 5 puertas (muy similar)

data.def$consurb <- round(data.def$consurb.4, 1)
data.def[, 11:13] <- NULL

### Ausentes velocidad

data.def$velocida.2 <- ifelse(test = (data.original$marca == "TATA" & is.na(data.original$velocida)), yes = 135, no = data.original$velocida) # Mismo valor que el otro TATA (igual que el ausente)
data.def$velocida.3 <- ifelse(test = (data.original$marca == "SUZUKI" & is.na(data.original$velocida)), yes = 146, no = data.def$velocida.2) # Se sustituye por valor intermedio entre otros SUZUKI de 95 CV

data.def$velocida <- data.def$velocida.3
data.def[, 11:12] <- NULL

## Comprobación de NA'S
apply(X = data.def, MARGIN = 2, FUN = function(x){sum(is.na(x))}) # No hay NA'S ya

# Ya se tienen los datos totalmente limpios y como se quiere

# Aproximación (gráfico y datos escalados)

library(factoextra)
data.scale <- data.frame(scale(data.def)) # Se escalan los datos para eliminar las unidades de medida
dist.eucl <- get_dist(x = data.scale, method = "euclidean") # Se hallan las distancias entre los coches
fviz_dist(dist.obj = dist.eucl, lab_size = 5, 
          gradient = list(low = "red", mid = "lightsteelblue1", high = "white")) # Se crea el gráfico y se representa

# Fase 2 ¿Es conveniente realizar una técnica de clustering?

## Clusters óptimos gráficos

plot.j <- fviz_nbclust(data.scale, hcut, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  geom_vline(xintercept = 3, linetype = 3) +
  ggtitle("Número óptimo de clusters - jerárquico") +
  labs(x = "Número k de clusters", y = "Suma total de cuadrados intra grupos") # Gráfica para determinar el número óptimo de clusters (3)
plot.j

plot.k <- fviz_nbclust(data.scale, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  geom_vline(xintercept = 3, linetype = 3) +
  ggtitle("Número óptimo de clusters - k medias") +
  labs(x = "Número k de clusters", y = "Suma total de cuadrados intra grupos") # Gráfica para determinar el número óptimo de clusters (3)
plot.k

## Clusters óptimos NbClust

library(NbClust)
clusters.optimos <- NbClust(data = data.scale, distance = "euclidean", min.nc = 2, 
                            max.nc = 10, method = "complete", index = "all") # Por regla mayoritaria se deberían elegir 3

## ¿Es necesario el clustering? Estadístico Hopkins

library(clustertend)
set.seed(666) # Se fija la semilla para que la aleatoriedad no cambie
hopkins(data = data.scale, n = nrow(data.scale) - 1) # Como es cercano a 0 (estaría bien hacer clusters)

# De esta manera, se elegirán 3 clusters (ambos criterios dan esa como mejor respuesta). Y sí, se podrá usar clustering

# Fase 3. Clusters jerárquicos (Ward) y no jerárquicos (k-means)

## Jerárquico

library(cluster)
cluster.jerarquico <- hclust(dist.eucl, method = "ward.D")
plot(cluster.jerarquico, cex = 0.45, ylab = "", xlab = "", axes = FALSE, ann = FALSE) + 
  title(main = "Dendrograma")
rect.hclust(cluster.jerarquico, k = 3, border = 2:4) #Se cogen los 3 clusters

data.def -> tt.clus.j # Se crea un data frame para añadir la columna del cluster resultante
tt.clus.j$cluster <- cutree(cluster.jerarquico, k = 3)

## No jerárquico

cluster.kmeans <- kmeans(data.scale, 3)
fviz_cluster(object = list(data = data.scale, cluster = cluster.kmeans$cluster), 
             geom = "point", main = "K-Means", xlab = "", ylab = "", ellipse = TRUE)
tt.clus.kmeans <- data.def
tt.clus.kmeans$cluster <- cluster.kmeans$cluster


# Fase 4. Análisis de cada cluster

# Se crea para cada tipo de clusterización usada (jerárquica y no jerárquica, una tabla que con sus estadísticos)
ch.clusters.j <- tt.clus.j %>%
  group_by(cluster) %>%
  dplyr::summarize(precio = mean(pvp),
                   cilindros = median(cilindro),
                   cilindrada = mean(cc),
                   potencia = mean(potencia),
                   rpm = mean(rpm),
                   peso = mean(peso),
                   plazas = median(plazas),
                   consumo90 = mean(cons90),
                   consumourbano = mean(consurb),
                   velocidad = mean(velocida),
                   elementos = n())

ch.clusters.kmeans <- tt.clus.kmeans %>%
  group_by(cluster) %>%
  dplyr::summarize(precio = mean(pvp),
                   cilindros = median(cilindro),
                   cilindrada = mean(cc),
                   potencia = mean(potencia),
                   rpm = mean(rpm),
                   peso = mean(peso),
                   plazas = median(plazas),
                   consumo90 = mean(cons90),
                   consumourbano = mean(consurb),
                   velocidad = mean(velocida),
                   elementos = n())

# Fase 5. Reparto del cluster 3

# Como se tienen que repartir 74 coches se va a volver a realizar otro k-means va el reparto
cluster3 <- tt.clus.kmeans[(tt.clus.kmeans$cluster == 3),]

cluster3.scale <- data.frame(scale(cluster3)) # Se escala el data frame creado
cluster3.scale <- cluster3.scale[,-11] # se quita la variable cluster

cluster3.kmeans <- kmeans(x = cluster3.scale, centers = 5)

tt.cluster3.kmeans <- cluster3[,-11] # Se crea otro data frame quitando la columna 11 (cluster)
tt.cluster3.kmeans$cluster <- cluster3.kmeans$cluster # Se mete la columna cluster (otra vez) con el nuevo k-means

# Fase 6 Resumen coche - garaje

garajes <- read.csv("Datos/CochesGaraje.csv", sep = ";")
data.def -> data.resumen
data.resumen$garaje <- garajes$Garaje # Se incluye el garaje en el dataset original

data.resumen # Este dataset ya incluye todas las variables y el garaje de cada coche


resumen_garaje <- data.resumen %>%
  group_by(garaje) %>%
  dplyr::summarize(precio = mean(pvp),
                   cilindros = median(cilindro),
                   cilindrada = mean(cc),
                   potencia = mean(potencia),
                   rpm = mean(rpm),
                   peso = mean(peso),
                   plazas = median(plazas),
                   consumo90 = mean(cons90),
                   consumourbano = mean(consurb),
                   velocidad = mean(velocida),
                   elementos = n())

# En esta tabla se ve cada garaje caracterizado.

