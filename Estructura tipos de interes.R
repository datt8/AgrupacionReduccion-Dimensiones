read.csv2("ACPTIUSD.csv",header=TRUE, sep=";", dec=".")->data_original
library(factoextra)
library(FactoMineR)
#Cargamos librerías y el dataset con el que trabajamos

str(data_original)
datos_activos<-data_original[1:949,2:10]
datos_supl<-data_original[950:978,2:10]
#Se ve la estructura de los datos y se hacen los dos grandes grupos para el modelo predictivo

acp=PCA(datos_activos, graph=T)
#Graficamos el PCA

cor(datos_activos)->matriz.corr
library(corrplot)
corrplot(matriz.corr,method="square", type="upper")
#Sacamos la primera columna (no numérica) y calculamos la matriz de correlaciones y graficamos para una mejor visualización

det(matriz.corr)
#Determinante de la matriz de correlaciones. Al ser casi nulo, muestra una inequivoca correlacion entre las variables

library(psych)
m_datos_activos<-datos_activos[sample(nrow(datos_activos),100),]
cortest.bartlett(matriz.corr,nrow(m_datos_activos))
#Con esto realizamos la prueba de esfericidad de Bartlett. Con ese p-value rechazamos la hipotesis nula

KMO(matriz.corr)
#Valores altos (>80%) aconsejan (firmemente) el uso de análisis factorial. El MSA pasa igual

fviz_eig(acp, addlabels=TRUE, hjust=-0.3)+
  labs(x="Dimensiones", y="Varianza explicada")+
  theme_minimal()
#Grafico sedimentacion
round(acp$eig,5)
#Valor autovalores

apply(as.matrix(acp$var$cos2), 1, sum) #Comprobación de que las comunalidades dan 1 (dan muy cercano a 1)
apply(as.matrix(acp$var$contrib), 2, sum) #Comprobacion de que todas las dimensiones suman 100

datos_norm = data.frame (scale (datos_activos)) 
sapply(datos_norm, mean, na.rm = T) #Comprobacion medias
sapply(datos_norm, sd , na.rm = T) #Comprobación desviaciones

acp2=prcomp(datos_norm) #Segundo ACP
summary(acp2) #Resumen ACP2

acp2_rot = principal(datos_norm, nfactors=9, rotate="varimax") #Modelo rotado
acp2_rot$loadings #Cargas

acp2_rot$communality #Comunalidad (todas suman 1)

supl.acp= PCA(data_original[,-1], ind.sup= 950:978,  quanti.sup = 10, graph=TRUE) #ACP con suplementarias

supl.acp$quanti.sup #Posición suplementarias

fviz_pca_ind(supl.acp, col.ind= "cos2", col.ind.sup = "red", repel = FALSE, 
             pointsize=1,
             labelsize = 3,
             label = "sup",
             #jitter = list(what = "label", width = NULL, height = NULL)
) #Gráfico posicion variables suplementarias
