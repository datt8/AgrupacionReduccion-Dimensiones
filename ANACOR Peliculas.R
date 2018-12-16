peliculas<-matrix(c(70,45,30,0,35,0,45,30,80,5,0,0,30,20,10), nrow = 5, ncol = 3)
rownames(peliculas)<-c("Terror", "Comedia", "Drama", "Accion", "Otras")
colnames(peliculas)<-c("<25", "25-50", ">50")
peliculas.df<-as.data.frame(peliculas)

test_ind<-chisq.test(peliculas)
test_ind

library(ggpubr)
ggballoonplot(peliculas.df, fill = "value")+
  scale_fill_viridis_c(option = "B")

library(FactoMineR)
library(factoextra)
pelis.ca=CA(as.table(peliculas), graph = FALSE)
summary(pelis.ca, nb.dec = 2, ncp = 2)

eig = get_eigenvalue(pelis.ca)
traza = sum(eig[1:2])
grad.asoc = sqrt(traza)
grad.asoc

fviz_ca_biplot(pelis.ca, map ="rowprincipal", arrow = c(TRUE, TRUE))