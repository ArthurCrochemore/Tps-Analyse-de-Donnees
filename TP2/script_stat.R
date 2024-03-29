library (ggplot2)
library (FactoMineR)
library (factoextra)
library (xlsx)

getwd() #affiche la localisation du répertoire de travail sous la forme d'un chemin absolu.
# setwd(dir="D:/2Travail/annee 2019-2020/cours analyse des donnees/mes_tp/2tp_iris acp "); getwd()  
setwd(getwd())

data(iris)
write.xlsx(iris, file="fichier_iris.xlsx", sheetName="donnees_iris")# sheetName impose le nom du document dans le classeur

corr=round(cor(iris[,1:4]),3)
write.xlsx(corr, file="correlations_iris.xlsx", sheetName="correlations_iris")

cov= round(cov(iris[,1:4]),3)
write.xlsx(cov, file="covariances_iris.xlsx", sheetName="covariances_iris")

moyenne=round(apply(iris[,1:4],MARGIN=2,FUN=mean),3) # moyenne des colonnes,
write.xlsx(moyenne, file="moyennes_iris.xlsx", sheetName="moyennes_iris")

variance=round(apply(iris[,1:4],MARGIN=2,FUN=var),3)
write.xlsx(variance, file="variances_iris.xlsx", sheetName="donnees_iris")

centre_reduit=round(scale(iris[,1:4]),2)
write.xlsx(centre_reduit, file="centre_reduit_iris.xlsx", sheetName="donnees_centrees_reduites_iris")



sort(res.PCA$ind$coord[,3])

sort(iris$Sepal.Width)



irislongueursApresProjection = (sqrt(res.PCA$ind$coord[,1]^2 + res.PCA$ind$coord[,2]^2))
sort(longueursApresProjection)

coordPoint118AvantProjection = scale(iris[,1:4])[118,]
coordPoint118AvantProjection
longueurAvantProjection <- sqrt(sum(coordPoint118AvantProjection^2))
longueurApresProjection <- longueursApresProjection[118]
longueurAvantProjection
longueurApresProjection
longueurApresProjection / longueurAvantProjection
#COORD Point 122
res.PCA$ind$coord[,1][122]
res.PCA$ind$coord[,2][122]
res.PCA$ind$coord[,3][122]
res.PCA$ind$coord[,4][122]

#COORD Point 118
res.PCA$ind$coord[,1][118]
res.PCA$ind$coord[,2][118]
res.PCA$ind$coord[,3][118]
res.PCA$ind$coord[,4][118]

#COORD Point 118
res.PCA$ind$coord[,1][42]
res.PCA$ind$coord[,2][42]
res.PCA$ind$coord[,3][42]
res.PCA$ind$coord[,4][42]
sort(distances)

plot.PCA(
  res.PCA,
  axes=c(1, 2),
  choix="ind" ,
  habillage="cos2"
)

moyenne2=round(apply(centre_reduit[,1:4],MARGIN=2,FUN=mean),3) # moyenne des colonnes,
write.xlsx(moyenne2, file="moyennes_centre_reduit_iris.xlsx", sheetName="moyennes_iris")

variance2=round(apply(iris[,1:4],MARGIN=2,FUN=mean),3)
write.xlsx(variance2, file="variances_centre_reduite_iris.xlsx", sheetName="donnees_iris")

res.PCA = PCA(iris[,1:4,], scale.unit = TRUE, ncp=4, graph=F)

plot.PCA(
  res.PCA,
  axes=c(1, 2),
  choix="var" ,
  habillage="cos2"
)
plot.PCA(
  res.PCA,
  axes = c(1, 3), # Sélectionnez les axes 1 et 3
  choix = "ind",  # Sélectionnez "ind" pour afficher les individus
  habillage = iris$Species # Colorer les points en fonction de la variable qualitative
)
fviz_pca_ind(res.PCA, axes = c(1,2), label="none", habillage=iris$Species)

fviz_pca_var(res.PCA)
fviz_eig(res.PCA)
fviz_pca_ind(res.PCA, select.ind = list(cos2 = 2))

result = PCA(X = iris, scale.unit = TRUE, ncp = 4, quali.sup = 5, graph = T)


fviz_eig(res.PCA, addlabels = TRUE, ylim = c(0, 50))  


cos2_PC1 <- res.PCA$ind$cos2[,1]

sort(res.PCA$ind$cos2[,3])
