library (FactoMineR)
library (factoextra)
library (xlsx)
getwd() #affiche la localisation du r?pertoire de travail sous la forme d'un chemin absolu.
setwd(dir="D:/2Travail/annee 2019-2020/cours analyse des donnees/mes_tp/2tp_iris acp "); getwd()  
data(iris)
write.xlsx(iris, file="fichier_iris.xlsx", sheetName="données_iris")# sheetName impose le nom du document dans le classeur

corr=round(cor(iris[,1:4]),3)
write.xlsx(corr, file="correlations_iris.xlsx", sheetName="correlations_iris")

cov= round(cov(iris[,1:4]),3)
write.xlsx(cov, file="covariances_iris.xlsx", sheetName="covariances_iris")

moyenne=round(apply(iris[,1:4],MARGIN=2,FUN=mean),3)# moyenne des colonnes,
write.xlsx(moyenne, file="moyennes_iris.xlsx", sheetName="moyennes_iris")

variance=round(apply(iris[,1:4],MARGIN=2,FUN=var),3)
write.xlsx(variance, file="variances_iris.xlsx", sheetName="données_iris")

centre_reduit=round(scale(iris[,1:4]),2)
write.xlsx(centre_reduit, file="centre_reduit_iris.xlsx", sheetName="données_centrées_réduites_iris")

moyenne2=round(apply(centre_reduit[,1:4],MARGIN=2,FUN=mean),3)# moyenne des colonnes,
write.xlsx(moyenne2, file="moyennes_centre_reduit_iris.xlsx", sheetName="moyennes_iris")

variance2=round(apply(iris[,1:4],MARGIN=2,FUN=mean),3)
write.xlsx(variance2, file="variances_centre_reduite_iris.xlsx", sheetName="données_iris")

res.PCA = PCA(iris[,1:4,], scale.unit = TRUE, ncp=4, graph=F)
plot.PCA(res.PCA, axes=c(1, 2), choix="var" , habillage=5)
fviz_eig(res.PCA)

fviz_pca_ind(res.PCA, select.ind = list(cos2 = 2))

PCA(X = data, scale.unit = TRUE, ncp = 4, quali.sup = 5, graph = T)

