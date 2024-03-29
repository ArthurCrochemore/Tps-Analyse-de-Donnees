#Il y a 150 individus et 4 dimensions/variables. Les 150 sont groupes en 3 especes setosa, versicolor et virginica. Ce qu'on voudrait savoir,
#c'est s'il y a une difference importante entre les especes, et s'il y en a une, quelles sont les variables qui contribuent le plus a  expliquer cette
#difference.
#
#probleme pose: il s'agit de discriminer 3 especes d'Iris a partir de la mesure de la longueur et la largeur des petales et des sepales des 50 fleurs
#de chacune des especes. Ainsi, a partir de ces mesures, on propose de definir une regle permettant d'affecter a une espece determinee
#un iris dont on ne connait l'esp?ce.

#library
library(FactoMineR)
library(factoextra)
getwd()#Set of data:
setwd (dir="C:/Users/makris/Documents/mes_tp/2tp_iris acp")
getwd()
# setwd(dir="D:/2Travail/annee 2019-2020/cours analyse des donnees/mes_tp/2tp_iris acp "); getwd()
data(iris)
data=iris
data
head(iris)
levels(iris$Species)
summary (iris$Species)
table(iris$Species)
#realisation d'une ACP sur les variables numeriques (quantitatives)
res.PCA = PCA(data[,1:4,], scale.unit = TRUE, ncp=4, graph=F)# on travaille avec les 4 variables
res.PCA# donne toutes les valeurs potentielles disponibles suite a la realisation de l'acp
# Voir le script fourni intitul?: script_info_acp
#basic plot of the PCA:




par(mfrow=c(2,2))
plot.PCA(res.PCA, axes=c(1,2), choix="ind")
plot.PCA(res.PCA, axes=c(1,2), choix="var")
#Pour savoir quelles variables sont correlees avec chaque axe factoriel
dimdesc(res.PCA, axes=c(1,2)) # on verifiera les valeurs de correlations obtenues pour chaque variable et sur les deux premiers axes sur le cercle des correlations
#il peut etre interessant d'ajouter une couleur
res.PCA = PCA(data, scale.unit=TRUE, ncp=4, graph=F , quali.sup=5)#affiche le premier plan facoriel des individus et des variables
par(mfrow=c(1,2))
plot.PCA(res.PCA, axes=c(1, 2), choix="ind" , habillage=5)
plot.PCA(res.PCA, axes=c(1, 2), choix="var" , habillage=5)
#Si l'on a beaucoup de points, on peut personnaliser la representation
res.PCA = PCA(data, scale.unit=TRUE, ncp=4, graph=T , quali.sup=5)
my_colors=c( rgb(143,199,74,maxColorValue = 255), rgb(242,104,34,maxColorValue = 255), rgb(111,145,202,maxColorValue = 255))
sort(abs(res.PCA$ind$coord[,1]))# fournit les coordonnees des projections de chaque point "variable" sur l'axe1
res.PCA$ind$coord[,2]# idem mais pour l'axe 2
res.PCA$V
plot(res.PCA$ind$coord[,1] , res.PCA$ind$coord[,2]  , xlab="axis1" , ylab="axis2" , pch=20 , cex=1, col=my_colors[as.numeric(res.PCA$call$quali.sup$quali.sup[,1])] )
abline(h=0 , v=0)
col=my_colors[as.numeric(res.PCA$call$quali.sup$quali.sup[,1])] 
legend("bottomright" , legend=levels(res.PCA$call$quali.sup$quali.sup[,1] ) , col=my_colors  , pch=20 )
#autre ecriture pour appeler l'objet composantes principales
sink("tableau.txt") #cree un fichier qui va contenir ce qui suit
summary (res.PCA)
sink() #Marque la fin de la redirection du graphique vers le fichier



#graphique enrichi

# Changer le titre principal et celui des axes. InspirÃ© de l'article: STHDA fviz_pca_ Visualisation de l'Analyse en Composante Principale - Logiciel R et analyse de donnÃ©es
fviz_pca_ind(res.PCA) + labs(title ="PCA", x = "PC1", y = "PC2")
# Changer les limites des axes en spÃ©cifiant le min et le max
fviz_pca_ind(res.PCA) +  xlim(-4, 4) + ylim (-4, 4)
# Utiliser seulement du texte
fviz_pca_ind(res.PCA, geom="text")
# Utiliser uniquement des points
fviz_pca_ind(res.PCA, geom="point")
# Changer la taille des points
fviz_pca_ind(res.PCA, geom="point", pointsize = 4)
# Changer la couleur des points et le thÃ¨me
fviz_pca_ind(res.PCA, col.ind = "blue")+  theme_minimal()
# ContrÃ´ler automatiquement la couleurs des individus par les valeurs de cos2 ou de contributions cos2 = qualitÃ© de rÃ©prÃ©sentation sur le graphique
fviz_pca_ind(res.PCA, col.ind="cos2")
# Gradient de couleur
fviz_pca_ind(res.PCA, col.ind="cos2") +  scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.6, space = "Lab")
# Changer le thÃ¨me et utiliser uniquement des points
fviz_pca_ind(res.PCA, col.ind="cos2", geom = "point") + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.6, space = "Lab")+ theme_minimal()
# Colorer en fonction de la contribution
fviz_pca_ind(res.PCA, col.ind="contrib") + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=4, space ="Lab")
# ContrÃ´ler la transparence des couleurs
# en fonction de la contribution
fviz_pca_ind(res.PCA, alpha.ind="contrib") +  theme_minimal()
# Colorer les individus par groupes
fviz_pca_ind(res.PCA, label="none", habillage=iris$Species)
# Ajouter des ellipses
p<-fviz_pca_ind(res.PCA, label="none", habillage=iris$Species,addEllipses=TRUE, ellipse.level=0.95)
print(p)
# Changer la couleur des groupes en utilisant les palettes RColorBrewer
p + scale_color_brewer(palette="Dark2") + theme_minimal()
p + scale_color_brewer(palette="Paired") + theme_minimal()
p + scale_color_brewer(palette="Set1") +  theme_minimal()
# Changer la couleur manuellement
p + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
# SÃ©lectionner et visualiser les individus avec cos2 > 0.96
fviz_pca_ind(res.PCA, select.ind = list(cos2 = 0.9989))
# SÃ©lectionner le top 20 selon le cos2
fviz_pca_ind(res.PCA, select.ind = list(cos2 = 20))
# Selectionner le top 20 selon la contribution
fviz_pca_ind(res.PCA, select.ind = list(contrib = 20))
# SÃ©lectionner par le nom
fviz_pca_ind(res.PCA, select.ind = list(name = c("23", "42", "119")))


# Graphique par dÃ©faut
fviz_pca_var(res.PCA)
# Utiliser des points et textes
fviz_pca_var(res.PCA, geom = c("point", "text"))
# Changer la couleur et le thÃ¨me
fviz_pca_var(res.PCA, col.var="steelblue")+  theme_minimal()
# ContrÃ´ler la couleur selon la contribution
fviz_pca_var(res.PCA, col.var="contrib")+ scale_color_gradient2(low="white", mid="blue", high="red", midpoint=96, space ="Lab") + theme_minimal()
# ContrÃ´ler la transparence des variables
# selon leurs contributions
fviz_pca_var(res.PCA, alpha.var="contrib") +  theme_minimal()
# SÃ©lectionner et visualiser les variables avec cos2 >= 0.96
fviz_pca_var(res.PCA, select.var = list(cos2 = 0.96))
# SÃ©lectionner le top 3 selon la contribution
fviz_pca_var(res.PCA, select.var = list(contrib = 3))
# SÃ©lectionner par noms
fviz_pca_var(res.PCA,  select.var= list(name = c("Sepal.Width", "Petal.Length")))

#fviz_pca_biplot(): Biplot des individus et variables
fviz_pca_biplot(res.PCA)
# Annoter uniquement les variables
fviz_pca_biplot(res.PCA, label ="var")
# Annoter uniquement les individus
fviz_pca_biplot(res.PCA, label ="ind")
# Cacher les variables
fviz_pca_biplot(res.PCA, invisible ="var")
# Cacher les individus
fviz_pca_biplot(res.PCA, invisible ="ind")
# ContrÃ´ler la couleur des individus selon le cos2
fviz_pca_biplot(res.PCA, label ="var", col.ind="cos2") +  theme_minimal()
# Change la couleur par groupe, ajouter des ellipses
fviz_pca_biplot(res.PCA, label="var", habillage=iris$Species, addEllipses=TRUE, ellipse.level=0.95)
# Top 30 des individus les plus contributifs
fviz_pca_biplot(res.PCA, label="var", select.ind = list(contrib = 30))

#export des informations dans un pdf
#etape1: création des graphiques comme objet R
scree.plot <-fviz_eig(res.PCA) #graphique des valeurs propres ou variance expliquée par chaque axe
ind.plot<-fviz_pca_ind(res.PCA, label="none", habillage=iris$Species,addEllipses=TRUE, ellipse.level=0.95) #graphique des individus
var.plot<-fviz_pca_var (res.PCA) #graphique des variables
#etape2, export des graphique dans le fichier
pdf ("results.pdf") # creation d'un peripherique pdf
print (scree.plot)
print (ind.plot)
print (var.plot)
dev.off()# fermeture du peripherique pdf

