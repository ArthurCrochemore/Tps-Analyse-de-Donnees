echo = FALSE
getwd()
setwd("D:/2Travail/annee 2019-2020/cours analyse des donnees/mes_tp/1tp_prise en main")
data(iris)
iris
dim(iris)
names(iris)
iris$Species
iris$Petal.Length
################
levels(iris$Species)
summary(iris$Species)
table(iris$Species)
pie(table(iris$Species))
barplot(table(iris$Species))
str(iris$Species)
T<-as.numeric(table(iris$Species))
dotchart(T)
#graphisme
#par(mfrow=c(n1,nc))
# exercice
a <- c(1,2,3) 
b <- c(4,5,6) 
par(mfrow = c(1,2))
barplot(a)
barplot(b)
pie(a)
pie(b)
dotchart(a)
dotchart(b)
#
par(mfrow=c(2,2))
pie(table(iris$Species))
barplot(table(iris$Species))
dotchart(table(iris$Species))
#par(mfcol=c(n1,nc))
par(mfcol=c(2,2))
pie(table(iris$Species))
barplot(table(iris$Species))
dotchart(table(iris$Species))
#fin exercice

# representation de la longueur du petale
# la troisieme colonne du fichier iris contient une variable quantite. pour resumer son contenu:
summary (iris$Petal.Length)
#Essayons de retrouver ces valeurs individuellement
min(iris$Petal.Length) 
max(iris$Petal.Length) 
sum(iris$Petal.Length) 
length(iris$Petal.Length) 
sum(iris$Petal.Length)/length(iris$Petal.Length)

sort(iris$Petal.Length)
# mise dans un fichier
ordlongPetal<-sort(iris$Petal.Length) 
ordlongPetal 
#on travaille sur le tableau, ici calcul de la moyenne de longueur des petales
sum(ordlongPetal)/length(ordlongPetal)
#recherche d'une cellule
ordlongPetal[37]
(ordlongPetal[92]+ordlongPetal[37])/2
#un mot sur l'histogramme
par(mfrow=c(1,1))
hist(iris$Petal.Length, col=grey(0.6), main="histogramme")
#Exercice: refaire ce travail avec les autres variables quantitatives du fichier Iris (largeur du p?tale, longueur et largeur du s?pale)
#
#representation de la longueur et largeur du petale, etude bivariee sous forme nuage de points
plot(iris$Petal.Length,iris$Petal.Width,xlab="Longueur du \npetale", ylab="largeur du petale", main= "Nuage de points", pch=20)
#Visualisation de points (individus) ayant les m?mes coordonn?es (superposition)
sunflowerplot(iris$Petal.Length,iris$Petal.Width,xlab="Longueur du petale",ylab="largeur du petale",main="Nuage de points", pch=20)
library(MASS)
densite<-kde2d(iris$Petal.Length,iris$Petal.Width)
filled.contour(densite, color=topo.colors, xlab="longueur du petale", ylab="largeur du petale") # to fill=remplir
#representation de la longueur des petales en fonction de l'espece avec la boite a moustaches
# et tout sur la meme planche
#exercice
par(mfrow = c(2, 2))
boxplot(iris$Petal.Length~iris$Species,col=grey(0.6))
boxplot(iris$Petal.Width~iris$Species,col=grey(0.6))
boxplot(iris$Sepal.Length~iris$Species,col=grey(0.6))
boxplot(iris$Sepal.Width~iris$Species,col=grey(0.6))
# les histogrammes
summary (iris)
par(mfrow = c(2, 2))
brk = seq(from = 0, to = 8, length = 20)
hist(iris$Petal.Length, main = "Ensemble des 150 iris", xlab = "Longueur du petale",breaks = brk)
hist(iris$Petal.Length[iris$Species == "setosa"], main = "Setosa",     xlab = "Longueur du petale", breaks= brk)
hist(iris$Petal.Length[iris$Species == "versicolor"], main = "Versicolor", xlab = "Longueur du petale", breaks = brk)
hist(iris$Petal.Length[iris$Species == "virginica"], main = "Virginica", xlab = "Longueur du petale", breaks = brk)
#affichage deux dimensions: longueur-largeur (tout espece confondue et ensuite par espece)
par(mfrow = c(2, 2))
plot(iris$Petal.Length, iris$Petal.Width, xlab = "Longueur du petale", ylab = "Largeur du petale", main = "Nuage de points", pch = 20)
plot(iris$Petal.Length[iris$Species == "setosa"], iris$Petal.Width[iris$Species =="setosa"], xlim = c(1, 6.9), ylim = c(0.1, 2.5), xlab = "Longueur du petale", ylab = "Largeur du petale", main = "iris setosa", pch = 20)
plot(iris$Petal.Length[iris$Species == "versicolor"], iris$Petal.Width[iris$Species =="versicolor"], xlim = c(1, 6.9), ylim = c(0.1, 2.5), xlab = "Longueur du petale", ylab = "Largeur du petale", main = "iris versicolor", pch = 20)
plot(iris$Petal.Length[iris$Species == "virginica"], iris$Petal.Width[iris$Species =="virginica"], xlim = c(1, 6.9), ylim = c(0.1, 2.5), xlab = "Longueur du petale", ylab = "Largeur du petale", main = "iris virginica", pch = 20)
#repr?sentation exaustive de toutes les comparaisons entre modalit?s et esp?ces.
new_data <- unclass(iris$Species)
pairs(iris[1:4], main = "Les iris de Fisher -- 3 especes", pch = c(21, 25, 24)[new_data], bg = c("red", "green3", "blue") [new_data], las = 3, gap = 0, labels = c("Longueur\nSepale","Largeur \nSepale","Longueur\nPetale","Largeur\nPetale"))
#gap: distance entre les carr?s, bg: couleur des points, las:orientation des titres des axes                                                                                               








