echo = TRUE
data(iris)
iris
dim(iris)
names(iris)

#Species
iris$Species
levels(iris$Species)
summary(iris$Species)
table(iris$Species)

pie(table(iris$Species))
barplot(table(iris$Species))
dotchart(table(iris$Species))

#Petal
iris$Petal.Length
levels(iris$Petal.Length)
summary(iris$Petal.Length)
table(iris$Petal.Length)

iris$Petal.Width
levels(iris$Petal.Width)
summary(iris$Petal.Width)

pie(table(iris$Petal.Width))
barplot(table(iris$Petal.Width))
dotchart(table(iris$Petal.Width))

#Sepal
iris$Sepal.Length
levels(iris$Sepal.Length)
summary(iris$Sepal.Length)
iris$Sepal.Width
levels(iris$Sepal.Width)
summary(iris$Sepal.Width)


a <- c(1,2,3)
b <- c(4,5,6)
par(mfrow = c(1,2))
barplot(a)
barplot(b)
pie(a)
pie(b)
dotchart(a)
dotchart(b)


par(mfrow = c(2, 2))

pie(table(iris$Species))
barplot(table(iris$Species))
dotchart(table(iris$Species))
dotchart(table(iris$Species))
 

summary (iris$Petal.Length)
min(iris$Petal.Length)
max(iris$Petal.Length)
sum(iris$Petal.Length)
length(iris$Petal.Length)
sum(iris$Petal.Length)/length(iris$Petal.Length)

sort(iris$Petal.Length)

ordlongPetal<-sort(iris$Petal.Length)
ordlongPetal
sum(ordlongPetal)/length(ordlongPetal)
ordlongPetal[37]
(ordlongPetal[92]+ordlongPetal[37])/2

hist(iris$Petal.Length, col=grey(0.6), main="histogramme")

par(mfrow = c(1, 2))

plot(iris$Petal.Length,iris$Petal.Width,xlab="Longueur du pétale", ylab="largeur du pétale", main= "Nuage de points", pch=20)
sunflowerplot(iris$Petal.Length,iris$Petal.Width,xlab="Longueur du pétale", ylab="largeur du pétale", main= "Nuage de points", pch=20)

par(mfrow = c(1, 1))

library(MASS)
densite<-kde2d(iris$Petal.Length,iris$Petal.Width)
filled.contour(densite, color=topo.colors, xlab="longueur du pétale", ylab="largeur du pétale")

boxplot(iris$Petal.Length~iris$Species,col=grey(0.6))

summary (iris)


par(mfrow = c(2, 2))
brk = seq(from = 0, to = 8, length = 20)
hist(iris$Petal.Length, main = "Ensemble des 150 iris", xlab = "Longueur du petale", breaks = brk)
hist(iris$Petal.Length[iris$Species == "setosa"], main = "Setosa", xlab = "Longueur du petale", breaks = brk)
hist(iris$Petal.Length[iris$Species == "versicolor"], main = "Versicolor", xlab = "Longueur du petale", breaks = brk)
hist(iris$Petal.Length[iris$Species == "virginica"], main = "Virginica", xlab = "Longueur du petale", breaks = brk)

brk = seq(from = 0, to = 8, length = 20)
hist(iris$Petal.Width, main = "Ensemble des 150 iris", xlab = "Largeur du petale", breaks = brk)
hist(iris$Petal.Width[iris$Species == "setosa"], main = "Setosa", xlab = "Largeur du petale", breaks = brk)
hist(iris$Petal.Width[iris$Species == "versicolor"], main = "Versicolor", xlab = "Largeur du petale", breaks = brk)
hist(iris$Petal.Width[iris$Species == "virginica"], main = "Virginica", xlab = "Largeur du petale", breaks = brk)


par(mfrow = c(2, 2))
plot(iris$Petal.Length, iris$Petal.Width, xlab = "Longueur du petale", ylab = "Largeur du petale", main = "Nuage de points", pch = 20)
plot(iris$Petal.Length[iris$Species == "setosa"], iris$Petal.Width[iris$Species =="setosa"], xlim = c(1, 6.9), ylim = c(0.1, 2.5), xlab = "", ylab = "", main = "iris setosa", pch = 20)
plot(iris$Petal.Length[iris$Species == "versicolor"], iris$Petal.Width[iris$Species =="versicolor"], xlim = c(1, 6.9), ylim = c(0.1, 2.5), xlab = "", ylab = "", main = "iris versicolor", pch = 20)
plot(iris$Petal.Length[iris$Species == "virginica"], iris$Petal.Width[iris$Species =="virginica"], xlim = c(1, 6.9), ylim = c(0.1, 2.5), xlab = "", ylab = "", main = "iris virginica", pch = 20)


#nouveau tableau sans colonne Species
new_data <- unclass(iris$Species)
pairs(iris[1:4], main = "Les iris de Fisher -- 3 especes", pch = c(21, 25, 24)[new_data], bg = c("red", "green3", "blue")[new_data], las = 2, gap = 0, labels = c("Longueur\nSepale", "Largeur\nSepale", "Longueur\nPetale","Largeur\nPetale"))
