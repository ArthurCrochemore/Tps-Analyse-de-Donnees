echo = TRUE 
data(iris) 
iris 
dim(iris) 
names(iris) 
iris$Species
#Permet
iris$Petal.Length
iris$Petal.Width
###################
iris$Specieste
levels(iris$Species)
table(iris$Species)
pie(table(iris$Species)) 
barplot(table(iris$Species)) 
dotchart(table(iris$Species))

#Pour le box plot on a un point blanc, qui reprensete une "anomalie" c'est a dire que on considere 
#dans un premier temps que c'etait le minimum mais lors du test de student il permet d'exlure des données (dont le point blanc)
# Le pour quoi, je sais pas ¯\(°_o)/¯ 
a<-c(1,2,3)
b<-c(4,5,6)
par(mfrow=c(2,2))
par(mfrow = c(1,2))
barplot(a)
barplot(b)
pie(a)
pie(b)
dotchart(a)
dotchart(b)
################
#par(mfrow=c(n1,nc))
par(mfrow=c(2,2))
pie(table(iris$Species))
barplot(table(iris$Species))
dotchart(as.numeric(table(iris$Species)))
#par(mfcol=c(n1,nc))
par(mfcol=c(2,2))
pie(table(iris$Species))
barplot(table(iris$Species))
dotchart(as.numeric(table(iris$Species)))
summary (iris$Petal.Length)
###########
min(iris$Petal.Length) 
max(iris$Petal.Length) 
sum(iris$Petal.Length) 
length(iris$Petal.Length) 
sum(iris$Petal.Length)/length(iris$Petal.Length) 
sort(iris$Petal.Length) 

par(mfrow = c(2, 2)) 
plot(iris$Petal.Length, iris$Petal.Width, xlab = "Longueur du petale", ylab = "Largeur du petale", main = "Nuage de 
points", pch = 20) 
plot(iris$Petal.Length[iris$Species == "setosa"], iris$Petal.Width[iris$Species =="setosa"], xlim = c(1, 6.9), ylim = 
       c(0.1, 2.5), xlab = "", ylab = "", main = "iris setosa", pch = 20) 
plot(iris$Petal.Length[iris$Species == "versicolor"], iris$Petal.Width[iris$Species =="versicolor"], xlim = c(1, 6.9), ylim 
     = c(0.1, 2.5), xlab = "", ylab = "", main = "iris versicolor", pch = 20) 
plot(iris$Petal.Length[iris$Species == "virginica"], iris$Petal.Width[iris$Species =="virginica"], xlim = c(1, 6.9), ylim = 
       c(0.1, 2.5), xlab = "", ylab = "", main = "iris virginica", pch = 20)

#nouveau tableau sans colonne Species 
new_data <- unclass(iris$Species) 
pairs(iris[1:4], main = "Les iris de Fisher -- 3 especes", pch = c(21, 25, 24)[new_data], bg = c("red", "green3", 
                                                                                                 "blue")[new_data], las = 2, gap = 0, labels = c("Longueur\nSepale", "Largeur\nSepale", 
                                                                                                                                                 "Longueur\nPetale","Largeur\nPetale")) 