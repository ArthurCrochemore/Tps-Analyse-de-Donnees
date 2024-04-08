#chargement des bibliotheques necessaires

library(FactoMineR)
library(factoextra)
#library(Rcmdr)
library(RcmdrMisc)


getwd() # quel est le chemin par defaut?
#setwd(dir="D:/5tp_afc")  #je fixe mon repertoire de travail
#getwd() # je verifie
#
enquete<-read.table("fichier_data.csv",header=TRUE, sep=";", dec=".", row.names=1, check.names = FALSE, fileEncoding="latin1")
enquete
enquete <- enquete[1:3]
enquete
#
dimnames(enquete)[[1]][1]<-"les deux conjoints travaillent"
dimnames(enquete)[[1]][2]<-"le travail du mari plus absorbant"
dimnames(enquete)[[1]][3]<-"Seul le mari travaille"
dimnames(enquete)[[2]][1]<-"Rester au foyer"
dimnames(enquete)[[2]][2]<-"Travailler a mi-temps"
dimnames(enquete)[[2]][3]<-"travailler a plein-temps"
# calcul des marges lignes et colonnes du tableau de contingence
enquete_marges <- enquete
enquete_marges
enquete_marges$Total1 <-rowSums(enquete_marges)
enquete_marges$Total1
colSums(enquete_marges)
enquete_marges[nrow(enquete_marges)+1, ] <-colSums(enquete_marges)
row.names(enquete_marges)[nrow(enquete_marges)]<-"Total"
enquete_marges
enquete
#calcul des pourcentages ligne (profil ligne)
enquete_pourcentage_ligne <- enquete
enquete_pourcentage_ligne[nrow(enquete_pourcentage_ligne)+1,]<-colSums(enquete_pourcentage_ligne)
row.names(enquete_pourcentage_ligne)[nrow(enquete_pourcentage_ligne)]<-"profil ligne moyen"
enquete_pourcentage_ligne<-rowPercents(enquete_pourcentage_ligne)
enquete_pourcentage_ligne
 #calcul des pourcentage colonne (profil colonne)
enquete_pourcentage_colonne <- enquete
enquete_pourcentage_colonne$Total<-rowSums(enquete_pourcentage_colonne)
enquete_pourcentage_colonne<-colPercents(enquete_pourcentage_colonne)
dimnames(enquete_pourcentage_colonne)[[2]][4]<-"Profil colonne moyen"
enquete_pourcentage_colonne
#calcul du tableau de probabilit?
enquete_tableau_probabilite<-enquete_marges / 1724
enquete_tableau_probabilite
#calcul du Khi2
khi2enquete<- chisq.test(enquete)
khi2enquete
#calcul du tableau des effectifs reels/obsservees
donnees_observees<-khi2enquete$observed
donnees_observees
#calcul tableau des effectifs th?oriques
donnees_theorique<-khi2enquete$expected
donnees_theorique
#calcul du tableau des residus
donnees_residus<-khi2enquete$residuals
donnees_residus
#calcul de l'AFC
afc_enquete<-CA(enquete)
#calcul de l'AFC avec les donn?es du mod?le d'ind?pendance
afc_enquete_independance<- CA(khi2enquete$expected)
#
summary(afc_enquete_independance)
#
plot(afc_enquete, cex = 0.7, cex.axis = 0.6, cex.lab = 0.8, title = "Représentation graphique", selectRow = "cos2 0.7", selectCol = "cos2 0.7")
#
summary(afc_enquete, nbelements = Inf)
