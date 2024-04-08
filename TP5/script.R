library(FactoMineR)
library(factoextra)
library(jpeg)
library(RcmdrMisc)

dimension = 320*243

setwd(dir="C:/Users/33611/Dossier_codes/DC-S8/Tps-Analyse-de-Donnees/TP5")  #je fixe mon repertoire de travail
getwd() # quel est le chemin par defaut?

dossier <- "yalefaces/"
nbfichiers = length(fichiers)

fichiers <- list.files(dossier, pattern = "\\.jpg$", full.names = TRUE)
matrices_images <- vector("list", nbfichiers)
trident <- numeric(dimension)


nb <- 1
for (image in fichiers) {
  img <- readJPEG(image, TRUE)
  
  img_vector <- as.vector(img)
  
  trident <- trident + img_vector
  
  matrices_images[[nb]] <- img_vector  # Utiliser [[nb]] pour accéder à la position correcte dans la liste
  nb = nb + 1  # Incrémenter l'index
}

trident <- trident / nbfichiers

for (vector in matrices_images) {
  vector <- vector - trident
}

C_matrice <- numeric(dimension)
for (i in 1:dimension) {
  for (j in 1:dimension) {
    for (k in 1:dimension) {
      i_prim <- (i + k) %% dimension
      j_prim <- (j + k) %% dimension
      C_matrice[i][j] <- C_matrice[i][j] + matrices_images[i_prim][j_prim] * matrices_images[j_prim][i_prim]
    }
  }
}

