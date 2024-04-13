# library(FactoMineR)
# library(factoextra)
# library(jpeg)
# library(RcmdrMisc)

# dimension = 320*243

# setwd(dir="C:/Users/33611/Dossier_codes/DC-S8/Tps-Analyse-de-Donnees/TP5")  #je fixe mon repertoire de travail
# getwd() # quel est le chemin par defaut?

# dossier <- "yalefaces/"
# nbfichiers = length(fichiers)

# fichiers <- list.files(dossier, pattern = "\\.jpg$", full.names = TRUE)
# matrices_images <- vector("list", nbfichiers)
# trident <- numeric(dimension)


# nb <- 1
# for (image in fichiers) {
#   img <- readJPEG(image, TRUE)
  
#   img_vector <- as.vector(img)
  
#   trident <- trident + img_vector
  
#   matrices_images[[nb]] <- img_vector  # Utiliser [[nb]] pour accéder à la position correcte dans la liste
#   nb = nb + 1  # Incrémenter l'index
# }

# trident <- trident / nbfichiers

# for (vector in matrices_images) {
#   vector <- vector - trident
# }

# C_matrice <- matrix(0L, nrow = dimension, ncol = dimension)
# for (i in 1:dimension) {
#   for (j in 1:dimension) {
#     for (k in 1:dimension) {
#       i_prim <- ((i + k) %% dimension)
#       j_prim <- ((j + k) %% dimension)
      
#       i_prim
      
#       C_matrice[i,j] <- C_matrice[i,j] + matrices_images[i_prim,j_prim] * matrices_images[j_prim,i_prim]
#     }
#   }
# }


# Chargement des bibliothèques nécessaires
library(jpeg)
library(ggplot2)

# Chemin vers le dossier contenant les images
dossier <- "yalefaces/"
fichiers <- list.files(dossier, pattern = "\\.jpg$", full.names = TRUE)
nbfichiers <- length(fichiers)

# Dimension d'une image
largeur_image <- 320
hauteur_image <- 243
dimension <- largeur_image * hauteur_image

# Initialisation de la matrice pour stocker les images
matrices_images <- vector("list", nbfichiers)
moyenne_images <- numeric(dimension)

# Boucle pour charger et traiter chaque image
for (nb in 1:nbfichiers) {
  # Lecture de l'image
  img <- readJPEG(fichiers[nb], TRUE)
  
  if (is.null(img)) {
    # Si la lecture de l'image a échoué, afficher un message d'erreur
    cat("Erreur lors de la lecture de l'image :", fichiers[nb], "\n")
  } else {
    # Conversion de l'image en vecteur
    img_vector <- as.vector(img)
  
    # Ajout de l'image à la moyenne
    moyenne_images <- moyenne_images + img_vector
  
    # Stockage de l'image dans la liste matrices_images
    matrices_images[[nb]] <- img_vector
  }
}

# Calcul de la moyenne des images
if (nbfichiers > 0) {
  moyenne_images <- moyenne_images / nbfichiers

  # Centrage des images par rapport à la moyenne
  for (nb in 1:nbfichiers) {
    if (!is.null(matrices_images[[nb]])) {
      matrices_images[[nb]] <- matrices_images[[nb]] - moyenne_images
    }
  }

  # Conversion des données en une matrice
  data_matrix <- matrix(unlist(matrices_images), nrow = dimension, byrow = TRUE)

  # Normalisation des données
  data_matrix <- scale(data_matrix)

  # Calcul de la matrice de covariance
  C_matrice <- cov(data_matrix)

  # Affichage de la matrice des variances-covariances
  print("Matrice des variances-covariances :")
  print(C_matrice)
} else {
  cat("Aucune image valide trouvée dans le dossier.\n")
}


# Calcul des valeurs propres et vecteurs propres
eig <- eigen(C_matrice)

# Choix du nombre de composantes principales
PVE <- eig$values / sum(eig$values) # D'apres la formule : PVE = lambda / sum(lambda)
k <- which.max(cumsum(PVE) >= 0.95)[1] # On choisit k tel que la variance cumulée soit supérieure à 95%
cat("Nombre de composantes principales choisies pour qu'on ait 95% de la variance : ", k, "\n")

# Sélection des k vecteurs propres correspondant aux plus grandes valeurs propres
k_vecteurs_propres <- eig$vectors[, 1:k]

# Projection des données sur les k vecteurs propres
projection<-data_matrix %*% k_vecteurs_propres

# Visualisation : Projection des visages sur les vecteurs propres
par(mfrow = c(2, 3))
for (i in 1:5) {
  # Projeter les visages sur les vecteurs propres.
  image(matrix(t(k_vecteurs_propres) %*% projection[, i] + moyenne_images, ncol = largeur_image), col = gray.colors(256), xaxt = "n", yaxt = "n", main = paste("Projection", i))
}

# Reconstruction des visages appris dans la base réduite
reconstruction <- k_vecteurs_propres %*% projection 

# Visualisation : Projection des visages sur les vecteurs propres
par(mfrow = c(2, 3))
for (i in 1:5) {
  image(matrix(reconstruction[, i] + moyenne_images, ncol = largeur_image), col = gray.colors(256), xaxt = "n", yaxt = "n", main = paste("Reconstruction", i))
}
