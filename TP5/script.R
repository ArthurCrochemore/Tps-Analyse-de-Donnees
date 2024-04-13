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


# Charger le package jpeg
library(jpeg)
library(ggplot2)

# Spécifier le chemin vers le dossier contenant les images
chemin_dossier <- "yalefaces/"

# Lire toutes les images du dossier
images <- list.files(chemin_dossier, full.names = TRUE)

# Définir la dimension d'une image
largeur_image <- 320
hauteur_image <- 243
dimension <- largeur_image * hauteur_image

# Initialiser la liste pour stocker les images
matrices_images <- vector("list", length(images))
moyenne_images <- numeric(dimension)

# Charger et traiter chaque image
for (i in 1:length(images)) {
  # Lecture de l'image
  img <- readJPEG(images[i], native = TRUE)
  
  # Vérifier si l'image a été chargée correctement
  if (!is.null(img)) {
    # Conversion de l'image en vecteur
    img_vector <- as.vector(img)
    
    # Ajout de l'image à la moyenne
    moyenne_images <- moyenne_images + img_vector
    
    # Stockage de l'image dans la liste matrices_images
    matrices_images[[i]] <- img_vector
  } else {
    cat("Erreur lors de la lecture de l'image :", images[i], "\n")
  }
}

# Calcul de la moyenne des images
moyenne_images <- moyenne_images / length(images)

# Centrage des images par rapport à la moyenne
for (i in 1:length(images)) {
  if (!is.null(matrices_images[[i]])) {
    matrices_images[[i]] <- matrices_images[[i]] - moyenne_images
  }
}

# Conversion des données en une matrice
data_matrix <- matrix(unlist(matrices_images), nrow = dimension, byrow = TRUE)

# Normalisation des données
data_matrix <- scale(data_matrix)

# Calcul de la matrice de covariance
C_matrice <- cov(data_matrix)

# Calcul des valeurs propres et vecteurs propres
eig <- eigen(C_matrice)

# Choix du nombre de composantes principales
PVE <- eig$values / sum(eig$values)
k <- which.max(cumsum(PVE) >= 0.95)[1]  # Choix pour expliquer au moins 95% de la variance
cat("Nombre de composantes principales choisies pour expliquer 95% de la variance :", k, "\n")

# Sélection des k vecteurs propres correspondant aux plus grandes valeurs propres
k_vecteurs_propres <- eig$vectors[, 1:k]

# Projection des données sur les k vecteurs propres
projection <- t(k_vecteurs_propres) %*% t(data_matrix)

# Convertir la projection en data frame pour ggplot2
projection_df <- data.frame(PC1 = projection[1, ], PC2 = projection[2, ])

# Afficher les données des visages dans l'espace des vecteurs propres avec ggplot2
ggplot(projection_df, aes(x = PC1, y = PC2)) +
  geom_point() +
  labs(x = "Composante Principale 1", y = "Composante Principale 2") +
  theme_minimal()

