library(jpeg)
library(ggplot2)

# Récupération des chemins vers les images
chemin_dossier <- "yalefaces/"
images <- list.files(chemin_dossier, full.names = TRUE)

# Saisie des dimensions
largeur_image <- 320
hauteur_image <- 243
dimension <- largeur_image * hauteur_image

matrices_images <- vector("list", length(images))
moyenne_images <- numeric(dimension)

# Ouverture des image 1 à 1 et stockage dans la matrice matrices_images
for (i in 1:length(images)) {
  img <- readJPEG(images[i], native = TRUE)
  
  if (!is.null(img)) {
    # Conversion de l'image en vecteur
    img_vector <- as.vector(img)
    moyenne_images <- moyenne_images + img_vector
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
C_matrice

# Calcul des valeurs propres et vecteurs propres
eig <- eigen(C_matrice)
valeurs_propres_triees <- round(sort(100 * eig[["values"]] / sum(eig[["values"]]), decreasing = TRUE), 2)
valeurs_propres_triees
sum(valeurs_propres_triees[1:5])
# On prend 5 valeurs propres, ce qui permet d'avoir 82% d'informations sans avoir trop de dimensions
k <- 5
k_vecteurs_propres <- eig$vectors[, 1:k]

# Projection des données sur les k vecteurs propres
projection <- t(k_vecteurs_propres) %*% t(data_matrix)

# Affichage des projections avec ggplot2
projection_df <- data.frame(PC1 = projection[1, ], PC2 = projection[2, ], PC3 = projection[3, ], PC4 = projection[4, ], PC5 = projection[5, ])
ggplot(projection_df, aes(x = PC1, y = PC2, fill=PC3, color=PC4, size=PC5^2)) +
  geom_point() +
  labs(x = "Composante Principale 1", y = "Composante Principale 2") +
  geom_point(shape=21) +
  scale_color_gradient(low="red", high="green") +
  scale_size_continuous(range=c(1,12)) +
  theme_minimal()

# Reconstruction et affichage des visages 10 par 10
par(mfrow = c(2, 5))
for (i in 1:10) {
  img_matrix <- matrix(matrices_images[[i]], nrow = hauteur_image, byrow = TRUE)
  image(img_matrix, col = gray.colors(256), xaxt = "n", yaxt = "n", main = paste("Reconstruction", i))
}
for (i in 11:20) {
  img_matrix <- matrix(matrices_images[[i]], nrow = hauteur_image, byrow = TRUE)
  image(img_matrix, col = gray.colors(256), xaxt = "n", yaxt = "n", main = paste("Reconstruction", i))
}
for (i in 21:30) {
  img_matrix <- matrix(matrices_images[[i]], nrow = hauteur_image, byrow = TRUE)
  image(img_matrix, col = gray.colors(256), xaxt = "n", yaxt = "n", main = paste("Reconstruction", i))
}
for (i in 31:40) {
  img_matrix <- matrix(matrices_images[[i]], nrow = hauteur_image, byrow = TRUE)
  image(img_matrix, col = gray.colors(256), xaxt = "n", yaxt = "n", main = paste("Reconstruction", i))
}
for (i in 41:50) {
  img_matrix <- matrix(matrices_images[[i]], nrow = hauteur_image, byrow = TRUE)
  image(img_matrix, col = gray.colors(256), xaxt = "n", yaxt = "n", main = paste("Reconstruction", i))
}
for (i in 51:60) {
  img_matrix <- matrix(matrices_images[[i]], nrow = hauteur_image, byrow = TRUE)
  image(img_matrix, col = gray.colors(256), xaxt = "n", yaxt = "n", main = paste("Reconstruction", i))
}
for (i in 61:70) {
  img_matrix <- matrix(matrices_images[[i]], nrow = hauteur_image, byrow = TRUE)
  image(img_matrix, col = gray.colors(256), xaxt = "n", yaxt = "n", main = paste("Reconstruction", i))
}
for (i in 71:80) {
  img_matrix <- matrix(matrices_images[[i]], nrow = hauteur_image, byrow = TRUE)
  image(img_matrix, col = gray.colors(256), xaxt = "n", yaxt = "n", main = paste("Reconstruction", i))
}
for (i in 81:90) {
  img_matrix <- matrix(matrices_images[[i]], nrow = hauteur_image, byrow = TRUE)
  image(img_matrix, col = gray.colors(256), xaxt = "n", yaxt = "n", main = paste("Reconstruction", i))
}
for (i in 91:100) {
  img_matrix <- matrix(matrices_images[[i]], nrow = hauteur_image, byrow = TRUE)
  image(img_matrix, col = gray.colors(256), xaxt = "n", yaxt = "n", main = paste("Reconstruction", i))
}
for (i in 101:110) {
  img_matrix <- matrix(matrices_images[[i]], nrow = hauteur_image, byrow = TRUE)
  image(img_matrix, col = gray.colors(256), xaxt = "n", yaxt = "n", main = paste("Reconstruction", i))
}
for (i in 111:120) {
  img_matrix <- matrix(matrices_images[[i]], nrow = hauteur_image, byrow = TRUE)
  image(img_matrix, col = gray.colors(256), xaxt = "n", yaxt = "n", main = paste("Reconstruction", i))
}
for (i in 121:130) {
  img_matrix <- matrix(matrices_images[[i]], nrow = hauteur_image, byrow = TRUE)
  image(img_matrix, col = gray.colors(256), xaxt = "n", yaxt = "n", main = paste("Reconstruction", i))
}
for (i in 131:140) {
  img_matrix <- matrix(matrices_images[[i]], nrow = hauteur_image, byrow = TRUE)
  image(img_matrix, col = gray.colors(256), xaxt = "n", yaxt = "n", main = paste("Reconstruction", i))
}
for (i in 141:150) {
  img_matrix <- matrix(matrices_images[[i]], nrow = hauteur_image, byrow = TRUE)
  image(img_matrix, col = gray.colors(256), xaxt = "n", yaxt = "n", main = paste("Reconstruction", i))
}



