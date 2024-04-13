
test = readdir("Test")
train = readdir("Train")

library(FactoMineR)
library(factoextra)
library(jpeg)
library(RcmdrMisc)

#dimension = 320*243

setwd(dir="C:/Users/33611/Dossier_codes/DC-S8/Tps-Analyse-de-Donnees/TP5")  #je fixe mon repertoire de travail
getwd() # quel est le chemin par defaut?

dossier <- "yalefaces/"
nbfichiers = length(fichiers)

"""
lireimages: transformer les images dont les noms
            figurent dans "noms_images" en vecteurs
input : - noms_images: liste des noms
output: - data: tableau qui contient tous les
                vecteurs-images
        - dimvisage : tableau contenant les dimensions
                du visage N1 * N2
        - n : nombre entier = nombre des images
"""
function lireimages(noms_images)
dimvisage =[320 243] # dimension du visage souhaitée
n=length(noms_images) # nmbre des images
i=1
data=zeros(n,dimvisage[1]*dimvisage[2]) # initialisation du tableau data
for i = 1:length(noms_images)
im=load("ALL/"*noms_images[i]) # importer l'image n°i
im=convert(Array{Float64},channelview(im)) # convertir l'image en array
data[i,:] = im[:] # im[:] est un vecteur de taille 192*168 = 32256
end
return data::Array{Float64,2},dimvisage::Array{Int},n::Int
end

lireimages(train)
size(lireimages(train)[1])

"""
decomppca: performe la PCA
input : - data: vecteurs-images
output: - U: les vecteurs propres
        - lambda : les valeurs propres
        - psi: le visage moyen
        - cumvar : variance cumulée
"""
function decomppca(data::Array{Float64,2})
data = data' # prendre la transposé car A = transpose(data)
  psi = mean(data,dims = 2) # calculer le visage moyen
  data_hat = data.-psi # centrer les vecteurs-visages
  U,s,_= svd(data_hat) # décomposition en valeurs singulières
  L=length(s)
  lambda = s.^2 # calculer les valeurs propres
  S = sum(lambda)
  cumvar = cumsum(lambda)./S
    
  return U::Array{Float64,2},lambda::Array{Float64,1},psi::Array{Float64,2},cumvar::Array{Float64,1}
end

