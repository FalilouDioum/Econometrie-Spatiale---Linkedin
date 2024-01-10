
# Installer et charger le package tibble si nécessaire
if (!require(tibble)) install.packages("tibble")
library(tibble)

# Création d'un jeu de données fictif
vendeurs_dakar <- tibble(
  ID_Vendeur = 1:1000,
  Annees_Experience = sample(1:10, 1000, replace = TRUE),
  Nb_Clients_Jour = sample(20:50, 1000, replace = TRUE),
  Latitude = runif(1000, min = 14.69, max = 14.77),
  Longitude = runif(1000, min = -17.45, max = -17.36)
)


# Création de la variable chiffre d'affaire
if (!require(tibble)) install.packages("dplyr")
library(dplyr)
vendeurs_dakar <- vendeurs_dakar %>%
  mutate(CA_FCFA = 10000000 * (Latitude - min(Latitude)) + 8000000 * (Longitude - min(Longitude)) + rnorm(1000, mean = , sd = 200000))

if (!require(lmtest)) install.packages("lmtest")
library(lmtest)

# Régression linéaire multiple
modele_regr <- lm(CA_FCFA ~ Annees_Experience +  Nb_Clients_Jour, data = vendeurs_dakar)

# Afficher le résumé du modèle
summary(modele_regr)

# Installer et charger les packages nécessaires
if (!require(spdep)) install.packages("spdep")
if (!require(sf)) install.packages("sf")
library(spdep)
library(sf)

# Création de l'objet sf à partir du jeu de données
vendeurs_sf <- st_as_sf(vendeurs_dakar, coords = c("Longitude", "Latitude"), crs = 4326)

# Calculer la matrice de poids spatiaux (par exemple, en utilisant des voisins les plus proches)
poids <- knn2nb(knearneigh(st_coordinates(vendeurs_sf), k = 5))
listw <- nb2listw(poids)

# Calculer l'indice de Moran pour le CA
moran_test <- moran.test(vendeurs_dakar$CA_FCFA, listw)
moran_plot <- moran.plot(vendeurs_dakar$CA_FCFA, listw, 
                         labels = as.character(vendeurs_dakar$ID_Vendeur), 
                         pch = 20, col = "blue", xlab = "CA_FCFA", ylab = "Lag de CA_FCFA")


if (!require(spatialreg)) install.packages("spatialreg")
library(spatialreg)

# Modèle Spatial Lag
sar_model <- lagsarlm(CA_FCFA ~ Annees_Experience + Nb_Clients_Jour, data = vendeurs_dakar, listw = listw)
sem_model <- errorsarlm(CA_FCFA ~ Annees_Experience + Nb_Clients_Jour, data = vendeurs_dakar, listw = listw)

