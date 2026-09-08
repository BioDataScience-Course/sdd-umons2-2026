#' ---
#' title: "SDD II module 10 : données ouvertes & SOM"
#' author: "Ph. Grosjean et G. Engels"
#' date: "2025-2026"
#' output:
#'   html_document:
#'     highlight: kate
#' ---
#' 
#' Document complémentaire au [module 10 du cours SDD II de 2025-2026](https://wp.sciviews.org/sdd-umons2-2025/open-data-som.html).
#' Distribué sous licence [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.fr).
#' 
#' **Veuillez vous référer au cours en ligne pour les explications et les interprétations de cette analyse.**
#' 
#' [Installer un environnement R](https://github.com/SciViews/svbox/tree/main/svbox2025-native)
#' adéquat pour reproduire cette analyse. 

#'
#' ### Cartes auto-adaptatives (SOM) {#som}
#'

# Configuration du dialecte SciViews::R avec le module "explore"
SciViews::R("explore", lang = "fr")
# Lecture des données zooplankton
zoo <- read("zooplankton", package = "data.io")
zoo

# Élimination de class, standardisation et transformation en matrice
zoo %>.%
  sselect(., -class) %>.%
  scale(.) %>.%
  as_matrix(.) ->
  zoo_mat

# Charger le package kohonen
library(kohonen)
# Grille rectangulaire 7 x 7
rect_grid_7_7 <- somgrid(7, 7, topo = "rectangular")

# Transformation en un objet de classe kohonen qui est une liste
rect_grid_7_7 %>.%
  structure(list(grid = .), class = "kohonen") %>.% # Objet de classe kohonen
  plot(., type = "property", # Graphique de propriété
    property = unit.distances(rect_grid_7_7)[25, ], # distance à la cellule 25
    main = "Distance depuis la cellule centrale") # Titre du graphique

# Grille hexagonale 7 x 7
hex_grid_7_7 <- somgrid(7, 7, topo = "hexagonal")

# Transformation en un objet de classe kohonen qui est une liste
hex_grid_7_7 %>.%
  structure(list(grid = .), class = "kohonen") %>.% # Objet de classe kohonen
  plot(., type = "property", # Graphique de propriété
    property = unit.distances(hex_grid_7_7)[25, ], # distance à la cellule 25
    main = "Distance depuis la cellule centrale") # Titre du graphique

# Initialisation du générateur de nombres pseudo-aléatoires
set.seed(8657)
# Analyse SOM avec grille hexagonale 7 x 7
zoo_som <- som(zoo_mat, grid = somgrid(7, 7, topo = "hexagonal"))
summary(zoo_som)

# Évolution de l'apprentissage de notre SOM au fil des itérations
plot(zoo_som, type = "changes")

set.seed(954)
# Nouvelle SOM avec 200 itérations
zoo_som <- som(zoo_mat, grid = somgrid(7, 7, topo = "hexagonal"), rlen = 200)
plot(zoo_som, type = "changes")

# Palette de 17 couleurs distinctes
colors17 <- c("#e6194B", "#3cb44b", "#ffe119", "#4363d8", "#f58231", "#911eb4",
  "#42d4f4", "#f032e6", "#bfef45", "#fabebe", "#469990", "#e6beff", "#9A6324",
  "#fffac8", "#800000", "#aaffc3", "#808000")

# Placement des individus dans la carte avec une couleur différente pour chaque classe
plot(zoo_som, type = "mapping", shape = "straight", col = colors17[zoo$class])

# Nombre d'individus dans chaque cellule
plot(zoo_som, type = "counts", shape = "straight")

# Cellule dans laquelle chaque individu est mappé
zoo_som$unit.classif

# Tableau de contingence des individus mappés dans chaque cellule
zoo_som_nb <- table(zoo_som$unit.classif)
zoo_som_nb

# Orientation de la carte SOM en fonction des variables
plot(zoo_som, type = "codes", codeRendering = "segments")

# Coloration de la carte variable par variable (6 variables représentées ici)
par(mfrow = c(2, 3)) # Graphiques sur 2 lignes et 3 colonnes
for (var in c("size", "mode", "range", "aspect", "elongation", "circularity"))
  plot(zoo_som, type = "property", property = zoo_som$codes[[1]][, var],
    main = var, palette.name = viridis::inferno)

#'
#' ### Regroupements {#groups}
#'

# Configuration de R
SciViews::R("explore", lang = "fr")

# Distance euclidienne entre cellules
zoo_som_dist <- dissimilarity(as.data.frame(zoo_som$codes[[1]]),
  method = "euclidean")
# CAH avec distance de Ward D2 et pondération des cellules
# en fonction du nombre d'individus mappés
zoo_som_cah <- cluster(zoo_som_dist, method = "ward.D2", members = zoo_som_nb)

# Dendrogramme et coupure à 10.5
chart(zoo_som_cah) +
  geom_dendroline(h = 10.5, col = "red")

# Prédiction des groupoes à l'aide de la CAH
groupes <- predict(zoo_som_cah, h = 10.5)
groupes

# Visualisation du découpage par CAH sur la carte SOM
plot(zoo_som, type = "mapping", pchs = ".", main = "SOM zoo, six groupes",
  bgcol =  RColorBrewer::brewer.pal(5, "Set2")[groupes])
add.cluster.boundaries(zoo_som, clustering = groupes)
