#' ---
#' title: "SDD II module 6 : Classification & indices"
#' author: "Ph. Grosjean et G. Engels"
#' date: "2025-2026"
#' output:
#'   html_document:
#'     highlight: kate
#' ---
#' 
#' Document complémentaire au [module 6 du cours SDD II de 2025-2026](https://wp.sciviews.org/sdd-umons2-2025/cah-kmeans-div.html).
#' Distribué sous licence [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.fr).
#' 
#' **Veuillez vous référer au cours en ligne pour les explications et les interprétations de cette analyse.**
#' 
#' [Installer un environnement R](https://github.com/SciViews/svbox/tree/main/svbox2025-native)
#' adéquat pour reproduire cette analyse. 

#'
#' ### Indices et matrices de distances {#dist-mat}
#'

#Configure SciViews::R for multivariate data exploration
SciViews::R("explore")

# Read the zooplankton dataset
zoo <- read("zooplankton", package = "data.io")
zoo

# Sous-ensemble des 6 premiers individus de zoo dans zoo6
zoo %>.%
  sselect(., -class) %>.% # Élimination de la colonne class
  head(., n = 6) ->
  zoo6 # Récupération des 6 premiers individus
# Calcul de la matrice de distance euclidienne
zoo6_dist <- dissimilarity(zoo6, method = "euclidean")
zoo6_dist

#'
#' ### Regroupement avec CAH {#group-cah}
#'

# Sous-enseble de zoo (individus 13 à 18)
zoo %>.%
  select(., -class) %>.%   # Élimination de la colonne class
  slice(., 13:18) ->       # Récupération des lignes 13 à 18
  zoo6

# Matrice de dissimilarité sur données standardisées
zoo6 %>.%
  dissimilarity(., method = "euclidean", scale = TRUE) ->
  zoo6std_dist

# Dendrogramme
zoo6std_dist %>.%
  cluster(.) ->
  zoo6std_clust # Calcul du dendrogramme
  chart(zoo6std_clust) +
    ylab("Hauteur")

# Classes des individus 13 à 18
zoo$class[13:18]

# Coupure du dendrogramme à 8 (2 groupes)
chart(zoo6std_clust) +
  geom_dendroline(h = 8, color = "red")

# Coupure du dendrogramme à 5,8 (3 groupes)
chart(zoo6std_clust) +
  geom_dendroline(h = 5.8, color = "red")

# Détail sur les noeuds du dendrogramme
str(zoo6std_clust)

# Deux groupes
chart(zoo6std_clust) +
  geom_dendroline(h = 7.5, color = "red")

# Extraction des deux groupes
(group2 <- predict(zoo6std_clust, h = 7.5))

# Quatre groupes
chart(zoo6std_clust) +
  geom_dendroline(h = 5, color = "red")

# Extraction des quatre groupes
(group4 <- predict(zoo6std_clust, h = 5))

# Ajout du regroupement dans zoo6
zoo6g <- augment(data = zoo6, zoo6std_clust, h = 7.5)
names(zoo6g) # Nom des variables dans ce tableau
# Nous transformons ces groupes en variable facteur
zoo6g$group <- factor(zoo6g$.fitted)
# Graphique des deux variables en utilisant la couleur en fonction des groupes CAH
chart(data = zoo6g, area ~ circularity %col=% group) +
  geom_point()

# Dendrogramme avec liens simples
zoo6std_dist %>.%
  cluster(., method = "single") %>.%
  chart(.)

# Dendrogramme avec liens moyens
zoo6std_dist %>.%
  cluster(., method = "average") %>.%
  chart(.)

# Dendrogramme avec liens Ward D2
zoo6std_dist %>.%
  cluster(., method = "ward.D2") %>.%
  chart(.)

# Dendrogramme avec liens centroïdes
zoo6std_dist %>.%
  cluster(., method = "centroid") %>.%
  chart(.)

# Dendrogramme sur le jeu de données complet zoo
zoo %>.%
  sselect(., -class) %>.% # Élimination de la colonne class
  # Matrice de dissimilarité sur données standardisées
  dissimilarity(., method = "euclidean", scale = TRUE) %>.%
  # CAH Ward D2
  cluster(., method = "ward.D2") -> # CAH avec Ward D2
  zoo_clust

# Dendrogramme horizontal et sans labels (plus lisible si beaucoup d'items)
chart$horizontal(zoo_clust, labels = FALSE) +
  geom_dendroline(h = 70, color = "red") + # Séparation en 3 groupes
  ylab("Hauteur")

# Ajout des groupes dans zoo
augment(data = zoo, zoo_clust, h = 70) %>.%
  smutate(., group = as.factor(.fitted)) ->
  zoog

# Nuage de points avec mise en évidence des groupes par la couleur
chart(data = zoog, compactness ~ ecd %col=% group) +
  geom_point() +
  coord_trans(x = "log10", y = "log10") # Axes en log10

# Tableau de contingence entre classes et groupes CAH
table(classe = zoog$class, cah = zoog$group)

#'
#' ### K-moyennes {#kmeans}
#'

# Dialect SciViews::R avec l'extension d'exploration de données multivariées
SciViews::R("explore")

# Jeu de données zooplankton
zoo <- read("zooplankton", package = "data.io")
zoo

# Initialisation du générateur de nombres psuedo-aléatoires
set.seed(38)

# Individus 13 à 18 uniquement
zoo %>.%
  select(., -class) %>.% # Élimination de la colonne class
  slice(., 13:18) ->   # Récupération des lignes 13 à 18
  zoo6

# K-moyennes, ne pas oublier de standardiser avec scale()
zoo6_kmn <- k_means(scale(zoo6), k = 2)
zoo6_kmn

# Taille des groupes K-moyennes
zoo6_kmn$size

# Information sur la classification K-moyennes
glance(zoo6_kmn)

# Profil de la somme des carrés intragroupes en fonction de k
profile_k(scale(zoo6)) # ou zoo6_kmn$data

# Récupération des groupes K-moyennes dans le jeu de données
augment(zoo6_kmn, zoo6) %>.%
  srename(., cluster = .cluster) ->
  zoo6b
names(zoo6b)

# Contenu de la variable cluster
zoo6b$cluster

# Variable factor
class(zoo6b$cluster)

# Centres des groupes K-moyennes
zoo6_centers <- tidy(zoo6_kmn, col.names = names(zoo6))
zoo6_centers

# Graphique des K-moyennes
kmn_chart <- chart(zoo6_kmn, choices = c("circularity", "area"),
  alpha = 0.8, c.size = 5, c.shape = 17)
kmn_chart

# Meilleurs labels des axes
kmn_chart +
  labs(x = "Circularité standardisée", y = "Aire standardisée")

# Données standardisées + classe pour les individus de 13 à 18 de zoo
zoo6std <- scale(zoo6)
zoo6std$class <- zoo$class[13:18] 
# Graphique annoté des classes
kmn_chart +
  labs(x = "Circularité standardisée", y = "Aire standardisée") +
  ggrepel::geom_text_repel(data = zoo6std, aes(label = class))

# Initialisation du générateur pseudo-aléatoire
set.seed(9768)

# K-moyennes
k_means(zoo6, k = 2, nstart = 50) # 50 positions de départ différentes

# Profil K sur le jeu entier zoo
zoo %>.%
  sselect(., -class) %>.%
  scale(.) %>.%
  profile_k(., k.max = 15)

# Initialisation pseudo-aléatoire
set.seed(562)
# K-moyennes sur jeu complet standardisé
sselect(zoo, -class) %>.%
  scale(.) %>.%
  k_means(., k = 3, nstart = 50) ->
  zoo_kmn
zoo_kmn

# Récupération des groupes K-moyennes dans le jeu de données zoo
augment(zoo_kmn, zoo) %>.%
  srename(., cluster = .cluster) ->
  zoob

# Graphique K-moyennes ECD vs Compacité
chart(zoo_kmn, choices = c("ecd", "compactness"), alpha = 0.2) +
  labs(x = "ECD standardisée", y = "Compacité standardisée") +
  stat_ellipse()

# Table de con,tingence entre classes et groupes K-moyennes
table(zoob$class, zoob$cluster)

#'
#' ### Indices de diversité {#diversity}
#'

# Dialecte SciViews::R avec module d'exploration multivariée
SciViews::R("explore")

# Jeu de données BCI
bci <- read("BCI", package = "vegan")

# Sous-échantillon de 5 parcelles
set.seed(2003)
bci_sub <- sample_n(bci, 5) 

# Exploration partielle des données (15 premières espèces)
skimr::skim(bci_sub[, 1:15])

# Calcul de la Richesse spécifique pour chacune des parcelles
vegan::specnumber(bci_sub)

# Indice de Shannon
bci_sub_h <- vegan::diversity(bci_sub)
bci_sub_h

# Richesse spécifique S
bci_sub_s <- vegan::specnumber(bci_sub)
bci_sub_s
# Indice d'équitabilité de Piélou J 
bci_sub_j <- bci_sub_h / ln(bci_sub_s)
bci_sub_j  

# Indice de diversité de Simpson
bci_sub_e <- vegan::diversity(bci_sub, index = "simpson")
bci_sub_e

# Indice de diversité de Jaccard
dissimilarity(bci_sub, method = "jaccard", binary = TRUE)

if (exists("assignment2"))
  assignment2("B06Ga_open_data", part = "I",
    url = "https://github.com/BioDataScience-Course/B06Ga_open_data",
    course.ids = c(
      'S-BIOG-061' = !"B06Ga_{YY}M_open_data"),
    course.urls = c(
      'S-BIOG-061' = !"{assign_url$B06Ga_open_data}"),
    course.starts = c(
      'S-BIOG-061' = !"{class2_start(mod, 'B06')}"),
    course.ends = c(
      'S-BIOG-061' = !"{n4_end(mod, 'B10')}"),
    term = "Q2", level = 4, n = 4,
    toc = "Étude de données ouvertes choisies librement (I)")
