#' ---
#' title: "SDD II module 7 : ACP & AFC"
#' author: "Ph. Grosjean et G. Engels"
#' date: "2025-2026"
#' output:
#'   html_document:
#'     highlight: kate
#' ---
#' 
#' Document complémentaire au [module 7 du cours SDD II de 2025-2026](https://wp.sciviews.org/sdd-umons2-2025/acp-afc.html).
#' Distribué sous licence [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.fr).
#' 
#' **Veuillez vous référer au cours en ligne pour les explications et les interprétations de cette analyse.**
#' 
#' [Installer un environnement R](https://github.com/SciViews/svbox/tree/main/svbox2025-native)
#' adéquat pour reproduire cette analyse. 

#'
#' ### ACP sur les indiens diabétiques {#acppima}
#'

# Chargement du dialecte SciViews::R avec le module d'exploration des données
SciViews::R("explore", lang = "fr")

# Lecture du jeu de données depuis le package mlbench
pima <- read("PimaIndiansDiabetes2", package = "mlbench")
pima

# Visualisation des données manquantes
naniar::vis_miss(pima)

# Elimination des lignes contenant des valeurs manquantes
pima <- sdrop_na(pima)
pima

# Description générale des données
skimr::skim(pima)

# Calcul de la matrice de corrélation, tableau formaté et graphique
pima_cor <- correlation(pima[, 2:8])
tabularise(pima_cor, digits = 2)
plot(pima_cor)

# ACP sur pima avec standardisation des données
pima_pca <- pca(data = pima, ~ glucose + pressure + triceps + insulin + mass +
  pedigree + age, scale = TRUE)

# Idem, mais avec élimination des colonnes non utilisées auparavant
pima %>.%
  sselect(., glucose:age) %>.%
  pca(., scale = TRUE) %->%
  pima_pca

# Résumé de notre ACP
summary(pima_pca)

# Graphique des éboulis de notre ACP
chart$scree(pima_pca, fill = "cornsilk")

# Autre version du graphique des éboulis
chart$altscree(pima_pca)

# Graphique de l'espace des variables pour les deux premiers axes de l'ACP
chart$loadings(pima_pca, choices = c(1, 2))

# Graphique de l'espace des variables pour les axes PC1 et PC3
chart$loadings(pima_pca, choices = c(1, 3))

# Graphique de l'espace des variables pour les axes PC2 et PC3
chart$loadings(pima_pca, choices = c(2, 3))

# Graphique de l'espace des individus pour les deux premeirs axes de l'ACP
chart$scores(pima_pca, choices = c(1, 2), aspect.ratio = 3/5)

# Idem, mais avec labels et ellipses
chart$scores(pima_pca, choices = c(1, 2),
  labels = pima$diabetes) +
  stat_ellipse()

# Graphique de l'espace des individus pour PC1 et PC3 + labels et ellipses
chart$scores(pima_pca, choices = c(1, 3),
  labels = pima$diabetes) +
  stat_ellipse()

# Graphique de l'espace des individus pour PC2 et PC3 + labels et ellipses
chart$scores(pima_pca, choices = c(2, 3),
  labels = pima$diabetes) +
  stat_ellipse()

# Graphique biplot de l'ACP
chart$biplot(pima_pca)

#'
#' ### ACP sur la biométrie d'oursins {#acpurchins}
#'

# Récupération des données depuis le package data.io
urchin <- read("urchin_bio", package = "data.io", lang = "FR")
urchin

# Visualisation des données manquantes
naniar::vis_miss(urchin)

# Elimination des variables inutilisées et des données manquantes
urchin %>.%
  sselect(., -(skeleton:spines), -sex) %>.%
  sdrop_na(.) ->
  urchin2
urchin2

# Description générale des données
skimr::skim(urchin2)

# Matrice de corrélation, tableau formaté et graphique
urchin2_cor <- correlation(urchin2[, 2:13])
tabularise(urchin2_cor)
# ou knitr::kable(urchin2_cor, digits = 2)
plot(urchin2_cor)

# Graphique de la masse totle en fonction du diamètre 1
chart(data = urchin2, weight ~ diameter1) +
  geom_point()

# Graphique de la masse totle en fonction du diamètre 1, avec double log
chart(data = urchin2, log(weight) ~ log(diameter1)) +
  geom_point()

# ACP sur données transformées log(x + 1) et standardisation
urchin2 %>.%
  sselect(., -origin, -maturity) %>.% # Élimine les variables non quantitatives
  log1p(.) %>.% # Transforme toutes les autres en log(x + 1)
  pca(data = ., ~., scale = TRUE) -> # Effectue l'ACP après standardisation
  urchin2_pca

# Résumé de l'ACP
summary(urchin2_pca)

# Graphique des éboulis de l'ACP
chart$scree(urchin2_pca)

# Graphique de l'espace des variables de l'ACP pour les deux premiers axes (par défaut)
chart$loadings(urchin2_pca)

# Nouvelle ACP sur données divisées par la masse immergée
urchin2 %>.%
  sselect(., -origin, -maturity, -buoyant_weight) %>.% # Élimination des variables inutiles
  (. / urchin2$buoyant_weight) %>.% # Division par buoyant_weight
  log1p(.) -> # Transformation log(x + 1)
  urchin3
head(urchin3)

# ACP avec standardisation
urchin3_pca <- pca(data = urchin3, ~., scale = TRUE)
# Résumé de l'ACP
summary(urchin3_pca)

# Graphique des éboulis de l'ACP
chart$scree(urchin3_pca)

# Graphique de l'espace des variables de l'ACP pour les deux premiers axes
chart$loadings(urchin3_pca, choices = c(1, 2))

# Graphique de l'espace des variables de l'ACP pour les PC2 et PC3
chart$loadings(urchin3_pca, choices = c(2, 3))

# Graphique de l'espace des individus pour PC1 et PC2 avec couleur et ellipses
chart$scores(urchin3_pca, choices = c(1, 2),
  col = urchin2$origin, labels = urchin2$maturity, aspect.ratio = 3/5) +
  theme(legend.position = "right") +
  stat_ellipse()

# Graphique de l'espace des individus pour PC2 et PC3 avec couleur et ellipses
chart$scores(urchin3_pca, choices = c(2, 3),
  col = urchin2$origin, labels = urchin2$maturity, aspect.ratio = 3/5) +
  theme(legend.position = "right") +
  stat_ellipse()

#'
#' ### Visualisation de données quantitatives {#visuquant}
#'

# Visu 2D : graphique de glucose en fonction d'insuline, couleur par diabète
chart(data = pima, glucose ~ insulin %col=% diabetes) +
  geom_point()

# Préparation du Quarto/R Markdown pour widget RGL
options(rgl.printRglwidget = TRUE)
library(rgl)
# Need this too now? library(rglwidget)
knitr::knit_hooks$set(webgl = hook_webgl)

# Visu 3D : nuage de points en 3 dimensions avec RGL
rgl::plot3d(pima$insulin, pima$glucose, pima$triceps,
  col = as.integer(pima$diabetes))

# Visu >3D : matrice de nuages de points
GGally::ggscatmat(pima, 2:6, color = "diabetes")

#'
#' ### AFC enquête sur la science {#afcscience}
#'

# Lecture des données depuis le package ca
wg <- read("wg93", package = "ca")
# Tableau présentant les premières et dernières données de wg
tabularise$headtail(wg)

# Recodage des niveaux des variables de wg
wg %>.%
  smutate(.,
    A = recode(A, `1` = "++", `2` = "+", `3` = "0", `4` = "-", `5` = "--"),
    B = recode(B, `1` = "++", `2` = "+", `3` = "0", `4` = "-", `5` = "--"),
    C = recode(C, `1` = "++", `2` = "+", `3` = "0", `4` = "-", `5` = "--"),
    D = recode(D, `1` = "++", `2` = "+", `3` = "0", `4` = "-", `5` = "--"),
    sex = recode(sex, `1` = "H", `2` = "F"),
    age = recode(age, `1` = "18-24", `2` = "25-34", `3` = "35-44",
      `4` = "45-54", `5` = "55-64", `6` = "65+"),
    edu = recode(edu, `1` = "primaire", `2` = "sec. part", `3` = "secondaire",
      `4` = "univ. part", `5` = "univ. cycle 1", `6` = "univ. cycle 2")
  ) -> wg
# Tableau présentant les premières et dernières données de wg réencodé
tabularise$headtail(wg)

# Tableau de contingence de question B en fonction de edu(cation)
table(B = wg$B, edu = wg$edu)

# Test de Chi2 question B versus edu(cation)
chisq.test(wg$B, wg$edu)

# AFC question B versus edu(cation) du jeu de données wg
wg_b_edu <- ca(data = wg, ~ B + edu)
wg_b_edu

# Résumé de l'AFC
summary(wg_b_edu, scree = TRUE, rows = FALSE, columns = FALSE)

# Graphique des éboulis de l'AFC
chart$scree(wg_b_edu)

# Graphique biplot de l'AFC : question B en turquoise, edu(cation) en rouge
chart$biplot(wg_b_edu, choices = c(1, 2))

#'
#' ### AFC sur une communauté d'acariens {#afcacariens}
#'

# Lecture des données depuis le package vegan (mite en anglais = acarien)
mite <- read("mite", package = "vegan")
# Description générale des données
skimr::skim(mite)

# Somme des lignes et des colonnes du tableau mite
rowSums(mite)
colSums(mite)

# Boites à moustaches parallèles pour mite
mite %>.%
  spivot_longer(., everything(), names_to = "species", values_to = "n") %>.%
  chart(., species ~ n) +
    geom_boxplot() +
  labs(x = "Espèces", y = "Observations")

# Transformation log(x + 1) pour mite
mite2 <- log1p(as_dtf(mite)) # Utilisons un data.frame pour les noms des lignes
# Ajouter le numéro des stations explicitement comme nom des lignes
rownames(mite2) <- 1:nrow(mite2)

# Boites à moustaches parallèles pour mite transformé log(x + 1)
mite2 %>.%
  spivot_longer(., everything(), names_to = "species", values_to = "log_n_1") %>.%
  chart(., species ~ log_n_1) +
  geom_boxplot() +
  labs(x = "Espèces", y = "Logarithme des observations")

# Autre visualisation des données tranformées log(x + 1) de mite
mite2 %>.%
  smutate(., station = 1:nrow(mite2)) %>.%
  spivot_longer(., Brachy:Trimalc2, names_to = "species", values_to = "n") %>.%
  chart(., species ~ station %fill=% n) +
  geom_raster()

# AFC sur données transformées log(x + 1) de mite
mite2_ca <- ca(mite2)

# Résumé de l'AFC
summary(mite2_ca, scree = TRUE, rows = FALSE, columns = FALSE)

# Graphique des éboulis de notre AFC
chart$scree(mite2_ca, fill = "cornsilk")

# Biplot de l'AFC : en turquoise les station, et en rouge, les espèces d'acariens
chart$biplot(mite2_ca, choices = c(1, 2))

# Même biplot, mais avec labels plus lisibles grâce à l'option repel = TRUE
chart$biplot(mite2_ca, choices = c(1, 2), repel = TRUE)
