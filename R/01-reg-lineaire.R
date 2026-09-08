#' ---
#' title: "SDD II module 1 : Régression linéaire I"
#' author: "Ph. Grosjean et G. Engels"
#' date: "2025-2026"
#' output:
#'   html_document:
#'     highlight: kate
#' ---
#' 
#' Document complémentaire au [module 1 du cours SDD II de 2025-2026](https://wp.sciviews.org/sdd-umons2-2025/lm.html).
#' Distribué sous licence [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.fr).
#' 
#' **Veuillez vous référer au cours en ligne pour les explications et les interprétations de cette analyse.**
#' 
#' [Installer un environnement R](https://github.com/SciViews/svbox/tree/main/svbox2025-native)
#' adéquat pour reproduire cette analyse. 

# Dialecte SciViews::R avec la section dédiée à la modélisation
SciViews::R("model", lang = "fr")

# Lecture du jeu de données d'exemple
trees <- read("trees", package = "datasets")

# Matrice de corrélation entre variables numériques du jeu de données
(trees_corr <- correlation(trees)) |>
  tabularise()

# Graphique de la matrice de corrélation
plot(trees_corr, type = "lower")

# Matrice de nuages de points pour visualiser les associations entre variables
GGally::ggscatmat(trees, 1:3)

# Graphique en nuage de points de la relation volume ~ diamètre
chart(data = trees, volume ~ diameter) +
  geom_point()

# Régression linéaire
trees_lm <- lm(data = trees, volume ~ diameter)

# Graphique de nos observations et de la droite obtenue avec la fonction lm()
# accompagnée d'une enveloppe de confiance à 95%
chart(trees_lm)

# Résumé condensé sous forme textuelle de notre modèle
summary_(trees_lm)

# Dispersion des résidus résumée par les cinq nombres
fivenum(residuals(trees_lm))

# Estimation des paramètres de notre modèle
coef(trees_lm)

# Résumé du modèle formaté à l'aide de tabularise()
summary_(trees_lm) |>
  tabularise(warn = FALSE)

# Graphique des résidus en fonction des valeurs prédites pour Y
chart$resfitted(trees_lm)

# Graphique quantile-quantile des résidus
chart$qqplot(trees_lm)

# Graphique des résidus servant à vérifier l'homoscédasticité
chart$scalelocation(trees_lm)

# Graphique de la distance de Cook des résidus
chart$cooksd(trees_lm)

# Graphique de l'effet de levier des résidus
chart$resleverage(trees_lm)

# Graphique de l'effet de levier versus distance de Cook
chart$cookleverage(trees_lm)

# Figure composite avec les 4 graphiques d'analyse des résidus principaux
chart$residuals(trees_lm)

# Nouveau jeu de données pouvant servir à des prédictions
new_trees <- dtx(diameter = seq(0.1, 0.7, length.out = 8))

# Nouvelles prédictions à l'aide de notre modèle
new_trees <- add_predictions(new_trees, trees_lm)
new_trees

# Ajout des prédictions au jeu de données initial
trees <- add_predictions(trees, trees_lm)

# Graphique avec les prédictions
chart(data = trees, volume ~ diameter) +
  geom_point() +
  geom_line(f_aes(pred ~ diameter))

# Données de paramétrisation du modèle
(trees_tidy <- tidy(trees_lm))

# Idem, mais formatage du tableau avec tabularise()
tabularise$tidy(trees_lm, header = FALSE)

# Métriques mesurant la qualité d'ajustement de notre modèle
(trees_glance <- glance(trees_lm))

# Idem avec formatage tabularise()
tabularise$glance(trees_lm, header = FALSE)
