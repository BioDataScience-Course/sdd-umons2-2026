#' ---
#' title: "SDD II module 2 : Régression linéaire II"
#' author: "Ph. Grosjean et G. Engels"
#' date: "2025-2026"
#' output:
#'   html_document:
#'     highlight: kate
#' ---
#' 
#' Document complémentaire au [module 2 du cours SDD II de 2025-2026](https://wp.sciviews.org/sdd-umons2-2025/lm2.html).
#' Distribué sous licence [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.fr).
#' 
#' **Veuillez vous référer au cours en ligne pour les explications et les interprétations de cette analyse.**
#' 
#' [Installer un environnement R](https://github.com/SciViews/svbox/tree/main/svbox2025-native)
#' adéquat pour reproduire cette analyse. 

# Dialecte SciViews::R avec la section dédiée à la modélisation
SciViews::R("model", lang = "fr")

# Importation des données depuis le package {datasets}
trees <- read("trees", package = "datasets")
# Régression (même modèle que dans le module 1)
trees_lm <- lm(data = trees, volume ~ diameter)
# Résumé du modèle
summary_(trees_lm)

# Tableau de l'ANOVA pour notre modèle
anova(trees_lm)

# Version formatée à l'aide de tabularise()
anova(trees_lm) |>
  tabularise()

# Résumé du modèle formaté avec tabuslarise()
summary_(trees_lm) |>
  tabularise()

# Second modèle ajusté sur un sous-ensemble des données
trees_lm2 <- lm(data = trees, volume ~ diameter, subset = diameter < 0.5)
chart(trees_lm2)

# Résumé de notre second modèle
summary_(trees_lm2) |>
  tabularise()

# Analyse des résidus (4 graphiques principaux)
chart$residuals(trees_lm2)

# Régression multiple utilisant deux variable indépendantes
trees_lm3 <- lm(data = trees, volume ~ diameter + height)
summary_(trees_lm3) |>
  tabularise()

# Analyse des résidus synthétique du troisième modèle
chart$residuals(trees_lm3) 

# Comparaison de deux modèles imbriqués par ANOVA
anova(trees_lm, trees_lm3) |>
  tabularise()

# Modèle polynomial d'ordre 2
trees_lm4 <- lm(data = trees, volume ~  diameter + I(diameter^2))
summary_(trees_lm4) |> tabularise()
chart(trees_lm4)

# Analyse des résidus synthétique du modèle polynomial
chart$residuals(trees_lm4)

# Modèle simplifié
trees_lm5 <- lm(data = trees, volume ~  I(diameter^2) + 0)
summary_(trees_lm5) |>
  tabularise()

# Modèle multiple et polynomial
trees_lm6 <- lm(data = trees, volume ~  diameter + I(diameter^2) + height)
summary_(trees_lm6) |>
  tabularise()

# Calcul du RMSE pour le modèle linéaire simple
rmse(trees_lm, trees)

# Comparaison de modèles par le critère d'Akaike
AIC(trees_lm) # Linéaire diamètre
AIC(trees_lm3) # Multiple diamètre et hauteur
AIC(trees_lm4) # Polynomial d'ordre 2 diamètre
AIC(trees_lm5) # Diamètre^2
AIC(trees_lm6) # Multiple et polynomial

# Analyse des résidus du modèle multiple et polynomial
chart$residuals(trees_lm6)

# Calcul du RMSE pour les deux modèles finalistes
rmse(trees_lm6, trees) # Multiple et polynomial
rmse(trees_lm4, trees) # Polynomial ordre 2 diamètre
