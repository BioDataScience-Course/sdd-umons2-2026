#' ---
#' title: "SDD II module 3 : Modèle linéaire"
#' author: "Ph. Grosjean et G. Engels"
#' date: "2025-2026"
#' output:
#'   html_document:
#'     highlight: kate
#' ---
#' 
#' Document complémentaire au [module 3 du cours SDD II de 2025-2026](https://wp.sciviews.org/sdd-umons2-2025/mod-lineaire.html).
#' Distribué sous licence [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.fr).
#' 
#' **Veuillez vous référer au cours en ligne pour les explications et les interprétations de cette analyse.**
#' 
#' [Installer un environnement R](https://github.com/SciViews/svbox/tree/main/svbox2025-native)
#' adéquat pour reproduire cette analyse. 

# Matrices de contraste par défaut
getOption("contrasts")

# Dialecte SciViews::R avec la section dédiée à la modélisation
SciViews::R("model", lang = "FR")
# Importation des données et élimination des données manquantes pour skeleton
read("urchin_bio", package = "data.io") %>.%
  sdrop_na(., skeleton) ->
  urchin
# Graphique de base
chart(data = urchin, skeleton ~ weight %col=% origin) +
  geom_point() +
  geom_vline(xintercept = 30, col = "darkgray", lty = 2)

# Modèle linéaire avec interactions, oursins de masse supérieure ou égale à 30g
urchin_lm <- lm(data = urchin, skeleton ~ weight * origin, subset = weight >= 30)
# Graphique de ce modèle linéaire
chart(data = sfilter(urchin, weight >= 30), skeleton ~ weight %col=% origin) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x)

# Résumé du modèle
summary_(urchin_lm)

# ANOVA du modèle linéaire
anova(urchin_lm)

# Second modèle linéaire sans décalage d'origine
urchin_lm2 <- lm(data = urchin, skeleton ~ weight + weight:origin, subset = weight >= 30)
summary_(urchin_lm2)

# Anova du second modèle
anova(urchin_lm2)

# Tableau formaté du résumé du modèle simplifié
summary_(urchin_lm2) |> tabularise()

# Pour obtenir l'équation du modèle dans un document R Markdown ou Quarto:
#' $$`r eq__(urchin_lm2, use_coefs = TRUE, coef_digits = c(2, 2, 3))`$$

# Analyse des résidus du modèle simplifié
chart$residuals(urchin_lm2)

#'
#' ### Second exemple plus compliqué : bébés à la naissance
#'

# Configuration de l'environnement SciViews::R en français
SciViews::R("model", lang = "fr")
# Importation des données
babies <- read("babies", package = "UsingR")
tabularise$headtail(babies[, c("wt", "wt1", "smoke")])

# Remaniement des données :
# wt = masse du bébé à la naissance en onces et 999 = valeur manquante
# wt1 = masse de la mère à la naissance en livres et 999 = valeur manquante
# smoke = 0 (non), = 1 (oui), = 2 (jusqu'à grossesse),
#       = 3 (plus depuis un certain temps) and = 9 (inconnu)
#       transformé en 0 -> "never", 3 -> "before", 2 -> "until", 1 -> "during"
#       et NA = 9 (éliminé)
babies %>.% 
  sselect(., wt, wt1, smoke) %>.% # Garder seulement wt, wt1 & smoke
  sfilter(., wt1 < 999, wt < 999, smoke < 9) %>.% # Éliminer les valeurs manquantes
  smutate(., wt = wt * 0.02835) %>.% # Transformer le poids de livres en kg
  smutate(., wt1 = wt1 * 0.4536) %>.% # Idem de onces en kg
  smutate(., smoke = recode(smoke, "0" = "never", "3" = "before",
                                   "2" = "until", "1" = "during")) %>.%
  smutate(., smoke = factor(smoke,
    levels = c("never", "before", "until", "during"))) ->
  babies2 # Enregistrer le résultat dans babies2

tabularise$headtail(babies2)

# Tableaux de description des données
skimr::skim(babies2)

# Graphique du poids du bébé en fonction du poids de la mère et de smoke
chart(data = babies2, wt ~ wt1 %col=% smoke) +
  geom_point() +
  xlab("wt1 : masse de la mère [kg]") +
  ylab("wt : masse du bébé [kg]")

# Boite à moustaches du poids du bébé en fonction de smoke
chart(data = babies2, wt ~ smoke) +
  geom_boxplot() +
  ylab("wt : masse du bébé [kg]")

# Boite à moustaches du poids de la mère en fonction de smoke
chart(data = babies2, wt1 ~ smoke) +
  geom_boxplot() +
  ylab("wt1 : masse de la mère [kg]")

# Modèle linéaire de type (anciennement) ANCOVA
babies2_lm <- lm(data = babies2, wt ~ smoke * wt1)
summary_(babies2_lm) |> tabularise()
anova(babies2_lm) |> tabularise()

chart(data = babies2, wt ~ wt1 %col=% smoke) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  xlab("wt1 : masse de la mère [kg]") +
  ylab("wt : masse du bébé [kg]")

# Modèle linéaire sans interactions (+ au lieu de * dans la formule)
babies2_lm2 <- lm(data = babies2, wt ~ smoke + wt1)
summary_(babies2_lm2) |> tabularise()
anova(babies2_lm2) |> tabularise()

# Graphique du dernier modèle (difficile à réaliser)
offsets <- c(0, 0.03549, 0.02267, -0.23794)
cols <- scales::hue_pal()(4)
chart(data = babies2, wt ~ wt1) +
  geom_point(aes(col = smoke)) +
  purrr::map2(offsets, cols, function(offset, col)
    geom_smooth(method = lm, formula = y + offset ~ x, col = col)) +
  xlab("wt1 : masse de la mère [kg]") +
  ylab("wt : masse du bébé [kg]")

# Analyse post-hoc du dernier modèle
babies2_lm2_compa <- multcomp::glht(babies2_lm2,
  linfct = multcomp::mcp(smoke = "Tukey")) |> confint()
summary_(babies2_lm2_compa)
.oma <- par(oma = c(0, 5.1, 0, 0)); plot(babies2_lm2_compa); par(.oma); rm(.oma)

# Nouveau modèle utilisant les contrastes de Helmert
babies2_lm3 <- lm(data = babies2, wt ~ smoke + wt1,
  contrasts = list(smoke = "contr.helmert"))
summary_(babies2_lm3) |> tabularise()
anova(babies2_lm3) |> tabularise()

# Matrice de Helmert correspondant à notre modèle
helm <- contr.helmert(4)
rownames(helm) <- c('smoke == "never"', 'smoke == "before"',
                    'smoke == "until"', 'smoke == "during"')
colnames(helm) <- c('smoke1', 'smoke2', 'smoke3')
helm

# Encore un autre modèle, en considérant smoke comme variable ordonnée
smutate(babies2, smoke = as.ordered(smoke)) |>
  lm(data = _, wt ~ smoke + wt1) |>
  summary_() |>
  tabularise()

# Recodage de smoke en smokepreg : contraster fumer pendant la grossesse ou non
babies2 %>.%
  smutate(., smokepreg = recode(smoke, "never" = "0", "before" = "0",
                                       "until" = "0", "during" = "1")) %>.%
  smutate(., smokepreg = factor(smokepreg, levels = c("0", "1"))) ->
  babies2
# Modèle utilisant smokepreg
babies2_lm4 <- lm(data = babies2, wt ~ smokepreg + wt1)
summary_(babies2_lm4) |> tabularise()
anova(babies2_lm4) |> tabularise()

# Graphique du modèle linéaire avec smokepreg
offsets <- c(0, -0.2458)
cols <- scales::hue_pal()(2)
chart(data = babies2, wt ~ wt1) +
  geom_point(aes(col = smokepreg)) +
  purrr::map2(offsets, cols, function(offset, col)
    geom_smooth(method = lm, formula = y + offset ~ x, col = col)) +
  xlab("wt1 : masse de la mère [kg]") +
  ylab("wt : masse du bébé [kg]")

# Graphique principal d'analyse des résidus
chart$resfitted(babies2_lm4)

# Graphique quantile-quantile d'analyse des résidus
chart$qqplot(babies2_lm4)

# Graphique d'analyse des résidus pour vérifier l'homoscédasticité
chart$scalelocation(babies2_lm4)

# Graphique de la distance de Cook des résidus
chart$cooksd(babies2_lm4)
