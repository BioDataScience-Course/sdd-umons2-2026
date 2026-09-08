#' ---
#' title: "SDD II module 4 : Modèle linéaire généralisé"
#' author: "Ph. Grosjean et G. Engels"
#' date: "2025-2026"
#' output:
#'   html_document:
#'     highlight: kate
#' ---
#' 
#' Document complémentaire au [module 4 du cours SDD II de 2025-2026](https://wp.sciviews.org/sdd-umons2-2025/mod-lineaire-gen.html).
#' Distribué sous licence [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.fr).
#' 
#' **Veuillez vous référer au cours en ligne pour les explications et les interprétations de cette analyse.**
#' 
#' [Installer un environnement R](https://github.com/SciViews/svbox/tree/main/svbox2025-native)
#' adéquat pour reproduire cette analyse. 

#'
#' ### GLM poisson : ray-grass dans les dunes
#'

# Configuration de R en dialecte SciViews::R pour la modélisation
SciViews::R("model", lang = "fr")
# Importation et préparation des données
dune <- sbind_cols(
  read("dune", package = "vegan") |> sselect(Lolipere),
  read("dune.env", package = "vegan") |> sselect(A1, Moisture, Management)
) %>.%
  smutate(., Management = case_when(
    Management == "NM" ~ "conservation",
    .default = "culture") |> factor())
skimr::skim(dune)

# Abondance de L. perenne
chart(data = dune, ~ Lolipere) +
  geom_bar() +
  labs(x = "Abondance [nbr de plants/quadrat]",
       y = "Nombre de sites")

# Table de contingence humidité du sol versus mode de gestion des dunes
table(Humidité = dune$Moisture, Gestion = dune$Management) |>
  tabularise()

# Abondance de L. perenne en fonction de l'épaisseur de la couche A1 du sol
chart(data = dune, Lolipere ~ A1) +
  geom_point() +
  labs(x = "Épaisseur de la couche A1 [cm]",
       y = "Abondance [nbr de plants/quadrat]")

# GLM de famille poisson
dune_glm <- glm(data = dune, Lolipere ~ A1, family = poisson)
summary_(dune_glm) |> tabularise()

# Même modèle, mais en GLM quasi-poisson
dune_glm_quasi <- glm(data = dune, Lolipere ~ A1, family = quasipoisson)
summary_(dune_glm_quasi) |> tabularise()

# Modèle GLM poisson plus complet (avec plus de variables indépendantes)
dune_glm2 <- glm(data = dune, Lolipere ~ A1 +  Management + Moisture,
  family = poisson)
summary_(dune_glm2) |> tabularise()

# Comparaison des deux modèles imbriqués par test Chi-carré
anova(dune_glm, dune_glm2, test = "Chisq") |> tabularise()

# Exemple de prédictions en utilisant un modèle GLM
new_sites <- dtbl_rows(
 ~A1, ~Moisture, ~Management,
   3,         1, "conservation",
   3,         1, "culture",
   5,         2, "conservation",
   5,         2, "culture"
) %>.%
  mutate(., Moisture   = ordered(Moisture, levels = c(1, 2, 4, 5)),
            Management = factor(Management))
# Probabilité d'abondance de L. perenne en ces sites
new_sites$pred <- predict(dune_glm2,
  newdata = new_sites, type = "response")
new_sites

# Graphique des observations avec distinction par gestion et humidité
chart(data = dune, Lolipere ~ A1 %size=% Moisture %col=% Management) +
  geom_point(alpha = 0.7) +
  labs(x = "Épaisseur de la couche A1 [cm]",
       y = "Abondance [nbr de plants/quadrat]")

#'
#' ### GLM binomiale avec proportions : maturation d'ovocytes
#'

# Préparation des données de maturation d'ovocytes par dose d'hypoxanthine
ovo <- dtbl_rows(
  ~hypo, ~mat, ~tot,
      4,    0,   32,
      3,    3,   23,
      2,   12,   24,
      1,   24,   32,
    0.5,   26,   29,
   0.25,   28,   30,
      0,   35,   35
) %>.%
  mutate(., prop = mat/tot)
# Graphique général
chart(data = ovo, prop ~ hypo) +
  geom_point() +
  labs(x = "Hypoxanthine [µM]",
       y = "Fraction d'ovocytes matures")

# GLM binomiale avec proportions ; notez bien l'utilisation de weights=... ici !
ovo_glm <- glm(data = ovo, prop ~ hypo,
  family = binomial, weights = ovo$tot)
summary_(ovo_glm) |> tabularise()

# Autre formulation de GLM binomiale ne nécessitant pas d'indiquer weights=...
ovo_glm_bis <- glm(data = ovo, cbind(mat, tot) ~ hypo,
  family = binomial)
summary_(ovo_glm) |> tabularise()

# Graphique de notre modèle GLM binomiale avec proportions
chart(data = ovo, prop ~ hypo) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family = binomial),
    formula = y ~ x, se = FALSE) +
  labs(x = "Hypoxanthine [µM]",
       y = "Fraction d'ovocytes matures")

#'
#' ### GLM binomiale avec variable binaire : acariens
#'

# Préparation des données sur les acariens
mite <- sbind_cols(
  read("mite", package = "vegan") |> sselect(Oribatl1),
  read("mite.env", package = "vegan") |> sselect(WatrCont, Topo)
) %>.%
  smutate(., Oribatl1 = case_when(
    Oribatl1 == 0 ~ "absent",
    .default = "present") |> factor())
skimr::skim(mite)

# Graphique des données par rapport au contenu en eau et à la topographie
chart(data = mite, Oribatl1 ~ WatrCont | Topo) +
  geom_point(alpha = 0.5) +
  labs(x = "Teneur en eau [g/L]", y = "Larves d'acariens")

# Modèle GLM binomial à partir de données binaires
mite_glm <- glm(data = mite, Oribatl1 ~ WatrCont * Topo,
  family = binomial)
summary_(mite_glm) |> tabularise()

# Modèle GLM binomial simplifié sans Topo
mite_glm2 <- glm(data = mite, Oribatl1 ~ WatrCont,
  family = binomial)
summary_(mite_glm2) |> tabularise()

# Comparaison des deux modèles imbriqués par test Chi-carré
anova(mite_glm, mite_glm2, test = "Chisq")

# Modèle GLM binomial avec Topo, mais sans interactions
mite_glm3 <- glm(data = mite, Oribatl1 ~ WatrCont + Topo,
  family = binomial)
summary_(mite_glm3) |> tabularise()

# Comparaison du dernier modèle avec le modèle complet par test Chi-carré
anova(mite_glm, mite_glm3, test = "Chisq")

# Exemple d'utilisation du modèle GLM binomial pour des prédictions
new_soils <- dtbl_rows(
 ~WatrCont, ~Topo,
       150, "Blanket",
       500, "Blanket",
       800, "Blanket",
       200, "Hummock",
       400, "Hummock"
)
# Probabilité de larves d'acariens pour ces sols
new_soils$P <- predict(mite_glm3,
  newdata = new_soils, type = "response")
new_soils

# Transformer la variable présence/absence en 0/1 avant de faire le graphique
mutate(mite, Ori = as.numeric(Oribatl1 == "present")) %>.%
  chart(data = ., Ori ~ WatrCont %col=% Topo) +
    geom_point(alpha = 0.5) +
    stat_smooth(
      method = "glm",
      method.args = list(family = binomial),
      formula = y ~ x,
      se = TRUE) +
  labs(x = "Teneur en eau [g/L]", y = "Probabilité de présence")

#'
#' ### Modèle linéaire généralisé mixte {#modlingenmixte}
#'

# Données sur la mobilité des spermatozoïdes en fonction de la dose d'éthanol
spe <- dtbl_rows(
 ~donor, ~conc, ~mobile, ~total,
      1,   0.0,     236,    301,
      1,   0.1,     238,    301,
      1,   0.5,     115,    154,
      1,   1.0,     105,    196,
      1,   2.0,     182,    269,
      2,   0.0,      92,    150,
      2,   0.1,      60,    111,
      2,   0.5,      63,    131,
      2,   1.0,      46,     95,
      2,   2.0,      50,    125,
      3,   0.0,     100,    123,
      3,   0.1,      91,    117,
      3,   0.5,     132,    162,
      3,   1.0,     145,    187,
      3,   2.0,      52,     92,
      4,   0.0,      83,    113,
      4,   0.1,     104,    123,
      4,   0.5,      65,     87,
      4,   1.0,      93,    136,
      4,   2.0,      78,    117,
      5,   0.0,     127,    152,
      5,   0.1,      82,    114,
      5,   0.5,      55,     84,
      5,   1.0,      80,    103,
      5,   2.0,      98,    120,
      6,   0.0,      62,     77,
      6,   0.1,      65,     79,
      6,   0.5,      63,     72,
      6,   1.0,      57,     67,
      6,   2.0,      39,     66,
      7,   0.0,      91,    116,
      7,   0.1,      51,     71,
      7,   0.5,      70,     87,
      7,   1.0,      53,     72,
      7,   2.0,      59,     82,
      8,   0.0,     121,    137,
      8,   0.1,      80,     98,
      8,   0.5,     100,    122,
      8,   1.0,     126,    157,
      8,   2.0,      98,    122
)

# Réencodage de ces données
spe <- smutate(spe, mob_frac = mobile / total, donor = as.factor(donor), conc = as.numeric(conc))
head(spe)

# Modèle GLLM binomial mixte complet (attention : tabularise() pas utilisable)
spe_m1 <- lme4::glmer(data = spe, cbind(mobile, total - mobile) ~ conc + (conc | donor),
  family = binomial(link = "logit"))
summary_(spe_m1)

# Graphique du modèle GLMM complet
chart(data = spe, mob_frac ~ conc %col=% donor) +
  geom_point() +
  geom_line(f_aes(fitted(spe_m1) ~ conc %col=% donor)) +
  labs(x   = "Ethanol [%]",
       y   = "Mobilité des spermatozoïdes [%]",
       col = "Donneur")

# Modèle GLMM binomial simplifié
spe_m2 <- lme4::glmer(data = spe,
  cbind(mobile, total - mobile) ~ conc + (1 | donor),
  family = binomial(link = "logit"))
anova(spe_m1, spe_m2)

# Résumé du modèle GLMM simplifié
summary_(spe_m2)

# Graphique du modèle GLMM simplifié
chart(data = spe, mob_frac ~ conc %col=% donor) +
  geom_point() +
  geom_line(f_aes(fitted(spe_m2) ~ conc %col=% donor)) +
  labs(x   = "Ethanol [%]",
       y   = "Mobilité des spermatozoïdes [%]",
       col = "Donneur")

# Intervalle de confiance à 95% sur les paramètres du modèle GLMM simplifié
confint(spe_m2)

# Vérification de la singularité et test d'autres optimiseurs pour notre GLMM
lme4::isSingular(spe_m2)
lme4::allFit(spe_m2)

# Analyse des résidus de Pearson de notre GLMM simplifiée
chart(data = spe,
  resid(spe_m2, type = "pearson") ~ fitted(spe_m2) %col=% donor) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x = "Valeurs ajustées", y = "Résidus de Pearson")

# Graphique de vérification de l'homoscédasticité des résidus de Pearson
chart(data = spe, sqrt(abs(resid(spe_m2, type = "pearson"))) ~ fitted(spe_m2) %col=% donor) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x = "Valeurs ajustées", y = "|Résidus de Pearson|^.5")

# Analyse des résidus de Pearson en fonction d'effet fixe (ici conc)
chart(data = spe, resid(spe_m2, type = "pearson") ~ conc %col=% donor) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x = " Ethanol [%]", y = "Résidus de Pearson")

# Graphique quantitle-quantile des résidus de Pearson -> PAS NÉCESSAIRE !
chart(data = spe, aes(sample = resid(spe_m2, type = "pearson"))) +
  geom_qq() +
  geom_qq_line()

# Fonction de lien utilisée pour notre GLMM
logit <- make.link("logit")
# Transforme quelques valeurs de Y (comprises entre 0 et 1, proportions)
y <- c(0.8, 0.85, 0.9, 0.95)
(y_logit <- logit$linkfun(y))
# Retransforme en y à l'aide de la fonction inverse
(y2 <- logit$linkinv(y_logit))
all.equal(y, y2)

# Coefficients de notre modèle GLMM simplifié
coef(spe_m2)

# Effets fixes de notre GLLM simplifiée
(spe_m2_fixef <- lme4::fixef(spe_m2))

# Prédiction à partir de quelques valeurs de concentration
intercept <- spe_m2_fixef[1]
slope <- spe_m2_fixef[2]
conc <- c(0, 0.25, 0.5, 1, 2)
slope * conc + intercept

# Il faut appliquer la transformation inverse pour les fractions de mobilité
(mobi <- logit$linkinv(slope * conc + intercept))

# Diminution de la mobilité pour une concentration de 1% d'éthanol
mobi[1] - mobi[4]
