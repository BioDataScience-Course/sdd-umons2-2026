#' ---
#' title: "SDD II module 5 : Régression non linéaire"
#' author: "Ph. Grosjean et G. Engels"
#' date: "2025-2026"
#' output:
#'   html_document:
#'     highlight: kate
#' ---
#' 
#' Document complémentaire au [module 5 du cours SDD II de 2025-2026](https://wp.sciviews.org/sdd-umons2-2025/reg-non-lin.html).
#' Distribué sous licence [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.fr).
#' 
#' **Veuillez vous référer au cours en ligne pour les explications et les interprétations de cette analyse.**
#' 
#' [Installer un environnement R](https://github.com/SciViews/svbox/tree/main/svbox2025-native)
#' adéquat pour reproduire cette analyse. 

#'
#' ### Rendement photosynthétique {#rend-photo}
#'

# Préparation des données de "rapid light curve"
rlc <- as_dtx(tribble(
  ~etr, ~par,
  0.0,  0,
  0.5,  2,
  3.2,  11,
  5.7,  27,
  7.4,  50,
  8.4,  84,
  8.9,  170,
  8.4,  265,
  7.8,  399
))
# Labélisation
rlc <- labelise(rlc,
  label = list(
    etr = "ETR",
    par = "PAR"),
  units = list(
    etr = "µmol électrons/m²/s",
    par = "µmol photons/m²/s")
)
# Graphique général de la RLC
chart(data = rlc, etr ~ par) +
  geom_point()

# Modèle polynomial RLC (inadéquat)
rlc_lm <- lm(data = rlc, etr ~  par + I(par^2) + I(par^3))
chart(rlc_lm)

# Équation du modèle PGH
pgh_model <- function(x, etr_max, alpha, beta)
  etr_max * (1 - exp(-x * alpha/etr_max)) * (exp(-x * beta/etr_max))

# Régression non linéaire en utilisant PGH
rlc_nls <- nls(data = rlc, etr ~ pgh_model(par, etr_max, alpha, beta),
  start = list(etr_max = 9, alpha = 1, beta = 0))
# Note : tabularise() ne peut pas encore traiter les modèles non "self-start"
# (vous verrez ce que cela signifie plus loin) pour les équations. Donc,
# nous demandons le tableau SANS l'équation du modèle
summary_(rlc_nls) |> tabularise(equation = FALSE)
# Une autre solution est de fournir l'équation en LaTeX à tabularise()
#summary(rlc_nls) |> tabularise(equation = "ETR = ETR_{max} \\cdot (1 - e^{-PAR \\cdot \\alpha/ETR_{max}}) \\cdot e^{-PAR \\cdot \\beta/ETR_{max}}")

# Graphique de la RLC avec le modèle PGH non linéaire ajusté
chart(data = rlc, etr ~ par) +
  geom_point() +
  stat_function(fun = as.function(rlc_nls), col = "skyblue3", linewidth = 1)
# On peut aussi faire simplement (mais c'est moins flexible) :
#chart(rlc_nls)

# Comparaison des deux modèles par le critère d'Akaike
AIC(rlc_lm, rlc_nls)

# Ajustement du modèle avec trace=TRUE pour voir la progression des étapes
rlc_nls <- nls(data = rlc, etr ~ pgh_model(par, etr_max, alpha, beta),
  start = list(etr_max = 9, alpha = 1, beta = 0),
  trace = TRUE)

#'
#' ### Modèles courants en biologie {#modelesbio}
#'

# Objects R commençant par "SS" (modèles self-start pour la plupart)
apropos("^SS", ignore.case = FALSE)

# Fonction exponentielle (pas de modèle self-start)
exponent <- function(x, y0, k)
  y0 * exp(k * x)

# Modèle von Bertalanffy en poids (pas de modèle self-start)
asympOff3 <- function(x, Asym, lrc, c0, m) Asym*(1 - exp(-exp(lrc) * (x - c0)))^3

# Equation de Richards (pas de modèle self-start)
richards <- function(x, Asym, lrc, c0, m) Asym*(1 - exp(-exp(lrc) * (x - c0)))^m

# Equation de Preece Baines 1 de croissance humaine (pas de modèle self-start)
preece_baines1 <- function(x, Asym, Drop, lrc1, lrc2, c0)
  Asym - (2 * (Asym - Drop)) / (exp(exp(lrc1) * (x - c0)) + exp(exp(lrc2) * (x - c0)))

# Équation de Tanaka de croissance indéterminée (pas de modèle self-start)
tanaka <- function(x, a, b, c0, d)
  1 / sqrt(b) * log(abs(2 * b * (x - c0) + 2 * sqrt(b^2 * (x - c0)^2 + a * b))) + d

#'
#' ### Choix du modèle {#choixmodele}
#'

# Configuration de R pour le dialecte SciViews avec modélisation
SciViews::R("model", lang = "fr")
# Chargement et visualisation des données
urchins <- read("urchin_growth", package = "data.io")
chart(data = urchins, diameter ~ age) +
  geom_point()

# Graphique avec points jitterisés
urchins_plot <- chart(data = urchins, diameter ~ age) +
  geom_jitter(width = 0.1, alpha = 0.2, color = "darkgrey") +
  ggtitle(expression(paste("Croissance de l'oursin ", italic("Paracentrotus lividus"))))
urchins_plot

# Régression non linéaire avec modèle de Gompertz
urchins_gomp <- nls(data = urchins, diameter ~ SSgompertz(age, Asym, b2, b3))
summary_(urchins_gomp) |> tabularise()

# Graphique du modèle de Gompertz ajusté
urchins_plot +
  stat_function(fun = as.function(urchins_gomp), color = "red", linewidth = 1)

# Régression non linéaire avec modèle logistique
urchins_logis <- nls(data = urchins, diameter ~ SSlogis(age, Asym, xmid, scal))
summary_(urchins_logis) |> tabularise()

# Graphique avec les deux modèles ajustés pour comparaison visuelle
urchins_plot +
  stat_function(fun = as.function(urchins_gomp), aes(color = "Gompertz"), linewidth = 1) +
  stat_function(fun = as.function(urchins_logis), aes(color = "logistique"), linewidth = 1) +
  labs(color = "Modèle")

# Comparaison des deux modèles via le critère d'Akaike
AIC(urchins_gomp, urchins_logis)

# Régression non linéaire avec modèle de Weibull
urchins_weib <- nls(data = urchins, diameter ~ SSweibull(age, Asym, Drop, lrc, pwr))
summary_(urchins_weib) |> tabularise()

# Graphique des trois modèles ajustés
urchins_plot +
  stat_function(fun = as.function(urchins_gomp), aes(color = "Gompertz"), linewidth = 1) +
  stat_function(fun = as.function(urchins_logis), aes(color = "logistique"), linewidth = 1) +
  stat_function(fun = as.function(urchins_weib), aes(color = "Weibull"), linewidth = 1) +
  labs(color = "Modèle")

# Comparaison par critère d'Akaike
AIC(urchins_gomp, urchins_logis, urchins_weib)

# Équation de Richards
richards <- function(x, Asym, lrc, c0, m) Asym*(1 - exp(-exp(lrc) * (x - c0)))^m

try({
# Régression non linéaire avec modèle de Richards
urchins_rich <- nls(data = urchins, diameter ~ richards(age, Asym, lrc, c0, m),
  start = c(Asym = 55, lrc = -0.7, c0 = 0, m = 1))
# N'étant pas une fonction selfStart, tabularise() ne peut pas déterminer
# l'équation du modèle toute seule. Donc, nous ne l'affichons pas.
summary_(urchins_rich) |> tabularise(equation = FALSE)
})

# Graphique avec les 4 modèles ajustés
urchins_plot +
  stat_function(fun = as.function(urchins_gomp), aes(color = "Gompertz"), linewidth = 0.7) +
  stat_function(fun = as.function(urchins_logis), aes(color = "logistique"), linewidth = 0.7) +
  stat_function(fun = as.function(urchins_weib), aes(color = "Weibull"), linewidth = 0.7) +
  stat_function(fun = as.function(urchins_rich), aes(color = "Richards"), linewidth = 0.7) +
  labs(color = "Modèle")

# Comparaison par critère d'Akaike des 4 modèles
AIC(urchins_gomp, urchins_logis, urchins_weib, urchins_rich)
