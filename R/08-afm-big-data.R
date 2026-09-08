#' ---
#' title: "SDD II module 8 : AFM & big data"
#' author: "Ph. Grosjean et G. Engels"
#' date: "2025-2026"
#' output:
#'   html_document:
#'     highlight: kate
#' ---
#' 
#' Document complémentaire au [module 8 du cours SDD II de 2025-2026](https://wp.sciviews.org/sdd-umons2-2025/afm-big-data.html).
#' Distribué sous licence [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.fr).
#' 
#' **Veuillez vous référer au cours en ligne pour les explications et les interprétations de cette analyse.**
#' 
#' [Installer un environnement R](https://github.com/SciViews/svbox/tree/main/svbox2025-native)
#' adéquat pour reproduire cette analyse. 

#'
#' ### Première AFM sur le plancton avec seulement des ACP {#afm1}
#'

# Chargement du dialecte SciViews::R avec le module explore
SciViews::R("explore", lang = "fr")
# Identification des différentes masses d'eaux (transect entre Nice et la Corse)
marzones <- factor(c(
  rep("périphérique", 16),
  rep("divergente1", 8),
  rep("convergente", 5),
  rep("frontale", 11),
  rep("divergente2", 5),
  rep("centrale", 23)),
  levels = c("périphérique", "divergente1", "convergente",
    "frontale", "divergente2", "centrale"))

# Jointure des deux tableaux marphy, marbio et ajout de marzones
bind_cols(
  read("marphy", package = "pastecs"),
  log1p(read("marbio", package = "pastecs"))) %>.%
  smutate(., Zone = marzones) ->
  mar

# Nom des variables de mar
names(mar)

# Première AFM sur mar
mar_mfa <- mfa(data = mar,
  ~ 4*std %as% environment + 24*num %as% plancton - 1*fct %as% zone)

# Résumé de la première AFM
summary(mar_mfa)

# Graphique des éboulis de la première AFM
chart$scree(mar_mfa, fill = "cornsilk")

# Graphique des variables de la première AFM
chart$loadings(mar_mfa, choices = c(1, 2))

# Graphique du compromis de la la première AFM
chart$axes(mar_mfa, choices = c(1, 2))

# Graphique des individus de la première AFM
chart$scores(mar_mfa, choices = c(1, 2))

# Variante du graphique des individus avec annotations des zones et ellipses
chart$ellipses(mar_mfa, choices = c(1, 2), keepvar = "Zone")

# Graphique de la relation entre groupes de la première AFM
chart$groups(mar_mfa, choices = c(1, 2))

#'
#' ### Seconde AFM sur le plancton avec données mixtes ACP et AFC {#afm2}
#'

# Seconde AFM avec plancton traité selon une AFC au lieu d'une ACP
mar_mfa2 <- mfa(data = mar,
  ~ 4*std %as% environment + 24*cnt %as% plancton - 1*fct %as% zone)

# Premières valeurs propres de la seconde AFM
head(mar_mfa2$eig)

# Graphique des éboulis de la seconde AFM
chart$scree(mar_mfa2, fill = "cornsilk")

# Graphique des variables de la seconde AFM
chart$loadings(mar_mfa2, choices = c(1, 2))

# Graphique du compromis de la la seconde AFM
chart$axes(mar_mfa2, choices = c(1, 2))

# Graphique des individus annoté pour la seconde AFM
chart$ellipses(mar_mfa2, choices = c(1, 2), keepvar = "Zone")

# Graphique de la relation entre groupes de la seconde AFM
chart$groups(mar_mfa2, choices = c(1,2))

# Biplot de l'AFC réalisée sur le plancton de la seconde AFM
chart$contingency(mar_mfa2, choices = c(1, 2))

#'
#' ### Big data et base de données {#bigdata}
#'

# # Exemple fictif de lecture de données depuis internet avec mise en cache
# # (décommentez et ajustez le code ci-dessous pour l'utiliser)
# #dir_create("data/cache") # S"assurer que le dossier existe
# #big_data <- read$csv.xz("https://mysite.org/mybigdata.cs.xz",
# #  cache_file = "data/cache/mybigdata.csv.xz")

# Chargement du dialecte SciViews::R
SciViews::R
# Lecture du jeu de données babynames depuis le package du même nom
babynames <- read("babynames", package = "babynames")

# Création de la base de données et copie de babynames dedans
con <- DBI::dbConnect(duckdb::duckdb())
copy_to(con, babynames)

# Élimination du jeu de données dans la session R (on garde la version duckdb)
rm(babynames)

# Liste des tables dans notre base de données
DBI::dbListTables(con)

# Liste des champs (colonnes) de la table babynames dans la base de données
DBI::dbListFields(con, "babynames")

# Liaison à une table dans la base de données
db_babynames <- tbl(con, "babynames")
# Premières lignes de la table
head(db_babynames)

# Remaniement de données dans la base de données
db_babynames %>.%
  group_by(., name) %>.%
  summarise(., total = sum(n, na.rm = TRUE)) %>.%
  arrange(., desc(total)) ->
  query

# Visualiation de la requête SQL équivalent
show_query(query)

# Réalisation de la requête SQL
names <- collect_dtx(query)
names

# Déconnexion et destruction de notre base de données en mémoire
DBI::dbDisconnect(con, shutdown = TRUE)
