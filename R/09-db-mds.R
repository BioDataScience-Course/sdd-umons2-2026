#' ---
#' title: "SDD II module 9 : bases de données & MDS"
#' author: "Ph. Grosjean et G. Engels"
#' date: "2025-2026"
#' output:
#'   html_document:
#'     highlight: kate
#' ---
#' 
#' Document complémentaire au [module 9 du cours SDD II de 2025-2026](https://wp.sciviews.org/sdd-umons2-2025/db-mds.html).
#' Distribué sous licence [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.fr).
#' 
#' **Veuillez vous référer au cours en ligne pour les explications et les interprétations de cette analyse.**
#' 
#' [Installer un environnement R](https://github.com/SciViews/svbox/tree/main/svbox2025-native)
#' adéquat pour reproduire cette analyse. 

#'
#' ### Accès aux bases de données {#db}
#'

# Installer le dialecte SciViews::R avec le module "explore"
SciViews::R("explore")

# Charger les packages nécessaires à l'accès à la base de données DuckDB
library(DBI)
library(duckdb)

# Créer la base de données dans un fichier
drv <- duckdb(dbdir = "duckdb_test.db")
# Se connecter à la base de données
con <- dbConnect(drv)

# Lecture des données who
who <- read("who", package = "tidyr")
# Structure du jeu de données who
str(who)
# Lecture du jeu de données pop
pop <- read("population", package = "tidyr")
# Structure du jeu de données pop
str(pop)

# Pivoter le tableau who on long
whol <- pivot_longer(who, starts_with("new"), names_to = "type", values_to = "new_cases")
dim(whol)
head(whol)

# Découpage des trois champs à partir de la chaine de caractères "new_ep_f2534"
substring("new_ep_f2534", 1, 6) # method
substring("new_ep_f2534", 8, 8) # sex
substring("new_ep_f2534", 9, 12) # age

# Extractions des trois champs depuis type
whol %>.%
  mutate(.,
    method = substring(type, 1, 6),
    sex    = substring(type, 8, 8),
    age    = substring(type, 9, 12)) %>.%
  select(., -type) ->
  whol
head(whol)

# Traitement des données pour répondre à la question
whol %>.%
  left_join(., pop) %>.% # étape 1 fusion
  filter(., year >= 2000 & year < 2010 & sex == "f" & age %in% c("2534", "3544", "4554")) %>.% # étape 2 filtrage
  group_by(., country, year) %>.% # étape 3 regroupement par pays et année
  summarise(., total = first(population), all_new = sum(new_cases, na.rm = TRUE)) %>.% # étape 4 somme des cas
  mutate(., prevalence = all_new / total) %>.% # étape 5, calcul de la prévalence
  group_by(., country) %>.% # étape 6 regroupement par pays
  summarise(., mean_prev = mean(prevalence, na.rm = TRUE)) %>.% # étape 7 prévalence moyenne
  slice_max(., mean_prev, n = 10) # étape 8 ne garder que les 10 plus élevés

# Difference sum() / fsum()
sum(NA, na.rm = TRUE)
fsum(NA, na.rm = TRUE)

# Même traitement, mais avec fsum()
whol %>.%
  left_join(., pop) %>.% # étape 1 fusion
  filter(., year >= 2000 & year < 2010 & sex == "f" & age %in% c("2534", "3544", "4554")) %>.% # étape 2 filtrage
  group_by(., country, year) %>.% # étape 3 regroupement par pays et année
  summarise(., total = first(population), all_new = fsum(new_cases, na.rm = TRUE)) %>.% # étape 4 somme des cas AVEC fsum()
  mutate(., prevalence = all_new / total) %>.% # étape 5, calcul de la prévalence
  group_by(., country) %>.% # étape 6 regroupement par pays
  summarise(., mean_prev = mean(prevalence, na.rm = TRUE)) %>.% # étape 7 prévalence moyenne
  slice_max(., mean_prev, n = 10) # étape 8 ne garder que les 10 plus élevés

#'
#' ### Normalisation des données {#dbnorm}
#'

library(DBI)
library(duckdb)

# création et connexion à la base de données
drv <- duckdb(dbdir = "duckdb_test.db")
con <- dbConnect(drv)

# Injection de deux data frames comme tables
# notez bien que nous utilisons le data frame long et correctement nettoyé
# `whol` pour notre table "who",... pas l'horrible data frame `who` initial !
dbWriteTable(con, "who", as.data.frame(whol))
dbWriteTable(con, "pop", as.data.frame(pop))

# Chargement du package dm
library(dm)

# Création d'un objet dm qui reprend le schéma de la base
# nous indiquons learn_keys = FALSE car nous n'avons pas encore de clés
# primaires et de toutes façons, {dm} ne les détecte pas depuis DuckDB
dm_from_con(con, learn_keys = FALSE) %>.%
  dm_set_colors(., red = who, darkgreen = pop) -> # Couleurs pour les tables (pour le schéma)
  who_dm

# Graphique du schéma de la base
dm_draw(who_dm, view_type = "all")

# Énumérer les cnadidats possibles pour une clé primaire
dm_enum_pk_candidates(who_dm, pop)

# Création des clés primaires
who_dm %>.%
  dm_add_pk(., pop, c(country, year), force = TRUE) %>.%
  dm_add_pk(., who, c(country, year, method, sex, age), force = TRUE) ->
  who_dm

# Graphique du schéma de la base
dm_draw(who_dm, view_type = "all")

# Ajout d'un clé étrangère de who vers pop
who_dm <- dm_add_fk(who_dm, who, c(country, year), pop)

# Graphique du schéma de la base avec relation entre les tables
dm_draw(who_dm, view_type = "all")

try({
# Vérification de la cardinalité 0 -> n
check_cardinality_0_n(pop, c(country, year), whol, c(country, year))
})

# Années reprises dans pop
unique(pop$year)
# Années reprises dans whol
unique(whol$year)

# Élimination des années antérieures à 1995 dans whol
whol1995 <- filter(whol, year >= 1995)

# Vérification des apys entre les deux tables
whol_countries <- unique(whol1995$country)
pop_countries <- unique(pop$country)
all(whol_countries %in% pop_countries)

# Quels pays diffèrent?
whol_countries[!whol_countries %in% pop_countries]

# Pays dont le nom commence par 'C' dans pop_countries
pop_countries[substring(pop_countries, 1, 1) == "C"]

# Correction des noms de pays mal orthographiés
whol1995$country[whol1995$country == "Cote d'Ivoire"] <- "Côte d'Ivoire"
whol1995$country[whol1995$country == "Curacao"] <- "Curaçao"
# Vérification
all(unique(whol1995$country) %in% pop_countries)

# Vérification de la cardinalité 0 -> n après correction
check_cardinality_0_n(pop, c(country, year), whol1995, c(country, year))

# Effacement de l'ancienne table "whol" et remplacement par la version corrigée
dbRemoveTable(con, "who")
dbWriteTable(con, "who", whol1995)

# Nouvel objet data model
dm_from_con(con, learn_keys = FALSE) %>.%
  dm_set_colors(., red = who, darkgreen = pop) %>.% # Couleurs (optionel)
  dm_add_pk(., pop, c(country, year)) %>.% # Clé primaire pop
  dm_add_pk(., who, c(country, year, method, sex, age)) %>.% # Clé primaire who
  dm_add_fk(., who, c(country, year), pop) -> # Clé étrangère who -> pop
  who_dm

# Graphique du schéma de la base
dm_draw(who_dm, view_type = "all")

# Type de cardinalité entre pop et whol1995
examine_cardinality(pop, c(country, year), whol1995, c(country, year))

# Création du data frame countries
whol1995 %>.%
  select(., country, iso2, iso3) %>.%
  distinct(.) ->
  countries
head(countries)

# Élimination des colonnes iso2 et iso3 de la table who (avec langage SQL)
dbExecute(con, 'ALTER TABLE "who" DROP COLUMN "iso2";')
dbExecute(con, 'ALTER TABLE "who" DROP COLUMN "iso3";')
# Vérification
dbListFields(con, "who")

# Ajout de la table countries
dbWriteTable(con, "countries", countries)
# Vérification
dbListTables(con)

# Nouveau dm qui tient compte de la table countries également
dm_from_con(con, learn_keys = FALSE) %>.%
  dm_set_colors(., red = who, darkgreen = pop, blue = countries) %>.% # Couleurs (optionel)
  dm_add_pk(., pop, c(country, year)) %>.% # Clé primaire pop
  dm_add_pk(., who, c(country, year, method, sex, age)) %>.% # Clé primaire who
  dm_add_pk(., countries, country) %>.% # Clé primaire countries
  dm_add_fk(., who, c(country, year), pop) %>.% # Clé étrangère who -> pop
  dm_add_fk(., pop, country, countries) %>.% # Clé étrangère pop -> countries
  dm_add_fk(., who, country, countries) -> # Clé étrangère who -> countries
  who_dm

# Graphique du schéma de la base
dm_draw(who_dm, view_type = "all")

# Version plus simple de la dm sans relation cyclique
dm_from_con(con, learn_keys = FALSE) %>.%
  dm_set_colors(., red = who, darkgreen = pop, blue = countries) %>.% # Couleurs (optionel)
  dm_add_pk(., pop, c(country, year)) %>.% # Clé primaire pop
  dm_add_pk(., who, c(country, year, method, sex, age)) %>.% # Clé primaire who
  dm_add_pk(., countries, country) %>.% # Clé primaire countries
  dm_add_fk(., who, c(country, year), pop) %>.% # Clé étrangère who -> pop
  # Nous n'utilisons pas cette clé étrangère pour éviter une relation cyclique
  #dm_add_fk(., pop, country, countries) %>.% # Clé étrangère pop -> countries
  dm_add_fk(., who, country, countries) -> # Clé étrangère who -> countries
  who_dm

# Graphique du schéma de la base
dm_draw(who_dm, view_type = "all")

# Validation finale de notre dm
dm_validate(who_dm)

#'
#' ### Requête dans une base de données relationnelle avec {dm} {#dbreq}
#'

# Requête de base de données avec dm
who_dm %>.%
  dm_filter(., who = year >= 2000 & year < 2010 & sex == "f" & age %in% c("2534", "3544", "4554")) %>.% # étape 2 filtrage
  dm_flatten_to_tbl(., who) ->
  who_flat
head(who_flat)

# Notre requête d'eemple en utilisant dm
who_dm %>.% # étape 1 de jointure inutile avec un objet dm
  dm_filter(., who = year >= 2000 & year < 2010 & sex == "f" & age %in% c("2534", "3544", "4554")) %>.% # étape 2 filtrage
  dm_flatten_to_tbl(., who) %>.% # Réduction en une seule table
  # Le traitement ci-dessous est identique à celui dans R,
  # sauf pour la gestion des pays ne rencontrant aucun cas !
  group_by(., country, year) %>.% # étape 3 regroupement par pays et année
  summarise(., total = first(population), all_new = sum(new_cases, na.rm = TRUE)) %>.% # étape 4 somme des cas
  mutate(., prevalence = all_new / total) %>.% # étape 5, calcul de la prévalence
  group_by(., country) %>.% # étape 6 regroupement par pays
  summarise(., mean_prev = mean(prevalence, na.rm = TRUE)) %>.% # étape 7 prévalence moyenne
  # étape supplémentaire nécessaire ici : éliminer les pays où mean_prev est NA
  filter(., !is.na(mean_prev)) %>.% # drop_na() ne fonctionne pas ici
  slice_max(., mean_prev, n = 10) # étape 8 garder les 10 plus élevés

# Déconnexion et nettoyage de la base de données
dbDisconnect(con, shutdown = TRUE)
# Seulement pour effacer définitivement la base de données !
# NE PAS utiliser avec une "vraie" base de données !!!
unlink("duckdb_test.db")

#'
#' ### Positionnement multidimensionnel (MDS) {#mds}
#'

# Mise en place du dialecte SciViews::R avec le module "explore"
SciViews::R("explore", lang = "fr")

# Lecture des données et renommage de la première colonne en station
read("varespec", package = "vegan") %>.%
  srename(., station = .rownames) ->
  veg
veg

# Graphique d'abondances des différentes espèces
veg %>.%
  sselect(., -station) %>.% # Colonne 'station' pas utile ici
  spivot_longer(., everything(), names_to = "espèce", values_to = "couverture") %>.%
  chart(., espèce ~ couverture) +
    geom_boxplot() + # Boites de dispersion
    labs(x = "Espèce", y = "Couverture [%]")

# Même graphqiue, mais après transformation log(x + 1)
veg %>.%
  sselect(., -station) %>.%
  spivot_longer(., everything(), names_to = "espèce", values_to = "couverture") %>.%
  chart(., espèce ~ log1p(couverture)) + # Transformation log(couverture + 1)
    geom_boxplot() +
    labs(x = "Espèce", y = "Couverture [%]")

# Matrice de dissimilarité de Bray-Curtis sur données transformées log(x + 1)
veg %>.%
  sselect(., -station) %>.%
  log1p(.) %>.%
  dissimilarity(., method = "bray") ->
  veg_dist

# Initialisation du générateur de nombres pseudo-aléatoires
set.seed(9)
# MDS métrique sur la matrice de dissimilarité
veg_mds <- mds$metric(veg_dist)

# Graphique de la MDS métrique
chart(veg_mds, labels = veg$station, col = veg$station)

# GOF de la MDS métrique
glance(veg_mds)

# Initialisation du générateur de nombres pseudo-aléatoires
set.seed(295)
# MDS non métrique
veg_nmds <- mds$nonmetric(veg_dist)

# Graphique de la MDS non métrique
chart(veg_nmds, labels = veg$station)

# R^2 de notre MDS non métrique
glance(veg_nmds)

# Diagramme de Shepard pour visualiser la fonction de stress
veg_sh <- shepard(veg_dist, veg_nmds)
chart(veg_sh) +
  labs(x = "Dissimilarité observée", y = "Distance sur l'ordination")
