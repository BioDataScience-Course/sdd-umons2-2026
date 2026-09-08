# This bookdown in its version 2025 needs the following R packages
#  R 4.4.3
# Use the following to list all dependencies:
#imports <- unique(c("bookdown", "learnitdown", "SciViews",
#  attachment::att_from_rmds(".")))
#attachment::att_to_desc_from_is(path.d = "DESCRIPTION",
#  imports = imports, suggests = NULL)

# From CRAN
install.packages(c("knitr", "bookdown", # The bases!
  "gdtools", "svglite", # SVG graphs
  "htmltools", "vembedr", # To embed videos easily
  "devtools", # To install packages from Github
  "ape",
  "broom",
  "ca",
  "car",
  "dbplyr",
  "dplyr",
  "factoextra",
  "FactoMineR",
  "fastcluster",
  "flashClust",
  "GGally",
  "gganimate",
  "ggdendro",
  "ggfortify",
  "ggrepel",
  "grid",
  "gridExtra",
  "iterators",
  "kohonen",
  "MASS",
  "mlbench",
  "modelr",
  "multcomp",
  "naniar",
  "plotly",
  "purrr",
  "RColorBrewer",
  "remotes",
  "rgl",
  "rlang",
  "RSQLite",
  "scales",
  "sessioninfo",
  "skimr",
  "tibble",
  "tidyverse",
  "transformr",
  "UsingR",
  "vegan",
  "viridis"
))

# From GitHub (latest devel version)
remotes::install_github("SciViews/svFast")
remotes::install_github("SciViews/svTidy")
remotes::install_github("SciViews/helpai")
remotes::install_github("SciViews/svMisc")
remotes::install_github("SciViews/svBase")
remotes::install_github("SciViews/svFlow")
remotes::install_github("SciViews/data.io")
remotes::install_github("SciViews/chart")
remotes::install_github("SciViews/tabularise")
remotes::install_github("SciViews/exploreit")
remotes::install_github("SciViews/modelit")
remotes::install_github("SciViews/inferit")
remotes::install_github("SciViews/SciViews")
remotes::install_github("SciViews/exploreit")
remotes::install_github("SciViews/learnitdown")
