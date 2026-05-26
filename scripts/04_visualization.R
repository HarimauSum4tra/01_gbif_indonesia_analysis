
rm(list = ls())
Sys.Date()
Sys.timezone()

packages_needed <- c(
  # Data wrangling
  "dplyr", "tidyr", "tidyverse", "readr", "data.table",
  "lubridate", "readxl", "purrr", "stringr", "hms",
  # Spasial
  "sp", "sf", "mapview", "maptools",
  # Visualisasi
  "ggplot2", "ggrepel", "patchwork", "tidyquant",
  # Utilitas
  "here", "fs", "rstudioapi", "stats", "rsconnect", "renv",
  # Keanekaragaman & Ekologi
  "vegan", "BiodiversityR", "moments", "qqplotr",
  "BIOMASS", "Distance",
  # Konservasi & Taksonomi
  "redlistr", "iucnredlist", "rcites", "taxize"
)

pk_to_install <- packages_needed[!(packages_needed %in% rownames(installed.packages()))]
if (length(pk_to_install) > 0) {
  install.packages(pk_to_install, repos = "http://cran.r-project.org")
}

lapply(packages_needed, require, character.only = TRUE)
