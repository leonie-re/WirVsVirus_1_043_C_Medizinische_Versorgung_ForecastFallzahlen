# ----setup----
## Save package names as a vector of strings
pkgs <-
  c("tidyverse", "tidyr", "dplyr", "matchingMarkets", "lubridate", "stringr", "ggplot2", "rmarkdown", "knitr"
  )

## Install uninstalled packages
lapply(pkgs[!(pkgs %in% installed.packages())], install.packages)

## Load all packages to library and adjust options
lapply(pkgs, library, character.only = TRUE)


rm(list=ls())
