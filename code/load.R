if(!suppressWarnings(require("pacman", character.only = TRUE))) {
  install.packages("pacman", repos = "https://cran.r-project.org/")
}

if (!require("devtools")) install.packages("devtools")

# install packages
pkg_list <- c("tidyverse", "splitstackshape",
              "mltools", "googlesheets4", "pheatmap", "dendextend",
              "kableExtra", "DT", "patchwork", "gtsummary",
              "cluster", "Rtsne",
              "FactoMineR", "factoextra", "missMDA", "plotly")
pacman::p_load(pkg_list, character.only = TRUE)
