# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################## LOADING PACKAGES, PATHS AND OBJECTS ####################################--------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

##################### Packages ############################################--------------

package_list <- c(
  "data.table", "magrittr", "tidyverse", "ggnewscale", "igraph",
  "quanteda", "tidytext", "ggraph", "tidygraph", "ggrepel",
  "leidenAlg", "reshape2", "scales","RMySQL",
  "ggforce", "directlabels", "patchwork", "DescTools", "DT", 
  "grid", "ggdendro", "knitr", "readtext", "pander","RColorBrewer")
for (p in package_list) {
  if (p %in% installed.packages() == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}

github_list <- c("ParkerICI/vite", "agoutsmedt/biblionetwork", "agoutsmedt/networkflow")
for (p in github_list) {
  if (gsub(".*/", "", p) %in% installed.packages() == FALSE) {
    devtools::install_github(p)
  }
  library(gsub(".*/", "", p), character.only = TRUE)
}

# py_install("python-igraph")
# py_install("leidenalg", forge = TRUE)

######################### Paths and data ##########################################------------

boards_path <- "/projects/data/macro_AA/EER/editorial_boards/"
produced_data_path <- "/projects/data/macro_AA/EER/"
picture_path <- "/home/aurelien/macro_AA/EER_Paper/Pictures/"
