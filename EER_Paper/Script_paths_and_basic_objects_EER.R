# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################## LOADING PACKAGES, PATHS AND OBJECTS ####################################--------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

##################### Packages ############################################--------------

package_list <- c(
  "data.table", "tidyverse", "ggnewscale", "igraph",
  "quanteda", "tidytext", "ggraph", "tidygraph",
  "leidenAlg", "reshape2", "scales","RMySQL",
  "ggforce", "directlabels", "patchwork", "DescTools", "DT", 
  "grid", "ggdendro", "readtext", "pander","RColorBrewer")
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

######################### Paths and data ##########################################------------

boards_path <- "/projects/data/macro_AA/EER/editorial_boards/"
eer_data <- "/projects/data/macro_AA/EER/Corpus_EER/"
picture_path <- "/home/aurelien/macro_AA/EER_Paper/Pictures/"

eer_nodes <- fread(paste0(eer_data,"EER_NODES_XP.csv")) %>% as.data.table()
eer_ref <- fread(paste0(eer_data,"EER_REFS_XP.csv")) %>% as.data.table()
eer_inst <- fread(paste0(eer_data,"EER_INST_XP.csv")) %>% as.data.table()
eer_aut <- fread(paste0(eer_data,"EER_AUT_XP.csv")) %>% as.data.table()
