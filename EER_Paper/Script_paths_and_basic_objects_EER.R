# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################## LOADING PACKAGES, PATHS AND OBJECTS ####################################--------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

##################### Packages ############################################--------------

package_list <- c(
  "data.table", "tidyverse", "furrr", "ggnewscale", "igraph",
  "quanteda", "tm", "tidytext", "ggraph", "tidygraph",
  "leidenAlg", "reshape2", "scales","RMySQL", "stringi",
  "ggforce", "directlabels", "patchwork", "DescTools", "DT", 
  "grid", "ggdendro", "readtext", "pander","RColorBrewer",
  "scico","plotly","crosstalk","widgetframe","sigmajs",
  "ggdark","topicmodels","ggrepel","stm", "tidystm", "huge")
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

if(stringr::str_detect(getwd(), "MEGA")) {
  boards_path <- path.expand("~/MEGA/Research/R/projets/data/macro_AA/EER/editorial_boards/")
  eer_data <- path.expand("~/MEGA/Research/R/projets/data/macro_AA/EER/Corpus_EER/")
  picture_path <- path.expand("~/MEGA/Research/R/projets/macro_AA/EER_Paper/Pictures/")
  data_path <- path.expand("~/MEGA/Research/R/projets/data/macro_AA/")
} else {
boards_path <- "/projects/data/macro_AA/EER/editorial_boards/"
eer_data <- "/projects/data/macro_AA/EER/Corpus_EER/"
picture_path <- "/home/aurelien/macro_AA/EER_Paper/Pictures/"
data_path <- "/projects/data/macro_AA/"
}

#eer_nodes <- fread(paste0(eer_data,"EER_NODES_XP.csv")) %>% as.data.table()
#eer_ref <- fread(paste0(eer_data,"EER_REFS_XP.csv")) %>% as.data.table()
#eer_inst <- fread(paste0(eer_data,"EER_INST_XP.csv")) %>% as.data.table()
#eer_aut <- fread(paste0(eer_data,"EER_AUT_XP.csv")) %>% as.data.table()

nodes_JEL <- readRDS(paste0(data_path, "Corpus_Econlit_Matched_WoS/JEL_matched_corpus_nodes.rds"))
nodes_old_JEL <- readRDS(paste0(data_path, "Corpus_Econlit_Matched_WoS/Old_JEL_matched_corpus_nodes.rds"))
nodes_JEL <- rbind(nodes_JEL, nodes_old_JEL)
rm("nodes_old_JEL")

edges_JEL <- readRDS(paste0(data_path, "Corpus_Econlit_Matched_WoS/JEL_matched_corpus_edges.rds"))
edges_old_JEL <- readRDS(paste0(data_path, "Corpus_Econlit_Matched_WoS/Old_JEL_matched_corpus_edges.rds"))
edges_JEL <- rbind(edges_JEL, edges_old_JEL)
rm("edges_old_JEL")

Institutions <- readRDS(paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Institutions.rds"))

mypalette <- c("#1969B3", "#01A5D8", "#DA3E61", "#3CB95F", "#E0AF0C", "#E25920", "#6C7FC9", "#DE9493", "#CD242E", "#6F4288", "#B2EEF8", "#7FF6FD", "#FDB8D6", "#8BF9A9", "#FEF34A", "#FEC57D", "#DAEFFB", "#FEE3E1", "#FBB2A7", "#EFD7F2", "#5CAADA", "#37D4F5", "#F5779B", "#62E186", "#FBDA28", "#FB8F4A", "#A4B9EA", "#FAC2C0", "#EB6466", "#AD87BC", "#0B3074", "#00517C", "#871B2A", "#1A6029", "#7C4B05", "#8A260E", "#2E3679", "#793F3F", "#840F14", "#401C56", "#003C65", "#741A09", "#602A2A", "#34134A", "#114A1B", "#27DDD1", "#27DD8D", "#4ADD27", "#D3DD27", "#DDA427", "#DF2935", "#DD27BC", "#BA27DD", "#3227DD", "#2761DD", "#27DDD1")


########################## Fixing the time_window for all the project ########################

time_window <- 7
