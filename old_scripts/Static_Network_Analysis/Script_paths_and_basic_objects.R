# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################## LOADING PACKAGES, PATHS AND OBJECTS ####################################--------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

##################### Packages ############################################--------------

cran_list <- c(
  "data.table", "tidyverse", "ggnewscale", "igraph","ggplot2",
  "tm", "quanteda", "tidytext", "ggraph", "tidygraph", "vite",
  "reticulate", "leidenAlg", "reshape2", "scales", "scico",
  "ggforce", "directlabels", "patchwork", "DescTools", "DT",
  "grid", "ggdendro","ggalluvial","ggrepel","leiden")
for (p in cran_list) {
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

data_path <- "/projects/data/macro_AA/Corpus_Econlit_Matched_WoS/"
graph_data_path <- "/projects/data/macro_AA/Graphs/"
picture_path <- "~/macro_AA/Static_Network_Analysis/Pictures/"

mypalette <- c("#1969B3", "#01A5D8", "#DA3E61", "#3CB95F", "#E0AF0C", "#E25920", "#6C7FC9", "#DE9493", "#CD242E", "#6F4288", "#B2EEF8", "#7FF6FD", "#FDB8D6", "#8BF9A9", "#FEF34A", "#FEC57D", "#DAEFFB", "#FEE3E1", "#FBB2A7", "#EFD7F2", "#5CAADA", "#37D4F5", "#F5779B", "#62E186", "#FBDA28", "#FB8F4A", "#A4B9EA", "#FAC2C0", "#EB6466", "#AD87BC", "#0B3074", "#00517C", "#871B2A", "#1A6029", "#7C4B05", "#8A260E", "#2E3679", "#793F3F", "#840F14", "#401C56", "#003C65", "#741A09", "#602A2A", "#34134A", "#114A1B", "#27DDD1", "#27DD8D", "#4ADD27", "#D3DD27", "#DDA427", "#DF2935", "#DD27BC", "#BA27DD", "#3227DD", "#2761DD", "#27DDD1")
palette_com_1 <- c("#C7FFFF", "#0E3F84", "#2B8C44", "#FFF953", "#B12C45", "#E7FCFF", "#0190C1", "#956407", "#B64017", "#2E3679", "#925554", "#840F14", "#572F6F")
palette_com_2 <- c("#C7FFFF", "#72BBE1", "#77ED98", "#FFF953", "#FDB8D6", "#E7FCFF", "#25CBF3", "#C57F7E", "#FCAA64", "#A4B9EA", "#F9BAB8", "#EF7776", "#8C5FA1")
######################### Fixing the sub-periods and thresholds ##########################################------------

start_date <- c(1970, 1977, 1984, 1991, 1997, 2003)
end_date <- c(1976, 1983, 1990, 1996, 2002, 2008)

######################### Integrating community names and creating their colors #########################-----
community_names <- fread(paste0(graph_data_path, "Community_names.csv")) %>% data.table()
community_names <- community_names[, Com_ID := gsub("\"\"", "", Com_ID)]

# Adding "first rank" colors to the data
color_com <- data.table(
  "Titre_provisoire" = sort(unique(community_names$Titre_provisoire)),
  "Color_Com_1" = palette_com_1[1:length(unique(community_names$Titre_provisoire))],
  "Color_Com_2" = palette_com_2[1:length(unique(community_names$Titre_provisoire))]
)
community_names <- merge(community_names, color_com, by = "Titre_provisoire")

# Manipulating the data table to have "second rank" colors
color_com <- unique(community_names[order(Titre_provisoire, Titre_provisoire_long), c("Titre_provisoire", "Titre_provisoire_long", "Color_Com_1", "Color_Com_2")])
color_com <- color_com[, `:=`(Order = 1:.N, total_titles = .N), by = list(Titre_provisoire)][, alpha_index := Order / total_titles]
color_com <- color_com[, Color_Com := MixColor(color_com$Color_Com_1, color_com$Color_Com_2, amount1 = color_com$alpha_index)]

community_names <- merge(community_names, color_com[, c("Titre_provisoire_long", "Color_Com")], by = "Titre_provisoire_long")
community_names$Titre_provisoire_long <- gsub("\\\\n", "\\\n", community_names$Titre_provisoire_long)
