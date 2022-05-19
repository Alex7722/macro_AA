# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################## LOADING PACKAGES, PATHS AND OBJECTS ####################################--------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

##################### Packages ############################################--------------

cran_list <- c(
  "data.table", "magrittr", "ggnewscale", "igraph","forcats",
  "tidytext", "ggraph", "tidygraph", "leidenAlg", "reshape2", "scales", 
  "ggforce", "directlabels", "patchwork", "DescTools", "DT", "grid", "scico",
  "ggalluvial", "dplyr","gridExtra","readr","tm","stringr", "stringi",
  "RColorBrewer","textstem","tidyr","tidytext","quanteda",
  "plotly","here"
)
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

if (str_detect(getwd(), "goutsmedt")) {
  data_path <- "C:/Users/goutsmedt/Mon Drive/data"
} else {
  if (str_detect(getwd(), "Dropbox")) {
    data_path <- "G:/.shortcut-targets-by-id/1EHqA0pp2uozTykWv0_Stf5xyrvo_dkN5/data"
  } else {
    data_path <- "/projects/data"
  }
}


graph_data_path <- here(data_path, "macro_AA", "4_Networks")
picture_path <- here(data_path, "macro_AA", "6_Pictures")

mypalette <- c("#1969B3", "#01A5D8", "#DA3E61", "#3CB95F", "#E0AF0C", "#E25920", "#6C7FC9", "#DE9493", "#CD242E", "#6F4288", "#B2EEF8", "#7FF6FD", "#FDB8D6", "#8BF9A9", "#FEF34A", "#FEC57D", "#DAEFFB", "#FEE3E1", "#FBB2A7", "#EFD7F2", "#5CAADA", "#37D4F5", "#F5779B", "#62E186", "#FBDA28", "#FB8F4A", "#A4B9EA", "#FAC2C0", "#EB6466", "#AD87BC", "#0B3074", "#00517C", "#871B2A", "#1A6029", "#7C4B05", "#8A260E", "#2E3679", "#793F3F", "#840F14", "#401C56", "#003C65", "#741A09", "#602A2A", "#34134A", "#114A1B", "#27DDD1", "#27DD8D", "#4ADD27", "#D3DD27", "#DDA427", "#DF2935", "#DD27BC", "#BA27DD", "#3227DD", "#2761DD", "#27DDD1")
palette_com_1 <- c("#C7FFFF", "#0E3F84", "#2B8C44", "#FFF953", "#B12C45", "#E7FCFF", "#0190C1", "#956407", "#B64017", "#2E3679", "#925554", "#840F14", "#572F6F")
palette_com_2 <- c("#C7FFFF", "#72BBE1", "#77ED98", "#FFF953", "#FDB8D6", "#E7FCFF", "#25CBF3", "#C57F7E", "#FCAA64", "#A4B9EA", "#F9BAB8", "#EF7776", "#8C5FA1")

######################### Loading WoS data #################################################

######################### Fixing the sub-periods and thresholds ##########################################------------

# Find the time_window
time_window <- 5

nodes_JEL <- readRDS(here(data_path,"macro_AA","3_Corpus_WoS","JEL_matched_corpus_nodes.RDS"))
nodes_JEL <- nodes_JEL[between(Annee_Bibliographique, 1969, 2015)]
# nodes_old_JEL <- readRDS(paste0(data_path, "Old_JEL_matched_corpus_nodes.rds"))
# nodes_JEL <- rbind(nodes_JEL, nodes_old_JEL)
# rm("nodes_old_JEL")

edges_JEL <- readRDS(here(data_path,"macro_AA","3_Corpus_WoS","JEL_matched_corpus_edges.RDS"))
# edges_old_JEL <- readRDS(paste0(data_path, "Old_JEL_matched_corpus_edges.rds"))
# edges_JEL <- rbind(edges_JEL, edges_old_JEL)
# rm("edges_old_JEL")

# Cleaning ID of edges:
# We check for doublons in both new_id and ItemID_Ref

check_id <- edges_JEL[ItemID_Ref != 0 & New_id2 != 0,
                     c("ItemID_Ref", "New_id2")] %>%
  unique() %>%
  as.data.table()

null_ItemID_Ref <- edges_JEL[ItemID_Ref == 0 & New_id2 != 0,
                             c("ItemID_Ref", "New_id2")] %>%
  unique() %>%
  select(New_id2) %>%
  as.data.table()

test_1 <- check_id[New_id2 %in% null_ItemID_Ref]
if(nrow(test_1) == 0){
  message("New_id2 associated with a null ItemID_Ref are not associated to any other positive ItemID_Ref")
} else{
  message("Warning: New_id2 associated with a null ItemID_Ref are also associated to positive ItemID_Ref")
}

check_id <- check_id %>%
  mutate(doublons = duplicated(ItemID_Ref)) %>%
  filter(doublons == FALSE) %>%
  mutate(new_id = New_id2)

edges_JEL <- edges_JEL %>%
  left_join(check_id[, c("ItemID_Ref","new_id")]) %>%
  mutate(new_id = ifelse(is.na(new_id), New_id2, new_id))

# corpus years
first_year <- nodes_JEL[order(Annee_Bibliographique), head(.SD, 1)]$Annee_Bibliographique
last_year <- (nodes_JEL[order(-Annee_Bibliographique), head(.SD, 1)]$Annee_Bibliographique - time_window + 1) # +1 to get the very last year in the window / -1 because 2016 is incomplete
all_years <- first_year:last_year

