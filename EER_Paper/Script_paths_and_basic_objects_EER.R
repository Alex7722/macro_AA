# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################## LOADING PACKAGES, PATHS AND OBJECTS ####################################--------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

##################### Packages ############################################--------------

package_list <- c(
  "here", "fs",
  "data.table", "tidyverse", "furrr", "ggnewscale", "igraph",
  "quanteda", "tm", "tidytext", "ggraph", "tidygraph",
  "leidenAlg", "reshape2", "scales", "RMySQL", "stringi",
  "ggforce", "directlabels", "patchwork", "DescTools", "DT",
  "grid", "ggdendro", "readtext", "pander", "RColorBrewer",
  "scico", "plotly", "crosstalk", "widgetframe", "sigmajs",
  "ggdark", "topicmodels", "ggrepel", "stm", "huge",
  "spacyr", "htmlwidgets"
)
for (p in package_list) {
  if (p %in% installed.packages() == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}

github_list <- c(
  "agoutsmedt/biblionetwork",
  "agoutsmedt/networkflow",
  "mikajoh/tidystm",
  "ParkerICI/vite"
)
for (p in github_list) {
  if (gsub(".*/", "", p) %in% installed.packages() == FALSE) {
    devtools::install_github(p)
  }
  library(gsub(".*/", "", p), character.only = TRUE)
}


######################### Paths ##########################################------------

<<<<<<< HEAD
if (stringr::str_detect(getwd(), "MEGA")) {
  data_path <- path.expand("~/data/macro_AA")
=======
if (stringr::str_detect(getwd(), "goutsmedt")) {
  data_path <- path.expand("~/data/macro_AA/")
>>>>>>> c38db255a4e07bb9ff64a836d20e55237a43c65b
} else {
  data_path <- "/projects/data/macro_AA"
}

picture_path <- here("EER_Paper", "Pictures")
eer_data <- here(data_path, "EER")
boards_path <- here(eer_data, "editorial_boards")
macro_data <- here(data_path, "Corpus_Econlit_Matched_WoS")

######################### data ##########################################------------

# Loading Macro articles
nodes_JEL <- readRDS(paste0(macro_data, "JEL_matched_corpus_nodes.rds"))
nodes_old_JEL <- readRDS(paste0(macro_data, "Old_JEL_matched_corpus_nodes.rds"))
nodes_JEL <- rbind(nodes_JEL, nodes_old_JEL)
rm("nodes_old_JEL")

edges_JEL <- readRDS(paste0(macro_data, "JEL_matched_corpus_edges.rds"))
edges_old_JEL <- readRDS(paste0(macro_data, "Old_JEL_matched_corpus_edges.rds"))
edges_JEL <- rbind(edges_JEL, edges_old_JEL)
rm("edges_old_JEL")

authors_JEL <- readRDS(paste0(macro_data, "JEL_matched_corpus_authors.rds"))
authors_old_JEL <- readRDS(paste0(macro_data, "Old_JEL_matched_corpus_authors.rds"))
authors_JEL <- rbind(authors_JEL, authors_old_JEL)

# Cleaning ID of edges:
# We check for doublons in both new_id and ItemID_Ref

check_id <- edges_JEL[
  ItemID_Ref != 0 & New_id2 != 0,
  c("ItemID_Ref", "New_id2")
] %>%
  unique() %>%
  as.data.table()

null_ItemID_Ref <- edges_JEL[
  ItemID_Ref == 0 & New_id2 != 0,
  c("ItemID_Ref", "New_id2")
] %>%
  unique() %>%
  select(New_id2) %>%
  as.data.table()

test_1 <- check_id[New_id2 %in% null_ItemID_Ref]
if (nrow(test_1) == 0) {
  message("New_id2 associated with a null ItemID_Ref are not associated to any other positive ItemID_Ref")
} else {
  message("Warning: New_id2 associated with a null ItemID_Ref are also associated to positive ItemID_Ref")
}

check_id <- check_id %>%
  mutate(doublons = duplicated(ItemID_Ref)) %>%
  filter(doublons == FALSE) %>%
  mutate(new_id = New_id2)

edges_JEL <- edges_JEL %>%
  left_join(check_id[, c("ItemID_Ref", "new_id")]) %>%
  mutate(new_id = ifelse(is.na(new_id), New_id2, new_id))

institutions_info_JEL <- fread(paste0(macro_data, "Macro_AA_Institutions_Cleaned.csv"), quote = "", fill = TRUE) %>% data.table()

ref_info_JEL <- readRDS(paste0(macro_data, "JEL_matched_corpus_references_info.rds"))
ref_info_old_JEL <- readRDS(paste0(macro_data, "Old_JEL_matched_corpus_references_info.rds"))
ref_info_JEL <- unique(rbind(ref_info_JEL, ref_info_old_JEL))

# keeping only refs with a title and a ESpecialite, then removing doublons
ref_info_JEL <- unique(ref_info_JEL[Titre != "NA" & ESpecialite != "NA", c("New_id2", "Titre", "ESpecialite")])
doublons <- which(duplicated(ref_info_JEL$New_id2))

if (length(doublons) != 0) {
  ref_info_JEL <- ref_info_JEL[-doublons]
}


# Adding info to references
edges_JEL <- merge(unique(edges_JEL), unique(ref_info_JEL[Titre != "NA" & ESpecialite != "NA", c("New_id2", "Titre", "ESpecialite")]), by = "New_id2", all.x = TRUE)
edges_JEL <- merge(edges_JEL, unique(nodes_JEL[, c("ID_Art", "Annee_Bibliographique")]), by = "ID_Art", all.x = TRUE)

# Adding institutions to Articles
institutions_info_JEL$ID_Art <- as.integer(institutions_info_JEL$ID_Art)
institutions_info_JEL$Ordre <- as.integer(institutions_info_JEL$Ordre)
authors_JEL <- merge(authors_JEL, institutions_info_JEL, by = c("ID_Art", "Ordre"), all.x = TRUE)

# removing useless files

rm(ref_info_JEL)
rm(institutions_info_JEL)

# passing name column to upper letters
edges_JEL <- edges_JEL[, Nom := toupper(Nom)]
authors_JEL <- authors_JEL[, Nom := toupper(Nom)]
nodes_JEL <- nodes_JEL[, Nom := toupper(Nom)]

# loading palette
mypalette <- c("#1969B3", "#01A5D8", "#DA3E61", "#3CB95F", "#E0AF0C", "#E25920", "#6C7FC9", "#DE9493", "#CD242E", "#6F4288", "#B2EEF8", "#7FF6FD", "#FDB8D6", "#8BF9A9", "#FEF34A", "#FEC57D", "#DAEFFB", "#FEE3E1", "#FBB2A7", "#EFD7F2", "#5CAADA", "#37D4F5", "#F5779B", "#62E186", "#FBDA28", "#FB8F4A", "#A4B9EA", "#FAC2C0", "#EB6466", "#AD87BC", "#0B3074", "#00517C", "#871B2A", "#1A6029", "#7C4B05", "#8A260E", "#2E3679", "#793F3F", "#840F14", "#401C56", "#003C65", "#741A09", "#602A2A", "#34134A", "#114A1B", "#27DDD1", "#27DD8D", "#4ADD27", "#D3DD27", "#DDA427", "#DF2935", "#DD27BC", "#BA27DD", "#3227DD", "#2761DD", "#27DDD1")
