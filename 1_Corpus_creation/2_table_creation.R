# source(here::here("Script_paths_and_basic_objects_EER.R"))
require(here)
require(xml2)
require(data.table)
require(dplyr)
require(tidyr)
require(stringr)
require(arrow)

`%notin%` <- Negate(`%in%`)
if (str_detect(getwd(), "goutsmedt")) {
  data_path <- "C:/Users/goutsmedt/Mon Drive/data"
} else {
  if (str_detect(getwd(), "Dropbox")) {
    data_path <- "G:/.shortcut-targets-by-id/1EHqA0pp2uozTykWv0_Stf5xyrvo_dkN5/data"
  } else {
    data_path <- "/projects/data/macro_AA"
  }
}

#  List ID_Art
base_corpus <-readRDS(here(data_path, "macro_AA","2_Matched_data","Econlit_matched_ID_Art_macroAA.RDS"))

# Journals
revues <- readRDS(here(data_path, "macro_AA","OST_generic_data", "revues.RDS"))

# Disciplines
disciplines <- readRDS(here(data_path, "macro_AA","OST_generic_data", "disciplines.RDS"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Authors ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
JEL_matched_corpus_authors <- readRDS(here(data_path, "macro_AA","OST_generic_data", "all_aut.RDS")) %>% .[ID_Art %in% base_corpus$ID_Art]

saveRDS(JEL_matched_corpus_authors, here(data_path, "macro_AA","3_Corpus_WoS", "JEL_matched_corpus_authors.rds"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Nodes ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
JEL_matched_corpus_nodes <- read_parquet(here(data_path, "macro_AA","OST_generic_data", "all_art.parquet"))  %>% .[ID_Art %in% base_corpus$ID_Art,.(ID_Art, ItemID_Ref, Titre, Annee_Bibliographique, Code_Revue)]

# 83 758 nodes
JEL_matched_corpus_nodes <- merge(JEL_matched_corpus_nodes, JEL_matched_corpus_authors[Ordre == 1, .SD, .SDcols = !c("Annee_Bibliographique")], by = "ID_Art", all.x = TRUE)

# Label column
JEL_matched_corpus_nodes <- JEL_matched_corpus_nodes[, name_short := gsub("-.*", "", Nom)]
JEL_matched_corpus_nodes$name_short <- toupper(JEL_matched_corpus_nodes$name_short)
JEL_matched_corpus_nodes <- JEL_matched_corpus_nodes[, Label := paste0(name_short, ",", Annee_Bibliographique)]
JEL_matched_corpus_nodes[, c("name_short") := NULL]

# Disciplines and journals
JEL_matched_corpus_nodes <- merge(JEL_matched_corpus_nodes, revues, by = "Code_Revue", all.x = TRUE)
JEL_matched_corpus_nodes[, Revue := sub("\r", "", Revue)]
JEL_matched_corpus_nodes <- merge(JEL_matched_corpus_nodes, disciplines, by = "Code_Discipline", all.x = TRUE)

saveRDS(JEL_matched_corpus_nodes, here(data_path, "macro_AA","3_Corpus_WoS", "JEL_matched_corpus_nodes.rds"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Edges ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
JEL_matched_corpus_edges <- read_parquet(here(data_path, "macro_AA","OST_generic_data", "all_ref.parquet")) %>% .[ID_Art %in% base_corpus$ID_Art]

saveRDS(JEL_matched_corpus_edges, here(data_path, "macro_AA","3_Corpus_WoS", "JEL_matched_corpus_edges.rds"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Refs info ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
JEL_matched_corpus_references <- read_parquet(here(data_path, "macro_AA","OST_generic_data", "all_art.parquet"))  %>% .[ItemID_Ref %in% JEL_matched_corpus_edges[ItemID_Ref!=0]$ItemID_Ref, .(ItemID_Ref, Titre, Code_Revue)]

JEL_matched_corpus_references <- JEL_matched_corpus_references[ItemID_Ref %in% JEL_matched_corpus_references[,.N,ItemID_Ref][N==1]$ItemID_Ref] # keep only unambiguous ItemID_Ref

JEL_matched_corpus_references <- merge(JEL_matched_corpus_references, revues[, .(Code_Revue, Code_Discipline, Revue)], by = "Code_Revue", all.x = TRUE)
JEL_matched_corpus_references[, Revue := sub("\r", "", Revue)]
JEL_matched_corpus_references <- merge(JEL_matched_corpus_references, disciplines, by = "Code_Discipline", all.x = TRUE)

saveRDS(JEL_matched_corpus_references, here(data_path, "macro_AA","3_Corpus_WoS", "JEL_matched_corpus_references_info.rds"))
