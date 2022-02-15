# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MATCHING JEL WITH WOS ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

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
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### 0 Making JEL Corpus (no need to run) ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

from_xml_to_df <- function(x ### x=df of edges
) {
  econlit <- read_xml(paste0("", x, ".xml", ""))
  
  # This line create a kind of list of all entries
  econlit <- xml_find_all(econlit, "//rec")
  # Extracting data
  title <- xml_text(xml_find_first(econlit, ".//atl"))
  year <- data.table(xml_text(xml_find_first(econlit, ".//dt")))
  year[, V1 := substr(V1, 1, 4)]
  journal <- xml_text(xml_find_first(econlit, ".//jtl"))
  vol <- xml_text(xml_find_first(econlit, ".//vid"))
  no <- xml_text(xml_find_first(econlit, ".//iid"))
  pubType <- xml_text(xml_find_first(econlit, ".//pubtype"))
  pages <- xml_text(xml_find_first(econlit, ".//pages"))
  author <- xml_text(xml_find_first(econlit, ".//au"))

  macro1 <- data.table(Title = title, Year = year, Journal = journal, Vol = vol, No = no, Pages = pages, PubType = pubType, Author=author)
  
  # Keep only articles
  macro1 <- macro1[PubType == "Journal Article"]
  return(macro1)
}

# Macro


macro1 <- from_xml_to_df(here(data_path, "macro_AA","Corpus_Econlit", "macro1"))
macro2 <- from_xml_to_df(here(data_path, "macro_AA","Corpus_Econlit", "macro2"))
macro3 <- from_xml_to_df(here(data_path, "macro_AA","Corpus_Econlit", "macro3"))
macro4 <- from_xml_to_df(here(data_path, "macro_AA","Corpus_Econlit", "macro4"))
macro5 <- from_xml_to_df(here(data_path, "macro_AA","Corpus_Econlit", "macro5"))
macro6 <- from_xml_to_df(here(data_path, "macro_AA","Corpus_Econlit", "macro6"))
macro7 <- from_xml_to_df(here(data_path, "macro_AA","Corpus_Econlit", "macro7"))
macro8 <- from_xml_to_df(here(data_path, "macro_AA","Corpus_Econlit", "macro8"))
macro9 <- from_xml_to_df(here(data_path, "macro_AA","Corpus_Econlit", "F3 JEL 1991-2012"))
macro10 <- from_xml_to_df(here(data_path, "macro_AA","Corpus_Econlit", "F3 JEL 2013-2016"))

dt_JEL_Articles <- rbind(macro1, macro2, macro3, macro4, macro5, macro6, macro7, macro8, macro9, macro10)
rm(macro1, macro2, macro3, macro4, macro5, macro6, macro7, macro8, macro9, macro10)

old_macro1 <- from_xml_to_df(here(data_path, "macro_AA","Corpus_Econlit", "Old_JEL_1969-1985"))
old_macro2 <- from_xml_to_df(here(data_path, "macro_AA","Corpus_Econlit", "Old_JEL_1986-1990"))

dt_Old_JEL_Articles <- rbind(old_macro1, old_macro2)
rm(old_macro1, old_macro2)
gc()

saveRDS(dt_JEL_Articles, here(data_path, "macro_AA","Corpus_Econlit", "dt_JEL_Articles.rds"))
saveRDS(dt_Old_JEL_Articles, here(data_path, "macro_AA","Corpus_Econlit", "dt_Old_JEL_Articles.rds"))


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### 1 Prepping corpora ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#################### 1.1 Prepping WoS ####################
# Articles
all_art <- read_parquet(here(data_path, "macro_AA","OST_generic_data", "all_art.parquet"))
# Authors
all_aut <- readRDS(here(data_path, "macro_AA","OST_generic_data", "all_aut.RDS"))

# Journals
revues <- readRDS(here(data_path, "macro_AA","OST_generic_data", "revues.RDS"))
revues[,Revue:=sub("\r", "", Revue)] # remove some weird characters in journal name
revues[,Revue:=toupper(Revue)]
revues[,Revue:=str_remove_all(Revue,"\\sTHE\\s*|\\sAND\\s*")]
revues[,Revue:=str_remove_all(Revue,"\\s")]
revues[,Revue:=str_remove_all(Revue,"[:punct:]")]
# Issues
issueID <- readRDS(here(data_path, "macro_AA","OST_generic_data", "revueID.RDS")) %>% data.table()

# Get IssueID for matching
all_art <- merge(all_art, issueID[, .(IssueID, Volume)], by = "IssueID", all.x = TRUE)
all_art[Volume=="NULL",Volume:=NA]
# Get name of journal for matching
all_art <- merge(all_art, revues, by = "Code_Revue", all.x = TRUE)

# Authors
first_aut <- all_aut[Ordre==1, .(Nom, ID_Art)]
# clean to get only name and first letter after
first_aut[,Prenom:=toupper(str_extract(Nom, "(?<=-).*"))]
first_aut[,Prenom:=str_sub(Prenom,1,1)]
first_aut[,Prenom:=str_remove_all(Prenom,"\\s")]
first_aut[,Prenom:=str_remove_all(Prenom,"[:punct:]")]
first_aut[,Nom:=toupper(str_remove(Nom, "-.*"))]
first_aut[,Nom:=str_remove_all(Nom,"\\s")]
first_aut[,Nom:=str_remove_all(Nom,"[:punct:]")]
first_aut[,Nom_Prenom:=paste(Nom,Prenom,sep = "-")]
# merge
all_art <- merge(all_art, first_aut, by = "ID_Art", all.x = TRUE)


# Titre
all_art[,Titre:=toupper(Titre)]
all_art[,Titre:=str_remove_all(Titre,"\\s")]
all_art[,Titre:=str_remove_all(Titre,"[:punct:]")]
all_art[,Titre:=str_remove_all(Titre,"\\n")]



#################### 1.2 Prepping Econlit ####################
# Articles
new_jel <- readRDS(here(data_path, "macro_AA","Corpus_Econlit", "dt_JEL_Articles.RDS"))
old_JEL <- readRDS(here(data_path, "macro_AA","Corpus_Econlit", "dt_Old_JEL_Articles.RDS"))
econlit <- rbind(new_jel, old_JEL) %>% as.data.table()
rm(new_jel,old_JEL)
gc()

ls(all_art)

# Page Format
econlit[,Page_Debut:=str_remove(Pages, "-.*")]
econlit[,Page_Fin:=toupper(str_extract(Pages, "(?<=-).*"))]

# Volume
econlit <- econlit %>% rename(Volume = Vol)

# Year
econlit <- econlit %>% rename(Annee_Bibliographique = Year.V1)

# Author
econlit[,Prenom:=toupper(str_extract(Author, "(?<=,).*"))]
econlit[,Prenom:=str_remove_all(Prenom,"\\s")]
econlit[,Prenom:=str_sub(Prenom,1,1)]
econlit[,Prenom:=str_remove_all(Prenom,"[:punct:]")]

econlit[,Nom:=toupper(str_remove(Author, ",.*"))]
econlit[,Nom:=str_remove_all(Nom,"\\s")]
econlit[,Nom:=str_remove_all(Nom,"[:punct:]")] # most special characters are removed in Wos, so we do the same (O'Hara is OHARA, and - too)
econlit[,Nom_Prenom:=paste(Nom,Prenom,sep = "-")]

# Journal
econlit <- econlit %>% rename(Revue = Journal)
econlit[,Revue:=toupper(Revue)]
econlit[,Revue:=str_remove_all(Revue,"\\sTHE\\s*|\\sAND\\s*")] # remove THE and AND, Big source of confusion
econlit[,Revue:=str_remove_all(Revue,"\\s")]
econlit[,Revue:=str_remove_all(Revue,"[:punct:]")]

# Titre
econlit <- econlit %>% rename(Titre = Title)
econlit[,Titre:=toupper(Titre)]
econlit[,Titre:=str_remove_all(Titre,"\\s")]
econlit[,Titre:=str_remove_all(Titre,"[:punct:]")]
econlit[,Titre:=str_remove_all(Titre,"\\n")]

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### 2 Matching ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

match_get_id <- function(df_main=all_art,
                         df_match=econlit, 
                         variable_list_for_match=c("Revue","Volume","Page_Debut"), 
                         IDs_to_get="ID_Art"){

  df_main <- df_main %>% filter_at(vars(variable_list_for_match),all_vars(!is.na(.)))
  df_match <- df_match %>% filter_at(vars(variable_list_for_match),all_vars(!is.na(.)))
  
  match_variable_name <- paste0(variable_list_for_match,collapse = "_")
  
  df1 <- df_main %>% unite(!!match_variable_name, variable_list_for_match) %>% select(all_of(match_variable_name),all_of(IDs_to_get))
  df2 <- df_match %>% unite(!!match_variable_name, variable_list_for_match) %>% select(all_of(match_variable_name))
  list_ids <- df1[df1[[match_variable_name]] %in% df2[[match_variable_name]]] %>% select(all_of(match_variable_name),all_of(IDs_to_get))
  return(list_ids)
}

match1 <- c("Revue","Volume","Page_Debut")
match2 <- c("Annee_Bibliographique", "Revue","Page_Debut","Page_Fin")
match3 <- c("Annee_Bibliographique","Volume","Page_Debut","Page_Fin")
match4 <- c("Nom","Annee_Bibliographique","Volume","Page_Debut")
match5 <- c("Nom","Titre","Annee_Bibliographique")
match6 <- c("Titre","Annee_Bibliographique", "Page_Debut")

listid1 <- match_get_id(variable_list_for_match = match1)
listid2 <- match_get_id(variable_list_for_match = match2)
listid3 <- match_get_id(variable_list_for_match = match3)
listid4 <- match_get_id(variable_list_for_match = match4)
listid5 <- match_get_id(variable_list_for_match = match5)
listid6 <- match_get_id(variable_list_for_match = match6)

listid_all <-rbind(listid1, listid2, listid3, listid4, listid5, listid6,fill=TRUE)

saveRDS(listid_all[,.N,ID_Art][,.(ID_Art)],here(data_path, "macro_AA","2_Matched_data","Econlit_matched_ID_Art.RDS"))

# save as csv for OST database
# Econlit_matched_ID_Art <- readRDS(here(data_path, "macro_AA","2_Matched_data","Econlit_matched_ID_Art.RDS"))
# write.csv(Econlit_matched_ID_Art, here(data_path, "macro_AA","2_Matched_data","Econlit_matched_ID_Art.csv") )


# Comparing with before
nodes <- readRDS(here(data_path, "macro_AA","Corpus_Econlit_Matched_WoS", "JEL_matched_corpus_nodes.RDS"))
nodes2 <- readRDS(here(data_path, "macro_AA","Corpus_Econlit_Matched_WoS", "Old_JEL_matched_corpus_nodes.RDS"))
all_nodes <-rbind(nodes,nodes2)
all_nodes[ID_Art %notin% listid_all$ID_Art]
#some exploration
all_art[ID_Art %in% listid_all$ID_Art] 
hist(all_art[ID_Art %in% listid_all$ID_Art]$Annee_Bibliographique)
# Articles avant 1969
listid_all[ID_Art %in% all_art[ID_Art %in% listid_all$ID_Art][Annee_Bibliographique<1969]$ID_Art]
# Combien après
all_art[ID_Art %in% listid_all$ID_Art][Annee_Bibliographique<1969]
all_art[ID_Art %in% listid_all$ID_Art][Annee_Bibliographique>1969]


