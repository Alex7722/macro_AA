#' ---
#' title: "Script for building the EER corpus"
#' author: "Aurélien Goutsmedt and Alexandre Truc"
#' date: "/ Last compiled on `r format(Sys.Date())`"
#' output: 
#'   github_document:
#'     toc: true
#'     number_sections: true
#' ---
#' 
#' # What is this script for?
#' 
#' This script aims at extracting all the articles of the EER, the references, the list of authors
#' and their affiliations. It also extracts the same data for the four missing years (1970-1973).
#' 
#' 
#' > WARNING: This script still needs a lot of cleaning
#' 
#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)

#' # Loading packages, paths and data
#'
library(RMySQL)
source("~/macro_AA/EER_Paper/Script_paths_and_basic_objects_EER.R")
pswd = 'alex55Truc!1epistemo'
usr = 'alexandre'
ESH <- dbConnect(MySQL(), user=usr, password=pswd, dbname='OST_Expanded_SciHum',
                 host='127.0.0.1')
setwd("/projects/data/macro_AA")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Disciplines ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

revues <- fread("all_journals.csv", quote="") %>% data.table
revues[,Code_Discipline:=as.character(Code_Discipline)]
revues[,Code_Revue:=as.character(Code_Revue)]

disciplines  <-  dbGetQuery(ESH, "SELECT ESpecialite, EGrande_Discipline, Code_Discipline FROM OST_Expanded_SciHum.Disciplines;") %>% data.table
disciplines <- disciplines[EGrande_Discipline=="Natural Sciences and Engineering", ESpecialite_custom:="Other NSE"]
disciplines <- disciplines[EGrande_Discipline=="Social Sciences and Humanities", ESpecialite_custom:="Other SSH"]
disciplines <- disciplines[Code_Discipline==132, ESpecialite_custom:= "Management"]
disciplines <- disciplines[Code_Discipline==119, ESpecialite_custom:= "Economics"]
disciplines <- disciplines[Code_Discipline==18, ESpecialite_custom:= "General NSE"]

disciplines <- disciplines[Code_Discipline>=101 & Code_Discipline<=109, ESpecialite_custom:="Psychology"]

disciplines <- disciplines[Code_Discipline==4, ESpecialite_custom:= "Ecology and ES"]
disciplines <- disciplines[Code_Discipline==69, ESpecialite_custom:= "Ecology and ES"]

disciplines <- disciplines[Code_Discipline==125, ESpecialite_custom:= "Pol Sci"]

disciplines <- disciplines[Code_Discipline==120, ESpecialite_custom:= "Geography"]

disciplines <- disciplines[Code_Discipline==91, ESpecialite_custom:= "Math and Stat"] # statistics
disciplines <- disciplines[Code_Discipline==88, ESpecialite_custom:= "Math and Stat"]
disciplines <- disciplines[Code_Discipline==89, ESpecialite_custom:= "Math and Stat"]
disciplines <- disciplines[Code_Discipline==90, ESpecialite_custom:= "Math and Stat"]

# disciplines$ESpecialite_custom <- str_wrap(disciplines$ESpecialite_custom, width = 10)

disciplines[,Code_Discipline:=as.character(Code_Discipline)]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Basic Corpus ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
Corpus <- fread("EER/Corpus_EER/EER_NODES_XP.csv", quote="") %>% data.table

# Scopus and bind
Corpus_scopus <- readRDS("EER/Corpus_EER/scopus_articles.RDS")
Corpus_scopus <- Corpus_scopus %>% rename(ID_Art = temp_id)
Corpus_scopus[,ID_Art:=paste0("S",ID_Art)]
Corpus_scopus <- Corpus_scopus %>% rename(Titre = title)
Corpus_scopus <- Corpus_scopus %>% rename(Nom_ISI = author)
Corpus_scopus[,Nom_ISI:=toupper(Nom_ISI)]
Corpus_scopus[,Code_Document:=99]
Corpus_scopus[,Code_Revue:=5200]
Corpus_scopus[,Code_Discipline:=119]
Corpus_scopus[,ItemID_Ref:=ID_Art]
Corpus <- rbind(Corpus, Corpus_scopus[,.(ID_Art, ItemID_Ref, Annee_Bibliographique, Titre, Code_Document, Code_Revue, Code_Discipline)])

Corpus[,Id:=as.character(ID_Art)]
Corpus[,ID_Art:=as.character(ID_Art)]
Corpus[,ItemID_Ref:=as.character(ItemID_Ref)]
Corpus[,Code_Revue:=as.character(Code_Revue)]
Corpus[, c("Code_Discipline"):=NULL]

# Document types
Corpus <- Corpus[Code_Document == 1 | Code_Document == 2 | Code_Document == 3 | Code_Document == 6 | Code_Document == 99]

# JEL IDs
JEL1 <- readRDS("/projects/data/macro_AA/Corpus_Econlit_Matched_WoS/Old_JEL_matched_corpus_nodes.rds")
JEL2 <- readRDS("/projects/data/macro_AA/Corpus_Econlit_Matched_WoS/JEL_matched_corpus_nodes.rds")
JEL <- rbind(JEL1,JEL2)
Corpus <- Corpus[,JEL_id:=0][ID_Art %in% JEL$ID_Art, JEL_id:=1]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Authors ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
Authors <- fread("EER/Corpus_EER/EER_AUT_XP.csv", quote="") %>% data.table

# Scopus and bind
Authors_scopus <- readRDS("EER/Corpus_EER/scopus_authors.RDS")
Authors_scopus <- Authors_scopus %>% rename(ID_Art = temp_id)
Authors_scopus[,ID_Art:=paste0("S",ID_Art)]

Authors_scopus <- Authors_scopus %>% rename(Ordre = order)
Authors_scopus <- Authors_scopus %>% rename(Nom_ISI = author)
Authors <- rbind(Authors, Authors_scopus)

Authors[,ID_Art:=as.character(ID_Art)]
Authors <- Authors[ID_Art %in% Corpus$ID_Art][,Nom_ISI:=toupper(Nom_ISI)]

# info about sources
Authors <- merge(Authors, Corpus[,.(ID_Art, Annee_Bibliographique)], by = "ID_Art", all.x = TRUE)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Institutions ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# UE <- fread("EER/UE.csv") %>% data.table
UE <- fread("EER/Europe_continent.csv") %>% data.table 

Institutions <- fread("EER/Corpus_EER/EER_INST_XP.csv", quote="") %>% data.table

# Scopus and bind
Institutions_scopus <- readRDS("EER/Corpus_EER/scopus_institutions.RDS")
Institutions_scopus <- Institutions_scopus %>% rename(ID_Art = temp_id)
Institutions_scopus[,ID_Art:=paste0("S",ID_Art)]
Institutions_scopus <- Institutions_scopus %>% rename(Ordre = order_inst)
Institutions_scopus <- Institutions_scopus %>% rename(Institution = institution)
Institutions_scopus <- Institutions_scopus %>% rename(Pays = country)
Institutions_scopus[,Pays:=toupper(Pays)]
Institutions_scopus[,Institution:=toupper(Institution)]
Institutions <- rbind(Institutions, Institutions_scopus[,.(ID_Art, Institution, Pays, Ordre)], fill=TRUE)


# Cleaning this wrong info
Institutions[Pays=="FED-REP-GER", Pays:="GERMANY"]
Institutions[Pays=="WEST-GERMANY", Pays:="GERMANY"]
Institutions[Pays=="CZECHOSLOVAKIA" | Pays=="CZECH-REPUBLIC", Pays:="CZECH REPUBLIC"]

Institutions[, c("Nom_ISI", "Ordre"):=NULL] # removing useless columns
Institutions <- Institutions[,head(.SD, 1),.(ID_Art,Institution)] #keeping unique institutions by ID_Art
Institutions <- Institutions[Institution!="NULL" | Pays!="NULL" ]

Institutions[,ID_Art:=as.character(ID_Art)]
Institutions <- Institutions[ID_Art %in% Corpus$ID_Art]

# info about sources
Institutions <- merge(Institutions, Corpus[,.(ID_Art, Annee_Bibliographique)], by = "ID_Art", all.x = TRUE)

# Identifying europe
Institutions[, Countries_grouped:=Pays]

Institutions[Pays %in% toupper(UE$Countries), Countries_grouped:="Europe"]

Institutions[Countries_grouped!="Europe" & Countries_grouped!="USA",.N,Pays][order(-N)]
Institutions[Countries_grouped=="Europe",.N,Pays][order(-N)]

# Identifying Collaborations
Institutions[,n_institutions_tot:=.N,.(ID_Art)]
Institutions[,EU:=0][Countries_grouped=="Europe", EU:=1][,EU_share:=sum(EU)/n_institutions_tot,ID_Art]
Institutions[,US:=0][Countries_grouped=="USA", US:=1][,US_share:=sum(US)/n_institutions_tot,ID_Art]
Institutions[, EU_US_collab:= "Neither", ID_Art]
Institutions[EU_share>0 & US_share>0, EU_US_collab:= "Collaboration", ID_Art]
Institutions[EU_share==0 & US_share==1, EU_US_collab:= "USA Only", ID_Art]
Institutions[EU_share==1 & US_share==0, EU_US_collab:= "Europe Only", ID_Art]


bridges_collab <- Institutions[EU_US_collab== "Collaboration"][,list(Target = rep(Institution[1:(length(Institution)-1)],(length(Institution)-1):1),
                                                                     Source = rev(Institution)[sequence((length(Institution)-1):1)]),
                                                               by= ID_Art]
bridges_collab <- bridges_collab[Source > Target, c("Target", "Source") := list(Source, Target)] # exchanging
bridges_collab[,.N,.(Target,Source)][order(-N)] %>% top_n(20)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Labels, Journals and Disciplines of Corpus ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#label
first_aut <- Authors[Ordre==1, .(Nom_ISI, ID_Art)]
Corpus <- merge(Corpus, first_aut, by = "ID_Art", all.x = TRUE)
Corpus[,n_tiret:=str_count(Nom_ISI,"-")]
Corpus[,name_short:=Nom_ISI]
Corpus[n_tiret>1, name_short:=  str_replace(Nom_ISI, "\\-","")]
Corpus[, name_short:=  gsub("-.*","",name_short)]
Corpus$name_short <- toupper(Corpus$name_short)
Corpus <- Corpus[,Label:=paste0(name_short,",",Annee_Bibliographique)]
Corpus[, c("name_short","n_tiret"):=NULL]

Corpus[,.N,ID_Art][N>1]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### References ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################  Claveau BD %%%%%%%%%%%%

all_ref <-  dbGetQuery(ESH, paste0("SELECT ID_Art, ItemID_Ref, New_id2, Annee, Nom, Revue_Abbrege, Volume, Page 
                                   FROM OST_Expanded_SciHum.References7 as ref 
                                   WHERE ID_Art IN (SELECT ID_Art FROM OST_Expanded_SciHum.Articles WHERE Code_Revue=5200);")) %>%  data.table
all_art <-  dbGetQuery(ESH, paste0("SELECT * 
                                   FROM OST_Expanded_SciHum.Articles 
                                   WHERE ItemID_Ref IN (SELECT ItemID_Ref FROM OST_Expanded_SciHum.References7 WHERE ID_Art IN (SELECT ID_Art FROM OST_Expanded_SciHum.Articles WHERE Code_Revue=5200));")) %>%  data.table
Corpus_Claveau <-  dbGetQuery(ESH, paste0("SELECT ID_Art, Annee_Bibliographique, Code_Revue 
                                   FROM OST_Expanded_SciHum.Articles
                                   WHERE Code_Revue=5200")) %>%  data.table

# Infos about refs (REF_RAW=83 174 with 59 286 unique id2 et 34 062 unique ItemID_Ref)
refs_Claveau <- merge(all_ref, all_art[ItemID_Ref!=0], by="ItemID_Ref", all.x = TRUE) 

# Infos about revues
refs_Claveau <- merge(refs_Claveau, revues[,Code_Revue:=as.integer(Code_Revue)][,.(Code_Revue,Code_Discipline)], by="Code_Revue", all.x = TRUE)

# Noramlize Claveau's BD
refs_Claveau <- refs_Claveau[,.(ID_Art_Source=ID_Art.x, ItemID_Ref_old_claveau=ItemID_Ref, New_id2, Nom, Annee, Code_Revue, Code_Discipline, Titre, Revue_Abbrege, Volume, Page)]
refs_Claveau <- refs_Claveau[ID_Art_Source %in% Corpus$ID_Art]
# Take unique observation when multiple refs for one article
refs_Claveau_unique <- refs_Claveau[ItemID_Ref_old_claveau!=0, head(.SD, 1), .(ID_Art_Source,ItemID_Ref_old_claveau)]
refs_Claveau <- rbind(refs_Claveau_unique,refs_Claveau[ItemID_Ref_old_claveau==0])
if(refs_Claveau[ItemID_Ref_old_claveau!="0",.N,.(ID_Art_Source,ItemID_Ref_old_claveau)][N>1][,.N]>1){print("ALERTE")}

# ItemID_Ref as new_id2, excepted when there is no New_id2 but an ItemID_Ref
refs_Claveau[,ItemID_Ref_Target:=as.character(ItemID_Ref_old_claveau)]
refs_Claveau[ItemID_Ref_old_claveau==0 & New_id2!=0,ItemID_Ref_Target:=paste0("cl",New_id2)]

# ################  Main_BD %%%%%%%%%%%%
# test <- fread("EER/Corpus_EER/EER_REFS_XP.csv", quote="") %>% data.table
# # Remove stupid doubles from stupid database having multiple ItemID_Ref for same ID_Art
# list_error_merge_title <- refs[ItemID_Ref_Target!="NULL",.N,.(ID_Art_Source,ItemID_Ref_Target)][N>1]
# refs[ItemID_Ref_Target %in% list_error_merge_title$ItemID_Ref_Target, Code_Discipline:="NULL"]
# refs[ItemID_Ref_Target %in% list_error_merge_title$ItemID_Ref_Target, Code_Revue:="NULL"]
# refs[ItemID_Ref_Target %in% list_error_merge_title$ItemID_Ref_Target, Titre:="NULL"]
# # Take unique observation when multiple refs for one article
# refs_unique <- refs[ItemID_Ref_Target!="NULL", head(.SD, 1), .(ID_Art_Source,ItemID_Ref_Target)] 
# refs <- rbind(refs_unique,refs[ItemID_Ref_Target=="NULL"])
# 
# refs[ItemID_Ref_Target!="NULL",.N,.(ID_Art_Source,ItemID_Ref_Target)][N>1]
# 
# #test to compare both ref tables
# refs_Claveau[ID_Art_Source %in% Corpus[Annee_Bibliographique<=2015]$ID_Art, .N]
# refs[ID_Art_Source %in% Corpus[Annee_Bibliographique<=2015]$ID_Art, .N]
# 
# ################  Bind all %%%%%%%%%%%%
# #Bind both
# refs <- rbind(refs[ItemID_Ref_Target!="NULL"], 
#               refs_Claveau[ItemID_Ref_old_claveau==0], 
#               fill=TRUE)

refs <- refs_Claveau[ID_Art_Source %in% Corpus$ID_Art]

# put everything in place for cleaning, we now have 83 153 refs with 58 646 unique refs
cleaning <- refs[ItemID_Ref_Target != 0, n_aut_year_couple:=.N,.(Nom,Annee)]
cleaning <- refs[ItemID_Ref_Target != 0, n_cit:=.N,ItemID_Ref_Target]

cleaning <- cleaning[ItemID_Ref_Target != 0][order(-n_aut_year_couple, Nom, Annee, ItemID_Ref_Target),.(n_aut_year_couple,n_cit, ItemID_Ref_Target, Nom, Annee, Titre, Revue_Abbrege)]
cleaning <- cleaning[!duplicated(cleaning)]
cleaning[,ItemID_Ref_Target:=as.character(ItemID_Ref_Target)]

#export and import the cleaned version for [ItemID_Ref_Target != 0]
write.csv(cleaning, "EER/Corpus_EER/cleaning_within_newid2.csv")
cleaned_refs <- fread("EER/Corpus_EER/cleaning_within_newid2_cleaned.csv") %>% data.table
cleaned_refs<- cleaned_refs[,.(New_ItemID_Ref_Target,ItemID_Ref_Target,New_Titre)][New_ItemID_Ref_Target!="" | New_Titre!=""]
cleaned_refs<- cleaned_refs[!duplicated(cleaned_refs)]

refs_final <- merge(refs, cleaned_refs, by = "ItemID_Ref_Target", all.x = TRUE, all.y = FALSE)
refs_final[New_ItemID_Ref_Target!="", ItemID_Ref_Target:=New_ItemID_Ref_Target]
refs_final[New_Titre!="", Titre:=New_Titre]
refs <- copy(refs_final)

#export and import the cleaned version for [ItemID_Ref_old_claveau==0 & New_id2!=0 & n_aut_year_couple>5]
write.csv(refs[ItemID_Ref_old_claveau==0 & New_id2!=0 & n_aut_year_couple>5][order(Nom,Annee,ItemID_Ref_Target)], "EER/Corpus_EER/cleaning_within_newid2_step2.csv")
cleaned_refs <- fread("EER/Corpus_EER/cleaning_within_newid2_step2_cleaned.csv") %>% data.table
cleaned_refs<- cleaned_refs[,.(New_ItemID_Ref_Target,ItemID_Ref_Target)][New_ItemID_Ref_Target!=""]
cleaned_refs<- cleaned_refs[!duplicated(cleaned_refs)]
refs[,New_ItemID_Ref_Target:=NULL]
refs_final <- merge(refs, cleaned_refs, by = "ItemID_Ref_Target", all.x = TRUE, all.y = FALSE)
refs_final[is.na(New_ItemID_Ref_Target)==FALSE, ItemID_Ref_Target:=New_ItemID_Ref_Target]
# we cleaned the corpus twice, we now have 83 153 refs 57 451 unique refs
refs <- copy(refs_final)
# remove institutionnal papers we now have 81 083 and 55 583 unique
refs <- refs[str_detect(Nom,"\\*")==FALSE]

refs[Revue_Abbrege %like% "HDB EC GROW%"]
write.csv(refs[ID_Art_Source %in% Corpus[Annee_Bibliographique<=1980]$ID_Art][ItemID_Ref_Target!=0,.N,.(ItemID_Ref_Target,Nom,Annee, Titre, Revue_Abbrege)][order(Nom,Annee,Revue_Abbrege,Titre)], "EER/Corpus_EER/explore_refs_last_check.csv")


#last cleaning
cleaned_refs <- fread("EER/Corpus_EER/explore_refs_last_check_cleaned.csv") %>% data.table
cleaned_refs<- cleaned_refs[,.(New_ItemID_Ref_Target,ItemID_Ref_Target,New_Titre, New_Nom)][New_ItemID_Ref_Target!="" | New_Titre!="" | New_Nom!=""]
cleaned_refs<- cleaned_refs[!duplicated(cleaned_refs)]

refs[,New_ItemID_Ref_Target:=NULL]
refs[,New_Titre :=NULL]

refs_final <- merge(refs, cleaned_refs, by = "ItemID_Ref_Target", all.x = TRUE, all.y = FALSE)
refs_final[New_ItemID_Ref_Target!="", ItemID_Ref_Target:=New_ItemID_Ref_Target]
refs_final[New_Titre!="", Titre:=New_Titre]
refs_final[New_Nom!="", Nom:=New_Nom]
# We now have 81 083 and 55 404 unique
refs <- copy(refs_final)




# refs[,n_aut_year:=NULL]
refs_Claveau[ItemID_Ref_Target=="46322181"][,.N]
refs_Claveau[ItemID_Ref_Target=="36302916"][,.N]
refs[ItemID_Ref_Target=="975989"][,.N]
################  Scopus merging %%%%%%%%%%%%
# Scopus normalization: there are 665 references in the scopus bd
refs_scopus <- readRDS("EER/Corpus_EER/scopus_references.RDS")
refs_scopus <- refs_scopus %>% rename(ID_Art = temp_id)
refs_scopus[,ID_Art:=paste0("S",ID_Art)]
refs_scopus[,temp_idref:=paste0("SR",temp_idref)]
refs_scopus <- refs_scopus %>% rename(Nom = author)
refs_scopus[,Nom:=toupper(Nom)]
refs_scopus <- refs_scopus %>% rename(Annee = Year)
refs_scopus <- refs_scopus %>% rename(Volume = volume)
refs_scopus <- refs_scopus %>% rename(Page = pages)
refs_scopus[, first_page:= str_replace(Page, "\\-.*","")]

# WoS normalization
# id_ref <- fread("EER/Corpus_EER/EER_refs_identifiers2.csv", quote="") %>% data.table
id_ref <- copy(refs)
# id_ref <- refs #29943
id_ref[, names_scopuslike:= str_replace(Nom, "\\-.*","")]


# # Match on author.year.volume
# id_ref_match <- id_ref[names_scopuslike!="NULL" & Annee!="NULL" & Volume!="NULL" & Page!="NULL"]
# id_ref_match <- id_ref_match[,matching_col:=paste0(names_scopuslike,Annee,Volume,Page)]
# refs_scopus_match <- refs_scopus[Nom!="<NA>" & Annee!="<NA>" & Volume!="<NA>" & first_page!="<NA>"]
# refs_scopus_match <- refs_scopus_match[,matching_col:=paste0(Nom,Annee,Volume,first_page)] 
# # Match and get the temp_idref/ItemID_Ref relationship
# scopus_ItemID_Ref <- merge(refs_scopus_match[,.(matching_col,temp_idref)], id_ref_match[,.(matching_col,ItemID_Ref_Target)], by = "matching_col")
# scopus_ItemID_Ref <- scopus_ItemID_Ref[,head(.SD, 1),matching_col]




# #### Give uniques IDs to the sames references that are not in WoS %%%
# refs_scopus <- merge(refs_scopus[,.(ID_Art, temp_idref, Nom, Annee, journal_scopus=journal, Titre_scopus=title)], scopus_ItemID_Ref[,.(temp_idref,ItemID_Ref_Target)], by = "temp_idref", all.x = TRUE)
# refs_to_give_unique_Ids <- refs_scopus[,find_scopus_ids:=.N,.(Nom,Annee)][order(find_scopus_ids)]
# write.csv(refs_to_give_unique_Ids[order(Nom,Annee)], "EER/Corpus_EER/refs_to_give_unique_Ids.csv")

# Manually give them Ids
# match_list_manual <- id_ref[names_scopuslike %in% refs_to_give_unique_Ids$Nom & Annee %in% refs_to_give_unique_Ids$Annee][order(Nom, Annee)]
# write.csv(match_list_manual, "EER/Corpus_EER/manual_check.csv")
refs_scopus <- refs_scopus %>% rename(journal_scopus = journal)
refs_scopus <- refs_scopus %>% rename(Titre_scopus = title)

refs_to_give_unique_Ids <- fread("EER/Corpus_EER/refs_to_give_unique_Ids_cleaned.csv", quote="") %>% data.table
refs_to_give_unique_Ids <- refs_to_give_unique_Ids %>% rename(manual_ids = manual_id)

refs_to_give_unique_Ids <- refs_to_give_unique_Ids[manual_ids!=""]

#### Scopus and bind %%%
refs_scopus <- merge(refs_scopus, refs_to_give_unique_Ids[,.(temp_idref,manual_ids)], by="temp_idref", all.x = TRUE)
refs_scopus[, ItemID_Ref_Target:=manual_ids]
refs_scopus[is.na(ItemID_Ref_Target)==TRUE, ItemID_Ref_Target:=temp_idref]
write.csv(refs_scopus, "EER/Corpus_EER/explore_refs_scopus_last_check.csv")

#### FINAL SCOPUS MANUAL CLEANING %%%
refs_scopus <- fread("EER/Corpus_EER/explore_refs_scopus_last_check_cleaned.csv") %>% data.table
refs_scopus[is.na(manual_ids)==FALSE, ItemID_Ref_Target:=manual_ids]
refs_scopus <- refs_scopus %>% rename(journal_scopus = journal)
refs_scopus <- refs_scopus %>% rename(Titre_scopus = title)
refs_scopus <- merge(refs_scopus, cleaned_refs, by = "ItemID_Ref_Target", all.x = TRUE, all.y = FALSE)
refs_scopus[New_ItemID_Ref_Target!="", ItemID_Ref_Target:=New_ItemID_Ref_Target]
refs_scopus[,New_ItemID_Ref_Target:=NULL]
refs_scopus[,New_Titre :=NULL]
refs_scopus[,New_Nom :=NULL]


refs <- rbind(refs, refs_scopus[,.(ID_Art_Source=ID_Art,ItemID_Ref_Target, Nom, Annee, journal_scopus,Titre_scopus)], fill=TRUE)
refs[ItemID_Ref_Target=="SR10",Titre_scopus:="L'Equilibre economique en 1961"]
refs[is.na(Titre) & is.na(Titre_scopus)==FALSE, Titre:=toupper(Titre_scopus)]
refs[, c("Titre_scopus"):=NULL]

################ Completing Refs Informations %%%%%%%%%%%%
refs[,ID_Art_Source:=as.character(ID_Art_Source)]
refs[,Id:=as.character(ID_Art_Source)]
refs[,Annee_Bibliographique_Target:=Annee]
refs[,Nom_Target:=Nom]
refs[,Code_Revue:=as.character(Code_Revue)]
refs[,Code_Discipline:=as.character(Code_Discipline)]

# Label column
refs <- refs[, name_short:=  gsub("-.*","",Nom)]
refs$name_short <- toupper(refs$name_short)
refs <- refs[,Label_Target:=paste0(name_short,",",Annee_Bibliographique_Target)]
refs[, c("name_short"):=NULL]

# Disciplines and journals
refs <- merge(refs, revues[,.(Code_Revue=as.character(Code_Revue), Revue)], by="Code_Revue", all.x = TRUE)
refs[,Revue := sub("\r","", Revue)]
refs <- merge(refs, disciplines, by="Code_Discipline", all.x = TRUE)
refs[is.na(Revue) & is.na(journal_scopus)==FALSE, Revue:=toupper(journal_scopus)]
refs[, c("journal_scopus"):=NULL]

# Info about Sources
refs[,nb_cit_tot:=.N,ItemID_Ref_Target]

# Info about Sources
refs <- merge(refs, Corpus[,.(ID_Art, Annee_Bibliographique)], by.x = "ID_Art_Source", by.y = "ID_Art", all.x = TRUE)
setnames(refs, "Annee_Bibliographique", "Annee_Bibliographique_Source")
refs[,ID_Art:=ID_Art_Source]
refs[,ItemID_Ref:=ItemID_Ref_Target]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Saving ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
saveRDS(Corpus, file = "EER/1_Corpus_Prepped_and_Merged/Corpus.rds")
saveRDS(Institutions, file = "EER/1_Corpus_Prepped_and_Merged/Institutions.rds")
saveRDS(Authors, file = "EER/1_Corpus_Prepped_and_Merged/Authors.rds")
saveRDS(refs, file = "EER/1_Corpus_Prepped_and_Merged/Refs.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Last steps of cleaning ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' We need to remove the doublons between WoS and scopus articles. We first load 
#' WoS articles and check for the dates
#' 

Corpus <- readRDS(paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Corpus.rds"))
remove_id <- Corpus[Annee_Bibliographique == 1970 & str_detect(ID_Art, "S")]$ID_Art
Corpus <- Corpus[!ID_Art %in% remove_id]

refs <- readRDS(paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Refs.rds")) %>% 
  mutate(Titre = toupper(Titre)) %>% 
  as.data.table()
refs <- refs[!ID_Art_Source %in% remove_id]

Authors <- readRDS(paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Authors.rds"))
Authors <- Authors[!ID_Art %in% remove_id]

#' Thanks to scopus, we have spotted wrong author names in WoS, that we can now correct
#' 
correct_name <- data.table(wrong_name = c("CONSTANT-M",
                                          "GEORGAKO-T",
                                          "HJALMARS-L",
                                          "MUELLBAU-J",
                                          "TINBERGE-J",
                                          "VANDENNO-P",
                                          "VANDERLO-S",
                                          "SCHIOPPA-F",
                                          "BROWN-P",
                                          "HAGEN-O",
                                          "STPAUL-G"),
                           right_name = c("CONSTANTOPOULOS-M",
                                          "GEORGAKOPOULOS-T",
                                          "HJALMARSSON-L",
                                          "MUELLBAUER-J",
                                          "TINBERGEN-J",
                                          "VANDENNOORT-P",
                                          "VANDERLOEFF-S",
                                          "PADOASCHIOPPA-F",
                                          "CLARKBROWN-P",
                                          "VONDEMHAGEN-O",
                                          "SAINTPAUL-G"))

for(i in seq_along(correct_name$wrong_name)){
  Corpus <- Corpus %>% 
    mutate(Nom_ISI = str_replace_all(Nom_ISI, correct_name$wrong_name[i], correct_name$right_name[i]))
}

#' We now need to add the abstracts identified with scopus. The difficulty
#' is to match the two corpora together. The strategy is to match on author-date
#' when there is a unique author-date per year, then to match on title, then
#' to match what is lefting for multiple author-date for a year, and then to finish
#' manually.
#' 

begin_words <- c("^A ","^THE ","^AN ", "^ON THE ")
Corpus <- Corpus[order(Annee_Bibliographique,Nom_ISI,Titre)][Annee_Bibliographique < 2019]
Corpus <- Corpus %>% 
  mutate(author_date = paste0(Nom_ISI,"-",Annee_Bibliographique),
         Titre = toupper(Titre),
         Titre_alt = str_squish(str_replace_all(Titre, "[:punct:]", " ")),
         Titre_alt = str_remove(Titre_alt, paste0(begin_words, collapse = "|"))) %>% 
  group_by(author_date) %>% 
  mutate(count_authordate = n()) %>% 
  as.data.table()

#' Loading and preparing abstracts data
scopus_abstract <- readRDS(paste0(eer_data,"scopus_abstract.RDS"))
scopus_abstract[Nom_ISI == "THELL-H"]$Nom_ISI <- "THEIL-H"
scopus_abstract[Nom_ISI == "RECONCILIATION-A"]$Nom_ISI <- "BRADA-J"
scopus_abstract[Nom_ISI == "STEIGUMJR-E"]$Nom_ISI <- "STEIGUM-E"
scopus_abstract[Nom_ISI == "ROSEN-NA"]$Nom_ISI <- "ROSEN-A"

scopus_abstract <- scopus_abstract[order(Annee_Bibliographique,Nom_ISI,Titre)][Annee_Bibliographique <= max(Corpus$Annee_Bibliographique) &
                                     !is.na(abstract)]
scopus_abstract <- scopus_abstract %>% 
  mutate(author_date = paste0(Nom_ISI,"-",Annee_Bibliographique),
         Titre_alt = str_squish(str_replace_all(Titre, "[:punct:]", " ")),
         Titre_alt = str_remove(Titre_alt, paste0(begin_words, collapse = "|"))) %>% 
  group_by(author_date) %>% 
  mutate(count_authordate = n()) %>% 
  as.data.table()

merge_corpus_authordate <- merge(scopus_abstract[count_authordate == 1, c("temp_id","abstract","author_date")], 
                      Corpus[count_authordate  == 1], 
                      by = "author_date")

merge_corpus_title <- merge(scopus_abstract[,c("temp_id","abstract","Titre_alt")], 
                               Corpus, 
                               by = "Titre_alt")

#' There is a "normal" doublon in `merge_corpus_title` that we can remove (an article
#' is an answer to another article, with the same title)
#' 
merge_corpus_title <- merge_corpus_title[-which(duplicated(merge_corpus_title$abstract))]

#' We now work on the cases with two author-dates. 
#' 

#merge_corpus_alt <- merge(scopus_abstract[count_authordate > 1, c("temp_id","abstract","Titre_alt")], 
#                          Corpus[count_authordate  > 1], 
#                          by = "Titre_alt") %>% 
#  unique()

#merge_corpus_alt <- merge_corpus_alt[-which(duplicated(merge_corpus_alt$abstract))]

#' We bind the two merges and remove doublons

merge_corpus <- rbind(merge_corpus_authordate, merge_corpus_title) %>% 
  unique()


#' We can now finish manually by comparing the lefting content of the two data-frames:
#' - `view(scopus_abstract[order(Annee_Bibliographique, Nom_ISI)][! temp_id %in% merge_corpus$temp_id])`
#' - `view(Corpus[! ID_Art %in% merge_corpus$ID_Art, -c("Code_Revue","Code_Document","Id","JEL_id")])`
#' We build a dataframe with the missing abstracts that we can bind with what we already have, and 
#' then merge to the Corpus.

link_id <- data.table(ID_Art = c("41014562","42281165","42035495","7143275",
                                 "7917371","11258778","10716665","12147841",
                                 "12668535","14139264","19354563","61558363",
                                 "61558365","61558360"),
                      temp_id = as.integer(c("650","552","554","2784",
                                  "2612","2199","2288","2153",
                                  "1983","1859","1398","3603",
                                  "3602","3614")))

manual_merge <- merge(link_id, scopus_abstract, by = "temp_id")
merge_corpus <- rbind(merge_corpus[,c("ID_Art","abstract")],manual_merge[,c("ID_Art","abstract")])
Corpus <- merge(Corpus, merge_corpus, by = "ID_Art", all.x = TRUE)
Corpus[, Label := paste0(str_remove(Nom_ISI, "-.*"), ",",Annee_Bibliographique)]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Saving bis ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
saveRDS(Corpus, file = paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Corpus.rds"))
saveRDS(Authors, file = paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Authors.rds"))
saveRDS(refs, file = paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Refs.rds"))

