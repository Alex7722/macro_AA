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

library(data.table)
library(magrittr)
library(RMySQL)
library(dplyr)
library(stringr)
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
Corpus <- merge(Corpus, rbind(Corpus_scopus[, .(Nom_ISI, ID_Art)],Authors[Ordre==1, .(Nom_ISI, ID_Art)]), by = "ID_Art", all.x = TRUE)
Corpus[,n_tiret:=str_count(Nom_ISI,"-")]
Corpus[,name_short:=Nom_ISI]
Corpus[n_tiret>1, name_short:=  str_replace(Nom_ISI, "\\-","")]
Corpus[, name_short:=  gsub("-.*","",name_short)]
Corpus$name_short <- toupper(Corpus$name_short)
Corpus <- Corpus[,Label:=paste0(name_short,",",Annee_Bibliographique)]
Corpus[, c("name_short","n_tiret"):=NULL]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### References ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

refs <- fread("EER/Corpus_EER/EER_REFS_XP.csv", quote="") %>% data.table 
refs <- refs[ID_Art_Source %in% Corpus$ID_Art]

#### Scopus merging %%%
# Scopus normalization
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
id_ref <- fread("EER/Corpus_EER/EER_refs_identifiers2.csv", quote="") %>% data.table
id_ref[, names_scopuslike:= str_replace(Nom, "\\-.*","")]
# Match on author.year.volume
id_ref_match <- id_ref[names_scopuslike!="NULL" & Annee!="NULL" & Volume!="NULL" & Page!="NULL"]
id_ref_match <- id_ref_match[,matching_col:=paste0(names_scopuslike,Annee,Volume,Page)]
refs_scopus_match <- refs_scopus[Nom!="<NA>" & Annee!="<NA>" & Volume!="<NA>" & first_page!="<NA>"]
refs_scopus_match <- refs_scopus_match[,matching_col:=paste0(Nom,Annee,Volume,first_page)]
# Match and get the temp_idref/ItemID_Ref relationship
scopus_ItemID_Ref <- merge(refs_scopus_match[,.(matching_col,temp_idref)], id_ref_match[,.(matching_col,ItemID_Ref)], by = "matching_col")
scopus_ItemID_Ref <- scopus_ItemID_Ref[,head(.SD, 1),matching_col]

#### Scopus and bind %%%
refs_scopus <- merge(refs_scopus[,.(ID_Art, temp_idref, Nom, Annee, journal_scopus=journal, Titre_scopus=title)], scopus_ItemID_Ref[,.(temp_idref,ItemID_Ref)], by = "temp_idref", all.x = TRUE)
refs_to_give_unique_Ids <- refs_scopus[is.na(ItemID_Ref)==TRUE][,find_scopus_ids:=.N,.(Nom,Annee)][find_scopus_ids>1][order(find_scopus_ids)]
write.csv(refs_to_give_unique_Ids, "refs_to_give_unique_Ids.csv")








#### Scopus and bind %%%
refs_scopus[is.na(ItemID_Ref)==FALSE]



refs_scopus <- refs_scopus %>% rename(Ordre = order_inst)
refs_scopus <- refs_scopus %>% rename(Institution = institution)
refs_scopus <- refs_scopus %>% rename(Pays = country)
Institutions_scopus[,Pays:=toupper(Pays)]
Institutions_scopus[,Institution:=toupper(Institution)]
Institutions <- rbind(Institutions, Institutions_scopus[,.(ID_Art, Institution, Pays, Ordre)], fill=TRUE)


refs[,ID_Art_Source:=as.character(ID_Art_Source)]
refs[,Id:=as.character(ItemID_Ref_Target)]
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
refs <- merge(refs, revues[,.(Code_Revue, Revue)], by="Code_Revue", all.x = TRUE)
refs[,Revue := sub("\r","", Revue)]
refs <- merge(refs, disciplines, by="Code_Discipline", all.x = TRUE)

# Info about Sources
refs[,nb_cit:=.N,ItemID_Ref_Target]

# Info about Sources
refs <- merge(refs, Corpus[,.(ID_Art, Annee_Bibliographique)], by.x = "ID_Art_Source", by.y = "ID_Art", all.x = TRUE)
setnames(refs, "Annee_Bibliographique", "Annee_Bibliographique_Source")
refs[,ID_Art:=ID_Art_Source]
refs[,ItemID_Ref:=ItemID_Ref_Target]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### References ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
saveRDS(Corpus, file = "EER/1_Corpus_Prepped_and_Merged/Corpus.rds")
saveRDS(Institutions, file = "EER/1_Corpus_Prepped_and_Merged/Institutions.rds")
saveRDS(Authors, file = "EER/1_Corpus_Prepped_and_Merged/Authors.rds")
saveRDS(refs, file = "EER/1_Corpus_Prepped_and_Merged/Refs.rds")

