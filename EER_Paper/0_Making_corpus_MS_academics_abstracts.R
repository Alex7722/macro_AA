require(bib2df)
require(ggplot2)
library(RMySQL)
library(data.table)
require(magrittr)
library(stringr)

setwd("/projects/data/macro_AA")
source("~/macro_AA/logins_DO_NOT_UPLOAD.R")
ESH <- dbConnect(MySQL(),
                 user = usr, password = pswd, dbname = "OST_Expanded_SciHum",
                 host = "127.0.0.1"
)

`%notin%` <- Negate(`%in%`)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Making the corpus ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

top_5_AB1 <- bib2df("EER/Corpus_AER/MS_Academics_AB/MS_JPE.bib")
top_5_AB2 <- bib2df("EER/Corpus_AER/MS_Academics_AB/MS_AER_69_90.bib")
top_5_AB3 <- bib2df("EER/Corpus_AER/MS_Academics_AB/MS_AER_91_07.bib")
top_5_AB4 <- bib2df("EER/Corpus_AER/MS_Academics_AB/MS_Econometrica.bib")
top_5_AB5 <- bib2df("EER/Corpus_AER/MS_Academics_AB/MS_QJE_01_07.bib")
top_5_AB6 <- bib2df("EER/Corpus_AER/MS_Academics_AB/MS_QJE_69_00.bib")
top_5_AB7 <- bib2df("EER/Corpus_AER/MS_Academics_AB/MS_RES.bib")
top_5_AB <- rbind(top_5_AB1, top_5_AB2, top_5_AB3, top_5_AB4, top_5_AB5, top_5_AB6, top_5_AB7)
top_5_AB <- top_5_AB %>% as.data.table()
top_5_AB$PAGES_START <- str_split_fixed(top_5_AB$PAGES, "--", 2) %>% .[,1]
top_5_AB$PAGES_END <- str_split_fixed(top_5_AB$PAGES, "--", 2) %>% .[,2]


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Exploration####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# histogram
ggplot(top_5_AB, aes(x=as.character(YEAR))) + geom_bar() +  facet_wrap(~JOURNAL, ncol = 2, scales = "free") 

# % of missing AB
top_5_AB %>% as.data.table() %>% .[,n_journals:=.N,JOURNAL] %>%  .[is.na(ABSTRACT)] %>% .[,.N,.(JOURNAL,n_journals)] %>% .[,N/n_journals,JOURNAL]

# histogram of missing abstracts
ggplot(top_5_AB[is.na(ABSTRACT)], aes(x=as.character(YEAR))) + geom_bar() +  facet_wrap(~JOURNAL, ncol = 2, scales = "free") 
top_5_AB

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Matching with WoS ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# all art
all_art <- dbGetQuery(ESH, paste0("SELECT * FROM OST_Expanded_SciHum.Articles WHERE ItemID_Ref != 0;")) %>% data.table()
issueID <- fread("Corpus_Econlit/revueID.csv", quote = "") %>% data.table()
# Disciplines and Journals
revues <- dbGetQuery(ESH, "SELECT Code_Revue, Revue, Code_Discipline FROM OST_Expanded_SciHum.Revues;") %>% data.table()
# Get IssueID for matching
all_art <- merge(all_art, issueID[, .(IssueID, Volume)], by = "IssueID")


######################### Getting all_art running smoothly ***************************
all_art <- all_art[, .(ID_Art, Annee_Bibliographique, Title = Titre, Code_Revue, Page_Debut, Page_Fin, Nb_Page, ItemID_Ref, Volume)]
all_art <- merge(all_art, revues, by = "Code_Revue")
all_art[, Revue := sub("\r", "", Revue)]
all_art <- all_art[, Pages := paste0(all_art$Page_Debut, "-", all_art$Page_Fin)] # formatting pages number
all_art <- all_art[, ID_by_pub := paste(all_art$Annee_Bibliographique, all_art$Revue, all_art$Pages, all_art$Volume)] # One variable for matching

######################### Do the same for dt of JEL ***************************
top_5_AB[, JOURNAL := toupper(JOURNAL)]
top_5_AB[JOURNAL=="THE AMERICAN ECONOMIC REVIEW", JOURNAL:="AMERICAN ECONOMIC REVIEW"]
top_5_AB[JOURNAL=="THE REVIEW OF ECONOMIC STUDIES", JOURNAL:="REVIEW OF ECONOMIC STUDIES"]
top_5_AB <- top_5_AB[, Pages := paste0(top_5_AB$PAGES_START, "-", top_5_AB$PAGES_END)] # formatting pages number
dt_MS_art <- top_5_AB[, ID_by_pub := paste(top_5_AB$YEAR, top_5_AB$JOURNAL, top_5_AB$Pages, top_5_AB$VOLUME)]
dt_MS_art <- dt_MS_art[PAGES_START!=""]
dt_MS_art <- dt_MS_art[PAGES_END!=""]
dt_MS_art <- dt_MS_art[VOLUME!=""]

#cleaninr double
dt_MS_art[,art_in_double:=.N,ID_by_pub] # some articles (~30) are in double because they have multiple pdf sources
dt_MS_art[art_in_double>1][order(ID_by_pub, NOTE)]
dt_MS_art[art_in_double>1][NOTE %like% "*Query*"]$BIBTEXKEY
dt_MS_art <- dt_MS_art[BIBTEXKEY %notin% dt_MS_art[art_in_double>1][NOTE %like% "*Query*"]$BIBTEXKEY] # remove artilces with "query dates" in note
dt_MS_art[, NOTE := sub(" cites:", "", NOTE)]
dt_MS_art[, NOTE := as.numeric(NOTE)]
dt_MS_art <- dt_MS_art[order(ID_by_pub, -NOTE)][,head(.SD,1),ID_by_pub] #keep only articles that are the most cited when same IDs


######################### Merging everything ***************************
# JEL with bd to find article by their volume, pages, year, and journal
dt_MS_art_ID_ART <- merge(dt_MS_art, all_art[,.(ID_Art, ID_by_pub)], by="ID_by_pub", all.x = TRUE)
saveRDS(dt_MS_art_ID_ART, "EER/1_Corpus_Prepped_and_Merged/abstracts_MS_with_ID_Art.RDS")
saveRDS(top_5_AB, "EER/1_Corpus_Prepped_and_Merged/abstracts_MS_RAW.RDS")



                                                        