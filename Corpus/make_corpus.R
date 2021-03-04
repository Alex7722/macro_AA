library(xml2)
library(knitr)
library(png)
library(grid)
library(ggnewscale)
library(vite)
library(RMySQL)
library(NetworkToolbox)
library(broom)
library(igraph)
library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)
library(tm)
library(tidyr)
library(tidytext)
library('cluster')
library('ggraph')
library('tibble')
library('tidygraph')
library(ggrepel)
library(readr)
library(leiden)
library(ggraph)
library(ggnewscale)
library(remotes)
library(vite)
library("reticulate")
library(reticulate)
library(leiden)
library(tidygraph)
library(rlang)
library(leiden)
library(ggforce)
library(d3Network)
library(scales)
library(RColorBrewer)
require(DescTools)
require(stringr)
require(docstring)

pswd = 'alex55Truc!1epistemo'
usr = 'alexandre'
ESH <- dbConnect(MySQL(), user=usr, password=pswd, dbname='OST_Expanded_SciHum',
                 host='127.0.0.1')




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART I : GETTING THE CORPUS FROM XML ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Setting things up####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

######################### Directory ***************************
setwd("/projects/data/macro_AA")

######################### Functions ***************************
`%notin%` <- Negate(`%in%`)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
######################### From xml to DT ***************************
# Give the input file name to the function.
from_xml_to_df <- function(x ### x=df of edges
){
  #econlit <- read_xml("BE_JEL6K005.xml")
  econlit <- read_xml(paste0('',x,".xml",''))
  
  #This line create a kind of list of all entries
  econlit <- xml_find_all(econlit, "//rec")
  #Extracting data
  title <- xml_text(xml_find_first(econlit, ".//atl"))
  year <- data.table(xml_text(xml_find_first(econlit, ".//dt")))
  year[,V1 := substr(V1,1,4)]
  journal <- xml_text(xml_find_first(econlit, ".//jtl"))
  vol <- xml_text(xml_find_first(econlit, ".//vid"))
  no <- xml_text(xml_find_first(econlit, ".//iid"))
  pubType <- xml_text(xml_find_first(econlit, ".//pubtype"))
  pages <- xml_text(xml_find_first(econlit, ".//pages"))
  #Compiling 5831 before finance now 6005
  macro1 <- data.table(Title = title, Year = year, Journal = journal, Vol = vol, No = no, Pages = pages, PubType = pubType)
  
  #Keep only articles
  macro1 <- macro1[PubType=="Journal Article"]
  return(macro1) # utilser cette ligne pour sortir un objet.
}

#Macro
#Macro
macro1 <- from_xml_to_df("Corpus_Econlit/macro1")
macro2 <- from_xml_to_df("Corpus_Econlit/macro2")
macro3 <- from_xml_to_df("Corpus_Econlit/macro3")
macro4 <- from_xml_to_df("Corpus_Econlit/macro4")
macro5 <- from_xml_to_df("Corpus_Econlit/macro5")
macro6 <- from_xml_to_df("Corpus_Econlit/macro6")
macro7 <- from_xml_to_df("Corpus_Econlit/macro7")
macro8 <- from_xml_to_df("Corpus_Econlit/macro8")
macro9 <- from_xml_to_df("Corpus_Econlit/F3 JEL 1991-2012")
macro10 <- from_xml_to_df("Corpus_Econlit/F3 JEL 2013-2016")
dt_JEL_Articles <- rbind(macro1, macro2, macro3, macro4, macro5, macro6, macro7, macro8, macro9, macro10)
rm(macro1, macro2, macro3, macro4, macro5, macro6, macro7, macro8, macro9, macro10)

old_macro1 <- from_xml_to_df("Corpus_Econlit/Old_JEL_1969-1985")
old_macro2 <- from_xml_to_df("Corpus_Econlit/Old_JEL_1986-1990")
dt_Old_JEL_Articles <- rbind(old_macro1, old_macro2)
rm(old_macro1, old_macro2)
gc()


hist(as.numeric(dt_JEL_Articles$Year.V1))

saveRDS(dt_JEL_Articles, file = "Corpus_Econlit/dt_JEL_Articles.rds")
saveRDS(dt_Old_JEL_Articles, file = "Corpus_Econlit/dt_Old_JEL_Articles.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART II: MATCHING JEL WITH WOS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# We have 105 333 articles from Jel Codes.
# We find 35 378 with  year, journal name, pages number and volume number.
# We find 40 433 with cleaned up title
# Total of 44 833 unique articles

######################### Getting the BD ***************************

# all art
all_art <-  dbGetQuery(ESH, paste0("SELECT * FROM OST_Expanded_SciHum.Articles WHERE ItemID_Ref != 0;")) %>%  data.table
issueID <- fread("Corpus_Econlit/revueID.csv", quote="") %>% data.table()
# Disciplines and Journals
revues  <-  dbGetQuery(ESH, "SELECT Code_Revue, Revue, Code_Discipline FROM OST_Expanded_SciHum.Revues;") %>% data.table
# Get IssueID for matching
all_art <- merge(all_art, issueID[,.(IssueID,Volume)], by= "IssueID")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Matching on Issues ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

######################### Getting all_art running smoothly ***************************
all_art <- all_art[,.(ID_Art, Annee_Bibliographique, Title = Titre, Code_Revue, Page_Debut, Page_Fin, Nb_Page, ItemID_Ref, Volume)]
all_art <- merge(all_art, revues, by="Code_Revue")
all_art[,Revue := sub("\r","", Revue)] 
all_art <- all_art[,Pages := paste0(all_art$Page_Debut,"-",all_art$Page_Fin)] # formatting pages number
all_art <- all_art[, ID_by_pub := paste(all_art$Annee_Bibliographique, all_art$Revue, all_art$Pages, all_art$Volume)] # One variable for matching

######################### Do the same for dt of JEL ***************************
dt_JEL_Articles[,Journal := toupper(Journal)]
dt_JEL_Articles <- transform(dt_JEL_Articles, Year.V1 = as.numeric(Year.V1))
dt_JEL_Articles <- dt_JEL_Articles[, ID_by_pub := paste(dt_JEL_Articles$Year.V1, dt_JEL_Articles$Journal, dt_JEL_Articles$Pages, dt_JEL_Articles$Vol)]

######################### Merging everything ***************************
#JEL with bd to find article by their volume, pages, year, and journal
merging <- all_art[ID_by_pub %in% dt_JEL_Articles$ID_by_pub]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Matching on Titles ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
all_art$Title<-removePunctuation(all_art$Title)
all_art$Title<-tolower(all_art$Title)
all_art$Title<-stripWhitespace(all_art$Title)
all_art <- all_art[, ID_by_title := paste(all_art$Title, all_art$Annee_Bibliographique)]

dt_JEL_Articles$Title<-removePunctuation(dt_JEL_Articles$Title)
dt_JEL_Articles$Title<-tolower(dt_JEL_Articles$Title)
dt_JEL_Articles$Title<-stripWhitespace(dt_JEL_Articles$Title)
dt_JEL_Articles <- dt_JEL_Articles[, ID_by_title := paste(dt_JEL_Articles$Title, dt_JEL_Articles$Year.V1)]

merging2 <- all_art[ID_by_title %in% dt_JEL_Articles$ID_by_title]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Corpus Final Wos ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

base_corpus <- rbind(merging, merging2, fill = TRUE)
rm(all_art)
gc()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART III: WOS MATCHED CORPUS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
pswd = 'alex55Truc!1epistemo'
usr = 'alexandre'
ESH <- dbConnect(MySQL(), user=usr, password=pswd, dbname='OST_Expanded_SciHum',
                 host='127.0.0.1')

# all art and all refs
all_ref <-  dbGetQuery(ESH, paste0("SELECT OST_Expanded_SciHum.References7.New_id2, OST_Expanded_SciHum.References7.ItemID_Ref, OST_Expanded_SciHum.References7.Annee, OST_Expanded_SciHum.References7.Nom, OST_Expanded_SciHum.References7.ID_Art, OST_Expanded_SciHum.References7.Revue_Abbrege FROM OST_Expanded_SciHum.References7 WHERE New_id2 != 0;")) %>%  data.table
all_aut <-  dbGetQuery(ESH, paste0("SELECT OST_Expanded_SciHum.Articles.ID_Art, OST_Expanded_SciHum.Articles.Titre, OST_Expanded_SciHum.Articles.Annee_Bibliographique, OST_Expanded_SciHum.Articles.Code_Revue, OST_Expanded_SciHum.Articles.ItemID_Ref, OST_Expanded_SciHum.Auteurs.Nom, OST_Expanded_SciHum.Auteurs.Ordre
                                   FROM OST_Expanded_SciHum.Articles
                                   JOIN OST_Expanded_SciHum.Auteurs ON OST_Expanded_SciHum.Articles.ID_Art=OST_Expanded_SciHum.Auteurs.ID_Art")) %>%  data.table

revues  <-  dbGetQuery(ESH, "SELECT Code_Revue, Revue, Code_Discipline FROM OST_Expanded_SciHum.Revues;") %>% data.table
revues  <- revues %>% mutate(Code_Discipline=replace(Code_Discipline, Code_Discipline>=101 & Code_Discipline<=109, 101)) %>% data.table
disciplines  <-  dbGetQuery(ESH, "SELECT ESpecialite, Code_Discipline FROM OST_Expanded_SciHum.Disciplines;") %>% data.table

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Authors ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

JEL_matched_corpus_authors <- all_aut[ID_Art %in% base_corpus$ID_Art] 
saveRDS(JEL_matched_corpus_authors, file = "Corpus_Econlit_Matched_WoS/JEL_matched_corpus_authors.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Nodes ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# 43 521 nodes
JEL_matched_corpus_nodes <- all_aut[Ordre==1][ID_Art %in% base_corpus$ID_Art]
# Label column
JEL_matched_corpus_nodes <- JEL_matched_corpus_nodes[, name_short:=  gsub("-.*","",Nom)]
JEL_matched_corpus_nodes$name_short <- toupper(JEL_matched_corpus_nodes$name_short)
JEL_matched_corpus_nodes <- JEL_matched_corpus_nodes[,Label:=paste0(name_short,",",Annee_Bibliographique)]
JEL_matched_corpus_nodes[, c("name_short"):=NULL]
# Disciplines and journals
JEL_matched_corpus_nodes <- merge(JEL_matched_corpus_nodes, revues, by="Code_Revue", all.x = TRUE)
JEL_matched_corpus_nodes[,Revue := sub("\r","", Revue)]
JEL_matched_corpus_nodes <- merge(JEL_matched_corpus_nodes, disciplines, by="Code_Discipline", all.x = TRUE)

saveRDS(JEL_matched_corpus_nodes, file = "Corpus_Econlit_Matched_WoS/JEL_matched_corpus_nodes.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Edges ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# 1 397 008 edges
JEL_matched_corpus_edges <- all_ref[ID_Art %in% base_corpus$ID_Art]

saveRDS(JEL_matched_corpus_edges, file = "Corpus_Econlit_Matched_WoS/JEL_matched_corpus_edges.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Refs ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
rm(all_aut, all_ref)
gc()
all_art <-  dbGetQuery(ESH, paste0("SELECT * FROM OST_Expanded_SciHum.Articles WHERE ItemID_Ref != 0;")) %>%  data.table

JEL_matched_corpus_references <- merge(JEL_matched_corpus_edges, all_art[,.(ItemID_Ref, Titre, Code_Revue)], by = "ItemID_Ref", all.x = TRUE)
JEL_matched_corpus_references <- merge(JEL_matched_corpus_references, revues[,.(Code_Revue, Code_Discipline)], by="Code_Revue", all.x = TRUE)
JEL_matched_corpus_references <- merge(JEL_matched_corpus_references, disciplines, by="Code_Discipline", all.x = TRUE)
rm(all_art)
gc()

saveRDS(JEL_matched_corpus_references, file = "Corpus_Econlit_Matched_WoS/JEL_matched_corpus_references_info.rds")







#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART IV: EXTENDING THE CORPUS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

JEL_matched_corpus_nodes <- readRDS("Corpus_Econlit_Matched_WoS/JEL_matched_corpus_nodes.rds")
JEL_matched_corpus_edges <- readRDS("Corpus_Econlit_Matched_WoS/JEL_matched_corpus_edges.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Find the cores ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

######################### Treshold for the Core ***************************

core_tresholds <- JEL_matched_corpus_edges[ItemID_Ref!=0][,.N, ItemID_Ref]
setnames(core_tresholds,"N", "Tresholds")
core_tresholds <- core_tresholds[,.N,Tresholds][order(N)]
setnames(core_tresholds,"N", "Number_of_Articles")
core_tresholds[order(-Tresholds),Number_cum := cumsum(Number_of_Articles)]
core_tresholds[order(-Tresholds), Number_gr := 100 * (Number_cum - shift(Number_cum)) / shift(Number_cum)]

ggplot(core_tresholds, aes(x=Tresholds, y=Number_gr)) + 
  coord_cartesian(xlim = c(4, 15), ylim = c(0, 30)) +
  geom_bar(stat='identity') +
  stat_summary(fun=sum, geom="line", color="black") +
  stat_summary(fun=sum, geom="point", color="black") +
  geom_segment(aes(x = 10, y = 20, xend =9.2, yend = 15),
               arrow = arrow(length = unit(0.5, "cm"))) +
  ylab(label = "Growth Rate of Corpus") +
  ggsave("Corpus_Extended/tresholds_core.png", width=20, height=15, units = "cm")


######################### Find the core with a treshold of 9 -> 16 822 articles ***************************
core <- JEL_matched_corpus_edges[ItemID_Ref!=0][,.N, ItemID_Ref]
setnames(core,"N", "Tresholds")
core <- core[Tresholds >=9] # list of ItemID_Ref

######################### Find the core ID_Art 13 670 articles ***************************
all_art <-  dbGetQuery(ESH, paste0("SELECT * FROM OST_Expanded_SciHum.Articles WHERE ItemID_Ref != 0;")) %>%  data.table
core <- all_art[ItemID_Ref %in% core$ItemID_Ref] 
rm(all_art)
gc()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Extending the core with citations ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
all_ref <-  dbGetQuery(ESH, paste0("SELECT OST_Expanded_SciHum.References7.ItemID_Ref, OST_Expanded_SciHum.References7.ID_Art FROM OST_Expanded_SciHum.References7 WHERE ItemID_Ref != 0;")) %>%  data.table

######################### citations to core ***************************
citations_to_core <- all_ref[ItemID_Ref %in% core$ItemID_Ref]

######################### Treshold for the citations of 6. We only take articles that cite 6 articles from our core ***************************

citations_to_core_treshold <- citations_to_core[,.N, ID_Art]
setnames(citations_to_core_treshold,"N", "Tresholds")
citations_to_core_treshold <- citations_to_core_treshold[,.N,Tresholds][order(N)]
setnames(citations_to_core_treshold,"N", "Number_of_Articles")
citations_to_core_treshold[order(-Tresholds),Number_cum := cumsum(Number_of_Articles)]
citations_to_core_treshold[order(-Tresholds), Number_gr := 100 * (Number_cum - shift(Number_cum)) / shift(Number_cum)]

ggplot(citations_to_core_treshold, aes(x=Tresholds, y=Number_gr)) + 
  coord_cartesian(xlim = c(1, 15), ylim = c(0, 30)) +
  geom_bar(stat='identity') +
  stat_summary(fun=sum, geom="line", color="black") +
  stat_summary(fun=sum, geom="point", color="black") +
  geom_vline(xintercept = 5.5) +
  ylab(label = "Growth Rate of Corpus") +
  ggsave("Corpus_Extended/tresholds_citations_to_core.png", width=20, height=15, units = "cm")

######################### citations corpus (88 425) ***************************

citations_to_core <- citations_to_core[,.N, ID_Art]
setnames(citations_to_core,"N", "Tresholds")
citations_to_core <- citations_to_core[Tresholds>=6]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Extending the core with references ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

######################### references of core ***************************
references_of_core <- all_ref[ID_Art %in% core$ID_Art]

######################### Treshold for the references of core of 7. We only take references which are cited at least 7 times by our core ***************************

references_of_core_tresholds <- references_of_core[,.N, ItemID_Ref]
setnames(references_of_core_tresholds,"N", "Tresholds")
references_of_core_tresholds <- references_of_core_tresholds[,.N,Tresholds][order(N)]
setnames(references_of_core_tresholds,"N", "Number_of_Articles")
references_of_core_tresholds[order(-Tresholds),Number_cum := cumsum(Number_of_Articles)]
references_of_core_tresholds[order(-Tresholds), Number_gr := 100 * (Number_cum - shift(Number_cum)) / shift(Number_cum)]

ggplot(references_of_core_tresholds, aes(x=Tresholds, y=Number_gr)) + 
  coord_cartesian(xlim = c(1, 15), ylim = c(0, 30)) +
  geom_bar(stat='identity') +
  stat_summary(fun=sum, geom="line", color="black") +
  stat_summary(fun=sum, geom="point", color="black") +
  geom_vline(xintercept = 6.5) +
  ylab(label = "Growth Rate of Corpus") +
  ggsave("Corpus_Extended/tresholds_references_to_core.png", width=20, height=15, units = "cm")

######################### references corpus (8 213) ***************************

references_of_core <- references_of_core[,.N, ItemID_Ref]
setnames(references_of_core,"N", "Tresholds")
references_of_core <- references_of_core[Tresholds>=7]

######################### references corpus with ID_Art (6 819) ***************************
all_art <-  dbGetQuery(ESH, paste0("SELECT * FROM OST_Expanded_SciHum.Articles WHERE ItemID_Ref != 0;")) %>%  data.table
references_of_core <- all_art[ItemID_Ref %in% references_of_core$ItemID_Ref] 
rm(all_art)
gc()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART V: FINAL EXTENDED CORPUS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Final Corpus - 93 355 unique articles ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### references corpus (8 213) ***************************

all_aut <-  dbGetQuery(ESH, paste0("SELECT OST_Expanded_SciHum.Articles.ID_Art, OST_Expanded_SciHum.Articles.Titre, OST_Expanded_SciHum.Articles.Annee_Bibliographique, OST_Expanded_SciHum.Articles.Code_Revue, OST_Expanded_SciHum.Articles.ItemID_Ref, OST_Expanded_SciHum.Auteurs.Nom, OST_Expanded_SciHum.Auteurs.Ordre
                                   FROM OST_Expanded_SciHum.Articles
                                   JOIN OST_Expanded_SciHum.Auteurs ON OST_Expanded_SciHum.Articles.ID_Art=OST_Expanded_SciHum.Auteurs.ID_Art")) %>%  data.table
all_ref <-  dbGetQuery(ESH, paste0("SELECT OST_Expanded_SciHum.References7.New_id2, OST_Expanded_SciHum.References7.ItemID_Ref, OST_Expanded_SciHum.References7.Annee, OST_Expanded_SciHum.References7.Nom, OST_Expanded_SciHum.References7.ID_Art FROM OST_Expanded_SciHum.References7 WHERE New_id2 != 0;")) %>%  data.table
revues  <-  dbGetQuery(ESH, "SELECT Code_Revue, Revue, Code_Discipline FROM OST_Expanded_SciHum.Revues;") %>% data.table
revues  <- revues %>% mutate(Code_Discipline=replace(Code_Discipline, Code_Discipline>=101 & Code_Discipline<=109, 101)) %>% data.table
disciplines  <-  dbGetQuery(ESH, "SELECT ESpecialite, Code_Discipline FROM OST_Expanded_SciHum.Disciplines;") %>% data.table

corpus_extended <- rbind(core[,.(ID_Art)], citations_to_core[,.(ID_Art)], references_of_core[,.(ID_Art)])
corpus_extended <- unique(corpus_extended)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Authors ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

corpus_extended_authors <- all_aut[ID_Art %in% corpus_extended$ID_Art] 
saveRDS(corpus_extended_authors, file = "Corpus_Extended/corpus_extended_authors.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Nodes ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# 93 355 nodes
corpus_extended_nodes <- all_aut[Ordre==1][ID_Art %in% corpus_extended$ID_Art]
#Label column
corpus_extended_nodes <- corpus_extended_nodes[, name_short:=  gsub("-.*","",Nom)]
corpus_extended_nodes <- corpus_extended_nodes[,Label:=paste0(name_short,",",Annee_Bibliographique)]
corpus_extended_nodes[, c("name_short"):=NULL]

corpus_extended_nodes <- merge(corpus_extended_nodes, revues, by="Code_Revue", all.x = TRUE)
corpus_extended_nodes[,Revue := sub("\r","", Revue)]
corpus_extended_nodes <- merge(corpus_extended_nodes, disciplines, by="Code_Discipline", all.x = TRUE)

saveRDS(corpus_extended_nodes, file = "Corpus_Extended/corpus_extended_nodes.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Edges ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# 1 398 679 edges
corpus_extended_edges <- all_ref[ID_Art %in% corpus_extended$ID_Art]

saveRDS(corpus_extended_edges, file = "Corpus_Extended/corpus_extended_edges.rds")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Refs ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
rm(all_aut, all_ref)
gc()
all_art <-  dbGetQuery(ESH, paste0("SELECT * FROM OST_Expanded_SciHum.Articles WHERE ItemID_Ref != 0;")) %>%  data.table

JEL_matched_corpus_references <- merge(corpus_extended_edges, all_art[,.(ItemID_Ref, Titre, Code_Revue)], by = "ItemID_Ref", all.x = TRUE)
JEL_matched_corpus_references <- merge(JEL_matched_corpus_references, revues[,.(Code_Revue, Code_Discipline)], by="Code_Revue", all.x = TRUE)
JEL_matched_corpus_references <- merge(JEL_matched_corpus_references, disciplines, by="Code_Discipline", all.x = TRUE)
rm(all_art)
gc()

saveRDS(JEL_matched_corpus_references, file = "Corpus_Extended/corpus_extended_refs.rds")








#### * ####
#### **************************** ####
#### *****OLD JEL FROM HERE ***** ####
#### **************************** ####
#### * ####












#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART VI: MATCHING OLD JEL WITH WOS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# We have 30 354 articles from Jel Codes.
# We find 14 909 with  year, journal name, pages number and volume number.
# We find 6 652 with cleaned up title
# Total of 15 012 unique articles

######################### Getting the BD ***************************

# all art
all_art <-  dbGetQuery(ESH, paste0("SELECT * FROM OST_Expanded_SciHum.Articles WHERE ItemID_Ref != 0;")) %>%  data.table
issueID <- fread("Corpus_Econlit/revueID.csv", quote="") %>% data.table()
# Disciplines and Journals
revues  <-  dbGetQuery(ESH, "SELECT Code_Revue, Revue, Code_Discipline FROM OST_Expanded_SciHum.Revues;") %>% data.table
# Get IssueID for matching
all_art <- merge(all_art, issueID[,.(IssueID,Volume)], by= "IssueID")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Matching on Issues ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

######################### Getting all_art running smoothly ***************************
all_art <- all_art[,.(ID_Art, Annee_Bibliographique, Title = Titre, Code_Revue, Page_Debut, Page_Fin, Nb_Page, ItemID_Ref, Volume)]
all_art <- merge(all_art, revues, by="Code_Revue")
all_art[,Revue := sub("\r","", Revue)] 
all_art <- all_art[,Pages := paste0(all_art$Page_Debut,"-",all_art$Page_Fin)] # formatting pages number
all_art <- all_art[, ID_by_pub := paste(all_art$Annee_Bibliographique, all_art$Revue, all_art$Pages, all_art$Volume)] # One variable for matching

######################### Do the same for dt of JEL ***************************
dt_Old_JEL_Articles[,Journal := toupper(Journal)]
dt_Old_JEL_Articles <- transform(dt_Old_JEL_Articles, Year.V1 = as.numeric(Year.V1))
dt_Old_JEL_Articles <- dt_Old_JEL_Articles[, ID_by_pub := paste(dt_Old_JEL_Articles$Year.V1, dt_Old_JEL_Articles$Journal, dt_Old_JEL_Articles$Pages, dt_Old_JEL_Articles$Vol)]

######################### Merging everything ***************************
#JEL with bd to find article by their volume, pages, year, and journal
merging <- all_art[ID_by_pub %in% dt_Old_JEL_Articles$ID_by_pub]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Matching on Titles ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
all_art$Title<-removePunctuation(all_art$Title)
all_art$Title<-tolower(all_art$Title)
all_art$Title<-stripWhitespace(all_art$Title)
all_art <- all_art[, ID_by_title := paste(all_art$Title, all_art$Annee_Bibliographique)]

dt_Old_JEL_Articles$Title<-removePunctuation(dt_Old_JEL_Articles$Title)
dt_Old_JEL_Articles$Title<-tolower(dt_Old_JEL_Articles$Title)
dt_Old_JEL_Articles$Title<-stripWhitespace(dt_Old_JEL_Articles$Title)
dt_Old_JEL_Articles <- dt_Old_JEL_Articles[, ID_by_title := paste(dt_Old_JEL_Articles$Title, dt_Old_JEL_Articles$Year.V1)]

merging2 <- all_art[ID_by_title %in% dt_Old_JEL_Articles$ID_by_title]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Corpus Final Wos ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

base_corpus <- rbind(merging, merging2, fill = TRUE)
rm(all_art)
gc()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART VII: WOS MATCHED OLD CORPUS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# all art and all refs
all_ref <-  dbGetQuery(ESH, paste0("SELECT OST_Expanded_SciHum.References7.New_id2, OST_Expanded_SciHum.References7.ItemID_Ref, OST_Expanded_SciHum.References7.Annee, OST_Expanded_SciHum.References7.Nom, OST_Expanded_SciHum.References7.ID_Art, OST_Expanded_SciHum.References7.Revue_Abbrege FROM OST_Expanded_SciHum.References7 WHERE New_id2 != 0;")) %>%  data.table
all_aut <-  dbGetQuery(ESH, paste0("SELECT OST_Expanded_SciHum.Articles.ID_Art, OST_Expanded_SciHum.Articles.Titre, OST_Expanded_SciHum.Articles.Annee_Bibliographique, OST_Expanded_SciHum.Articles.Code_Revue, OST_Expanded_SciHum.Articles.ItemID_Ref, OST_Expanded_SciHum.Auteurs.Nom, OST_Expanded_SciHum.Auteurs.Ordre
                                   FROM OST_Expanded_SciHum.Articles
                                   JOIN OST_Expanded_SciHum.Auteurs ON OST_Expanded_SciHum.Articles.ID_Art=OST_Expanded_SciHum.Auteurs.ID_Art")) %>%  data.table

revues  <-  dbGetQuery(ESH, "SELECT Code_Revue, Revue, Code_Discipline FROM OST_Expanded_SciHum.Revues;") %>% data.table
revues  <- revues %>% mutate(Code_Discipline=replace(Code_Discipline, Code_Discipline>=101 & Code_Discipline<=109, 101)) %>% data.table
disciplines  <-  dbGetQuery(ESH, "SELECT ESpecialite, Code_Discipline FROM OST_Expanded_SciHum.Disciplines;") %>% data.table

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Authors ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

JEL_matched_corpus_authors <- all_aut[ID_Art %in% base_corpus$ID_Art] 
saveRDS(JEL_matched_corpus_authors, file = "Corpus_Econlit_Matched_WoS/Old_JEL_matched_corpus_authors.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Nodes ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# 16 222 nodes
JEL_matched_corpus_nodes <- all_aut[Ordre==1][ID_Art %in% base_corpus$ID_Art]
# Label column
JEL_matched_corpus_nodes <- JEL_matched_corpus_nodes[, name_short:=  gsub("-.*","",Nom)]
JEL_matched_corpus_nodes$name_short <- toupper(JEL_matched_corpus_nodes$name_short)
JEL_matched_corpus_nodes <- JEL_matched_corpus_nodes[,Label:=paste0(name_short,",",Annee_Bibliographique)]
JEL_matched_corpus_nodes[, c("name_short"):=NULL]
# Disciplines and journals
JEL_matched_corpus_nodes <- merge(JEL_matched_corpus_nodes, revues, by="Code_Revue", all.x = TRUE)
JEL_matched_corpus_nodes[,Revue := sub("\r","", Revue)]
JEL_matched_corpus_nodes <- merge(JEL_matched_corpus_nodes, disciplines, by="Code_Discipline", all.x = TRUE)

saveRDS(JEL_matched_corpus_nodes, file = "Corpus_Econlit_Matched_WoS/Old_JEL_matched_corpus_nodes.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Edges ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# 272 650 edges
JEL_matched_corpus_edges <- all_ref[ID_Art %in% base_corpus$ID_Art]

saveRDS(JEL_matched_corpus_edges, file = "Corpus_Econlit_Matched_WoS/Old_JEL_matched_corpus_edges.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Refs ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
rm(all_aut, all_ref)
gc()
all_art <-  dbGetQuery(ESH, paste0("SELECT * FROM OST_Expanded_SciHum.Articles WHERE ItemID_Ref != 0;")) %>%  data.table

JEL_matched_corpus_references <- merge(JEL_matched_corpus_edges, all_art[,.(ItemID_Ref, Titre, Code_Revue)], by = "ItemID_Ref", all.x = TRUE)
JEL_matched_corpus_references <- merge(JEL_matched_corpus_references, revues[,.(Code_Revue, Code_Discipline)], by="Code_Revue", all.x = TRUE)
JEL_matched_corpus_references <- merge(JEL_matched_corpus_references, disciplines, by="Code_Discipline", all.x = TRUE)
rm(all_art)
gc()

saveRDS(JEL_matched_corpus_references, file = "Corpus_Econlit_Matched_WoS/Old_JEL_matched_corpus_references_info.rds")






#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART VIII: EXTENDING THE OLD CORPUS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

JEL_matched_corpus_nodes <- readRDS("Corpus_Econlit_Matched_WoS/Old_JEL_matched_corpus_nodes.rds")
JEL_matched_corpus_edges <- readRDS("Corpus_Econlit_Matched_WoS/Old_JEL_matched_corpus_edges.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Find the cores ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

######################### Treshold for the Core ***************************

core_tresholds <- JEL_matched_corpus_edges[ItemID_Ref!=0][,.N, ItemID_Ref]
setnames(core_tresholds,"N", "Tresholds")
core_tresholds <- core_tresholds[,.N,Tresholds][order(N)]
setnames(core_tresholds,"N", "Number_of_Articles")
core_tresholds[order(-Tresholds),Number_cum := cumsum(Number_of_Articles)]
core_tresholds[order(-Tresholds), Number_gr := 100 * (Number_cum - shift(Number_cum)) / shift(Number_cum)]

ggplot(core_tresholds, aes(x=Tresholds, y=Number_gr)) + 
  coord_cartesian(xlim = c(4, 15), ylim = c(0, 30)) +
  geom_bar(stat='identity') +
  stat_summary(fun=sum, geom="line", color="black") +
  stat_summary(fun=sum, geom="point", color="black") +
  geom_segment(aes(x = 10, y = 20, xend =9.2, yend = 15),
               arrow = arrow(length = unit(0.5, "cm"))) +
  ylab(label = "Growth Rate of Corpus") +
  ggsave("Corpus_Extended/tresholds_old_core.png", width=20, height=15, units = "cm")


######################### Find the core with a treshold of 9 -> 1 929 articles ***************************
core <- JEL_matched_corpus_edges[ItemID_Ref!=0][,.N, ItemID_Ref]
setnames(core,"N", "Tresholds")
core <- core[Tresholds >=9] # list of ItemID_Ref

######################### Find the core ID_Art 13 670 articles ***************************
all_art <-  dbGetQuery(ESH, paste0("SELECT * FROM OST_Expanded_SciHum.Articles WHERE ItemID_Ref != 0;")) %>%  data.table
core <- all_art[ItemID_Ref %in% core$ItemID_Ref] 
rm(all_art)
gc()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Extending the core with citations ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
all_ref <-  dbGetQuery(ESH, paste0("SELECT OST_Expanded_SciHum.References7.ItemID_Ref, OST_Expanded_SciHum.References7.ID_Art FROM OST_Expanded_SciHum.References7 WHERE ItemID_Ref != 0;")) %>%  data.table

######################### citations to core ***************************
citations_to_core <- all_ref[ItemID_Ref %in% core$ItemID_Ref]

######################### Treshold for the citations of 9. We only take articles that cite 9 articles from our core ***************************

citations_to_core_treshold <- citations_to_core[,.N, ID_Art]
setnames(citations_to_core_treshold,"N", "Tresholds")
citations_to_core_treshold <- citations_to_core_treshold[,.N,Tresholds][order(N)]
setnames(citations_to_core_treshold,"N", "Number_of_Articles")
citations_to_core_treshold[order(-Tresholds),Number_cum := cumsum(Number_of_Articles)]
citations_to_core_treshold[order(-Tresholds), Number_gr := 100 * (Number_cum - shift(Number_cum)) / shift(Number_cum)]

ggplot(citations_to_core_treshold, aes(x=Tresholds, y=Number_gr)) + 
  coord_cartesian(xlim = c(1, 15), ylim = c(0, 50)) +
  geom_bar(stat='identity') +
  stat_summary(fun=sum, geom="line", color="black") +
  stat_summary(fun=sum, geom="point", color="black") +
  geom_vline(xintercept = 8.5) +
  ylab(label = "Growth Rate of Corpus") +
  ggsave("Corpus_Extended/tresholds_citations_to_old_core.png", width=20, height=15, units = "cm")

######################### citations corpus (3 933) ***************************

citations_to_core <- citations_to_core[,.N, ID_Art]
setnames(citations_to_core,"N", "Tresholds")
citations_to_core <- citations_to_core[Tresholds>=9]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Extending the core with references ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

######################### references of core ***************************
references_of_core <- all_ref[ID_Art %in% core$ID_Art]

######################### Treshold for the references of core of 7. We only take references which are cited at least 7 times by our core ***************************

references_of_core_tresholds <- references_of_core[,.N, ItemID_Ref]
setnames(references_of_core_tresholds,"N", "Tresholds")
references_of_core_tresholds <- references_of_core_tresholds[,.N,Tresholds][order(N)]
setnames(references_of_core_tresholds,"N", "Number_of_Articles")
references_of_core_tresholds[order(-Tresholds),Number_cum := cumsum(Number_of_Articles)]
references_of_core_tresholds[order(-Tresholds), Number_gr := 100 * (Number_cum - shift(Number_cum)) / shift(Number_cum)]

ggplot(references_of_core_tresholds, aes(x=Tresholds, y=Number_gr)) + 
  coord_cartesian(xlim = c(1, 15), ylim = c(0, 40)) +
  geom_bar(stat='identity') +
  stat_summary(fun=sum, geom="line", color="black") +
  stat_summary(fun=sum, geom="point", color="black") +
  geom_vline(xintercept = 4.5) +
  ylab(label = "Growth Rate of Corpus") +
  ggsave("Corpus_Extended/tresholds_references_to_old_core.png", width=20, height=15, units = "cm")

######################### references corpus (878) ***************************

references_of_core <- references_of_core[,.N, ItemID_Ref]
setnames(references_of_core,"N", "Tresholds")
references_of_core <- references_of_core[Tresholds>=5]

######################### references corpus with ID_Art (696) ***************************
all_art <-  dbGetQuery(ESH, paste0("SELECT * FROM OST_Expanded_SciHum.Articles WHERE ItemID_Ref != 0;")) %>%  data.table
references_of_core <- all_art[ItemID_Ref %in% references_of_core$ItemID_Ref] 
rm(all_art)
gc()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART IX: FINAL EXTENDED OLD CORPUS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Final Corpus - 93 355 unique articles ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### references corpus (8 213) ***************************

all_aut <-  dbGetQuery(ESH, paste0("SELECT OST_Expanded_SciHum.Articles.ID_Art, OST_Expanded_SciHum.Articles.Titre, OST_Expanded_SciHum.Articles.Annee_Bibliographique, OST_Expanded_SciHum.Articles.Code_Revue, OST_Expanded_SciHum.Articles.ItemID_Ref, OST_Expanded_SciHum.Auteurs.Nom, OST_Expanded_SciHum.Auteurs.Ordre
                                   FROM OST_Expanded_SciHum.Articles
                                   JOIN OST_Expanded_SciHum.Auteurs ON OST_Expanded_SciHum.Articles.ID_Art=OST_Expanded_SciHum.Auteurs.ID_Art")) %>%  data.table
all_ref <-  dbGetQuery(ESH, paste0("SELECT OST_Expanded_SciHum.References7.New_id2, OST_Expanded_SciHum.References7.ItemID_Ref, OST_Expanded_SciHum.References7.Annee, OST_Expanded_SciHum.References7.Nom, OST_Expanded_SciHum.References7.ID_Art FROM OST_Expanded_SciHum.References7 WHERE New_id2 != 0;")) %>%  data.table
revues  <-  dbGetQuery(ESH, "SELECT Code_Revue, Revue, Code_Discipline FROM OST_Expanded_SciHum.Revues;") %>% data.table
revues  <- revues %>% mutate(Code_Discipline=replace(Code_Discipline, Code_Discipline>=101 & Code_Discipline<=109, 101)) %>% data.table
disciplines  <-  dbGetQuery(ESH, "SELECT ESpecialite, Code_Discipline FROM OST_Expanded_SciHum.Disciplines;") %>% data.table

corpus_extended <- rbind(core[,.(ID_Art)], citations_to_core[,.(ID_Art)], references_of_core[,.(ID_Art)])
corpus_extended <- unique(corpus_extended)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Authors ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

corpus_extended_authors <- all_aut[ID_Art %in% corpus_extended$ID_Art] 
saveRDS(corpus_extended_authors, file = "Corpus_Extended/Old_corpus_extended_authors.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Nodes ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# 93 355 nodes
corpus_extended_nodes <- all_aut[Ordre==1][ID_Art %in% corpus_extended$ID_Art]
#Label column
corpus_extended_nodes <- corpus_extended_nodes[, name_short:=  gsub("-.*","",Nom)]
corpus_extended_nodes <- corpus_extended_nodes[,Label:=paste0(name_short,",",Annee_Bibliographique)]
corpus_extended_nodes[, c("name_short"):=NULL]

corpus_extended_nodes <- merge(corpus_extended_nodes, revues, by="Code_Revue", all.x = TRUE)
corpus_extended_nodes[,Revue := sub("\r","", Revue)]
corpus_extended_nodes <- merge(corpus_extended_nodes, disciplines, by="Code_Discipline", all.x = TRUE)

saveRDS(corpus_extended_nodes, file = "Corpus_Extended/Old_corpus_extended_nodes.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Edges ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# 1 398 679 edges
corpus_extended_edges <- all_ref[ID_Art %in% corpus_extended$ID_Art]

saveRDS(corpus_extended_edges, file = "Corpus_Extended/Old_corpus_extended_edges.rds")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Refs ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
rm(all_aut, all_ref)
gc()
all_art <-  dbGetQuery(ESH, paste0("SELECT * FROM OST_Expanded_SciHum.Articles WHERE ItemID_Ref != 0;")) %>%  data.table

JEL_matched_corpus_references <- merge(corpus_extended_edges, all_art[,.(ItemID_Ref, Titre, Code_Revue)], by = "ItemID_Ref", all.x = TRUE)
JEL_matched_corpus_references <- merge(JEL_matched_corpus_references, revues[,.(Code_Revue, Code_Discipline)], by="Code_Revue", all.x = TRUE)
JEL_matched_corpus_references <- merge(JEL_matched_corpus_references, disciplines, by="Code_Discipline", all.x = TRUE)
rm(all_art)
gc()

saveRDS(JEL_matched_corpus_references, file = "Corpus_Extended/Old_corpus_extended_refs.rds")










#### * ####
#### **************************** ####
#### ***** all_id_art ***** ####
#### **************************** ####
#### * ####



ID_Art1 <- readRDS("Corpus_Econlit_Matched_WoS/JEL_matched_corpus_nodes.rds")
ID_Art2 <- readRDS("Corpus_Econlit_Matched_WoS/JEL_matched_corpus_references_info.rds")

ID_Art3 <- readRDS("Corpus_Econlit_Matched_WoS/Old_JEL_matched_corpus_nodes.rds")
ID_Art4 <- readRDS("Corpus_Econlit_Matched_WoS/Old_JEL_matched_corpus_references_info.rds")

ID_Art5 <- readRDS("Corpus_Extended/corpus_extended_nodes.rds")
ID_Art6 <- readRDS("Corpus_Extended/corpus_extended_refs.rds")

ID_Art7 <- readRDS("Corpus_Extended/Old_corpus_extended_nodes.rds")
ID_Art8 <- readRDS("Corpus_Extended/Old_corpus_extended_refs.rds")

all_ID_Art <- rbind(ID_Art1, ID_Art2, ID_Art3, ID_Art4, ID_Art5, ID_Art6, ID_Art7, ID_Art8, fill=TRUE)
write.csv(all_ID_Art[,.N,ID_Art][order(N)], file = "all_ID_Art.csv", row.names=FALSE)
