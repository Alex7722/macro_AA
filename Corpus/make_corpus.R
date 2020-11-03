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
macro1 <- from_xml_to_df("Corpus_Econlit/macro1")
macro2 <- from_xml_to_df("Corpus_Econlit/macro2")
macro3 <- from_xml_to_df("Corpus_Econlit/macro3")
macro4 <- from_xml_to_df("Corpus_Econlit/macro4")
macro5 <- from_xml_to_df("Corpus_Econlit/macro5")
macro6 <- from_xml_to_df("Corpus_Econlit/macro6")
macro7 <- from_xml_to_df("Corpus_Econlit/macro7")
macro8 <- from_xml_to_df("Corpus_Econlit/macro8")
dt_JEL_Articles <- rbind(macro1, macro2, macro3, macro4, macro5, macro6, macro7, macro8)
rm(macro1, macro2, macro3, macro4, macro5, macro6, macro7, macro8)
gc()

hist(as.numeric(dt_JEL_Articles$Year.V1))

saveRDS(dt_JEL_Articles, file = "Corpus_Econlit/dt_JEL_Articles.rds")

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

dt_JEL_Articles$Title<-removePunctuation(dt_JEL_Articles$Title)
dt_JEL_Articles$Title<-tolower(dt_JEL_Articles$Title)
dt_JEL_Articles$Title<-stripWhitespace(dt_JEL_Articles$Title)

merging2 <- all_art[Title %in% dt_JEL_Articles$Title][Annee_Bibliographique>=1991]
merging2 <- merging2[, n_words:=sapply(strsplit(Title, " "), length)]
merging2 <- merging2[n_words>2]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Corpus Final ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

base_corpus <- rbind(merging, merging2, fill = TRUE)
rm(all_art)
gc()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART III: WOS MATCHED CORPUS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# all art and all refs
all_ref <-  dbGetQuery(ESH, paste0("SELECT OST_Expanded_SciHum.References7.New_id2, OST_Expanded_SciHum.References7.Annee, OST_Expanded_SciHum.References7.Nom, OST_Expanded_SciHum.References7.ID_Art FROM OST_Expanded_SciHum.References7 WHERE New_id2 != 0;")) %>%  data.table
all_aut <-  dbGetQuery(ESH, paste0("SELECT OST_Expanded_SciHum.Articles.ID_Art, OST_Expanded_SciHum.Articles.Titre, OST_Expanded_SciHum.Articles.Annee_Bibliographique, OST_Expanded_SciHum.Articles.Code_Revue, OST_Expanded_SciHum.Articles.ItemID_Ref, OST_Expanded_SciHum.Auteurs.Nom, OST_Expanded_SciHum.Auteurs.Ordre
                                   FROM OST_Expanded_SciHum.Articles
                                   JOIN OST_Expanded_SciHum.Auteurs ON OST_Expanded_SciHum.Articles.ID_Art=OST_Expanded_SciHum.Auteurs.ID_Art")) %>%  data.table

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Nodes ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# 43 833 nodes
JEL_matched_corpus_nodes <- all_aut[Ordre==1][ID_Art %in% base_corpus$ID_Art]
#Label column
JEL_matched_corpus_nodes <- JEL_matched_corpus_nodes[, name_short:=  gsub("-.*","",Nom)]
JEL_matched_corpus_nodes <- JEL_matched_corpus_nodes[,Label:=paste0(name_short,",",Annee_Bibliographique)]
JEL_matched_corpus_nodes[, c("name_short"):=NULL]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Edges ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# 929 100 edges
JEL_matched_corpus_edges <- all_ref[ID_Art %in% base_corpus$ID_Art]

saveRDS(JEL_matched_corpus_nodes, file = "Corpus_Econlit_Matched_WoS/JEL_matched_corpus_nodes.rds")
saveRDS(JEL_matched_corpus_edges, file = "Corpus_Econlit_Matched_WoS/JEL_matched_corpus_edges.rds")


hist(JEL_matched_corpus_nodes$Annee_Bibliographique)
