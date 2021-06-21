install.packages("searcher")
require(searcher)
install.packages("rvest")
require(rvest)
require(tidyverse)
require(data.table)
require(tm)
setwd("/projects/data/macro_AA")
require(bib2df)



# scrap_jel <- function(Titre=Titre,Corpus=Corpus_titles){
#   title_to_match <- Titre
#   id_art_name <- Corpus[Titre==title_to_match]$ID_Art
#   
#   url <- paste0("https://www.google.com/search?q=",Titre)
#   # url <- str_replace(url, "\\\\", "")
#   
#   first_page <- read_html(url)
#   titles <- html_nodes(first_page, xpath = "//div/div/div/a") %>% html_attr('href') 
#   titles <- titles[titles %like% "www.sciencedirect.com"]
#   titles <- str_replace(titles, "\\/url\\?q\\=", "")
#   titles <- titles[str_detect(titles,"pdf")==FALSE]
#   
#   if(identical(titles, character(0))==FALSE)
#   {titles <- str_split(titles, "&")[[1]][1]
#   
#   url = paste(titles)
#   jel <- read_html(url)
#   jel <- html_nodes(jel, ".keyword") %>% html_text() %>% as.data.table() %>% rename(jel_codes = ".")
#   jel <- jel[str_detect(jel_codes,"[:digit:]")]
#   jel[,ID_Art:=id_art_name[1]]
#   } else{jel="0000"}
#   closeAllConnections()
#   gc()
#   # Sys.sleep(1)
#   return(jel)
# }
# 
# 
# 
# Corpus <- readRDS(file = "EER/1_Corpus_Prepped_and_Merged/Corpus.rds")
# Corpus_titles <- copy(Corpus[Annee_Bibliographique>=1995,.(Titre, ID_Art)])
# Corpus_titles[,Titre:=str_replace_all(Titre," ", "+")]
# 
# all_jel <- lapply(Corpus_titles[1:1000]$Titre, scrap_jel)
# all_jel2 <- lapply(Corpus_titles[1000:1750]$Titre, scrap_jel)
# all_jel3 <- lapply(Corpus_titles[1751:2296]$Titre, scrap_jel)










all_art <-  dbGetQuery(ESH, paste0("SELECT * 
                                   FROM OST_Expanded_SciHum.Articles 
                                   WHERE Code_Revue=5200;")) %>%  data.table
issueID <- fread("Corpus_Econlit/revueID.csv", quote = "") %>% data.table()
EER_jel <- bib2df("EER/Corpus_EER/EER_corpus_JEL.bib") %>% as.data.table()
EER_jel <- EER_jel[,.(VOLUME,YEAR,KEYWORDS,JOURNAL,PAGES,NUMBER)]
# all art
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
EER_jel[, JOURNAL := toupper(JOURNAL)]
EER_jel <- transform(EER_jel, YEAR = as.numeric(YEAR))
EER_jel[,PAGES:=str_replace_all(PAGES, " ","")]
EER_jel <- EER_jel[, ID_by_pub := paste(EER_jel$YEAR, EER_jel$JOURNAL, EER_jel$PAGES, EER_jel$VOLUME)]

######################### Merging everything ***************************
# JEL with bd to find article by their volume, pages, year, and journal
merging <- all_art[ID_by_pub %in% EER_jel$ID_by_pub]
merging <- merge(merging[,.(ID_by_pub,ID_Art)], EER_jel[,.(ID_by_pub,KEYWORDS)], by="ID_by_pub")
merging <- merging[,.(ID_Art,KEYWORDS)]
merging[, KEYWORDS := str_replace_all(KEYWORDS, "(\\b[^\\s\\d]+\\b)","")]
merging[, KEYWORDS := str_replace_all(KEYWORDS, "[:punct:]","")]
merging[,KEYWORDS := stripWhitespace(KEYWORDS)]
merging[,KEYWORDS := str_replace_all(KEYWORDS,"^\\s","")]
merging[, paste0("jel_codes", 1:11) := tstrsplit(KEYWORDS, " ")]
merging[,KEYWORDS:=NULL]
jel_codes <- melt(merging, id.vars=c("ID_Art"))
jel_codes <-jel_codes[is.na(value)==FALSE]
saveRDS(jel_codes, file = paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Jel_info.rds"))



