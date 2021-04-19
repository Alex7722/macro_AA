#' ---
#' title: "Script for extracting abstracts of EER articles from scopus"
#' author: "Aurélien Goutsmedt"
#' date: "/ Last compiled on `r format(Sys.Date())`"
#' output: 
#'   github_document:
#'     toc: true
#'     number_sections: true
#' ---
#' 
#' # What is this script for?
#' 
#' This script aims at extracting the data of all EER articles from scopus, notably the abstract. 
#' The resulting data table will be merged with the corpus extracted from WoS, in order to use
#' textual analysis on abstracts.
#' 
#' 
#' 
#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)

#' # Loading packages, paths and data
#' 

library(tidyverse)
library(data.table)
eer_data <- "/projects/data/macro_AA/EER/Corpus_EER/"
source("~/macro_AA/functions/functions_for_cleaning_strings.R")

#' We import the list of articles extracted from SCOPUS

scopus <- read_delim(paste0(eer_data,"EER_scopus_abstract.txt"), "\t", escape_double = FALSE, trim_ws = TRUE) %>% data.table()

#' # Cleaning scopus data
#' 
#' ## Cleaning the .txt file
#' We need first to extract the relevant lines to put them in a data frame (authors, title, volume, number, pages, year, abstracts).
#' 
#' 
#' #### Identifying ids
#' Each time you find a series of number, you add one to a column name `temp_id`, what allows you 
#' to identify each article (you just need to move it one row above to integrate the name of authors
#' which is above the serie of numbers).

temp_id <- scopus %>% 
  mutate(temp_id = cumsum(str_detect(scopus$Scopus, pattern = "^[:digit:]{8,}"))) %>% 
  select(temp_id)
  
scopus <- scopus %>% 
  mutate(temp_id = c(temp_id[-1]$temp_id,max(temp_id)))

#' #### Identifying authors
scopus$id <- str_detect(scopus$Scopus, pattern = "^[:digit:]{8,}") # We need this to identify the authors then
name <- which(scopus$id == TRUE) - 1 # this saves the position in the text of authors names (one line above the id)

#scopus$author <- FALSE # useful later for abstracts
#scopus[name]$author <- TRUE
#' #### identifying title
titre <- which(scopus$id == TRUE) + 1

#' #### Identifying informations on the article (journal, volume, etc.)
info <- which(scopus$id == TRUE) + 2

#' #### Extracting abstracts
#' 
#' You don't have an abstract for each article, so we extract the id of the articles with an abstract,
#' and the corresponding abstract, to merge them just after.

abstract <- which(str_detect(scopus$Scopus, pattern = "ABSTRACT"))
abstract_list <- data.table(temp_id = scopus[abstract]$temp_id, abstract = scopus[abstract]$Scopus)


#' #### Creating the data frame of articles
#' 
#' We extract the information we need for each article and merge with the abstracts. As we are
#' interest in abstract content, we don't take articles without abstracts. 

scopus_art <- data.table("temp_id" = unique(scopus$temp_id),
                         "author" = scopus[name]$Scopus, 
                         "title" = scopus[titre]$Scopus,
                         "info" = scopus[info]$Scopus)

scopus_art <- merge(scopus_art,abstract_list, by = "temp_id", all.x = TRUE)

#' We remove the lines which are articles from the "European Economic Review of Economic History",
#' and not from the EER.
scopus_art <- scopus_art[-which(str_detect(info, "Economic History"))]

#' ## Cleaning the corpus
#' 
#' Now, we just have to clean a bit the data. We need to keep the first author and put it
#' in the WoS format (SURNAME-F), to clean journal and volume info.
#' 
#' ### Cleaning `author`
#' 
#' We need to separate surnames and initials for each authors.
#' 
#' The first step is to calculate the maximum number of authors for an article. We use the number
#' of comma in a row to calculate this (one comma, separating surname and initial means 1 author,
#' 3 commas mean 2 authors, etc.). Then we can create the different columns to put the surnames 
#' and initials in.

nb_authors <- max((str_count(scopus_art$author, ",") + 1)/2)
name_column <- paste0(c("surname_","initial_"),rep(1:nb_authors, each = 2))

scopus_art  <- scopus_art %>% 
  separate(author, name_column, ",")

#' We want to avoid NA initials and we want to clean this as soon as possible, thus we check if we have 
#' missing values

test  <-  scopus_art %>% 
  filter(!is.na(surname_1) & is.na(initial_1)) %>% 
  select(surname_1)
if(length(test$surname_1) > 0){
  message("Missing values for initials")
} else {
  message("No missing values for initials")
}

scopus_art[surname_1 == test$surname_1]$initial_1  <- "S"
scopus_art[surname_1 == test$surname_1]$surname_1  <- "Telphlluch"

#' we now want to create authors column like in WoS
# cleaning by removing space, punctuation, and fusioning surnames and initials
scopus_art <- scopus_art %>% 
  select(temp_id, title, info, surname_1, initial_1, info, abstract) %>%
  mutate(across(contains("surname"), ~ remove_punct(remove_space(.x, replacement = "-"))),
         across(contains("initial"), ~ remove_punct(remove_space(.x))),
         across(contains("surname"), ~ str_replace(.x,"^-","")),
         across(contains("initial"), ~ str_extract(.x, "^[A-z]{1}"))) %>% 
  .[, Nom_ISI := toupper(paste0(surname_1,"-",initial_1))] %>% 
  .[, -..name_column[1:2]]

#' We need to replace special characters by "normal" characters, as in 
#' WoS.
#' 
special_character <- c("Ø","Ó","Ö","Ä","È","É","Ñ","Í","Ü")
normal_character <- c("O","O","O","A","E","E","N","I","U")

for(i in seq_along(special_character)){
scopus_art <- scopus_art %>% 
  mutate(Nom_ISI, str_replace_all(Nom_ISI, special_character[i], normal_character[i]))
}

#' we now need to remove any punctuation character in the name.

#' ### Cleaning `scopus_art`
#' 
#' We need to put the year, the Review (which is the same in our case), the volume, the number, and the 
#' pages in separated columns, and delete the number of citations.

scopus_art[, `:=` (Titre = toupper(title), # useful for later
                   Annee_Bibliographique = extract_year(info),
                   Journal = "European Economic Review",
                   info = str_remove(str_remove(info, ".*Review, "), ". Cited .*"))] %>% 
  .[, `:=` (Volume = str_extract(info, "^.[:digit:]{1,2}"),
            Issue = str_remove_all(str_extract(info, "\\([:digit:]{1,2}\\)|\\([:digit:]{1}-[:digit:]{1}\\)"),"[\\(\\)]"),
            Pages = str_remove(str_remove(str_extract(info, "pp.*"), "pp. "), "\\."))] # We first extract the number following pp., then we remove pp. and the final point.

#' Removing what are not articles
not_article <- c("TREASURER","COMMITTEE","SECRETARY","CHAIRMAN","EDITORS","PRESIDENT")
scopus_art <- scopus_art[!(str_detect(Titre, "REPORT") & str_detect(Titre, paste0(not_article, collapse = "|")))]

#' The `scopus_art` is now clean, and we can save it !
saveRDS(scopus_art[,c("temp_id","Nom_ISI","Annee_Bibliographique","Titre","Journal","Volume","Issue","Pages","abstract")], paste0(eer_data,"scopus_abstract.RDS"))
