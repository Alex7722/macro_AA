#' ---
#' title: "New Script for cleaning the missing articles and references extracting in Scopus"
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
#' This script aims at cleaning scopus data. The input is a .txt with articles metadata and their references
#' extracted from scopus. It aims at producing a data.frame that we will merge with WoS data.
#' 
#' 
#' > WARNING: This script still needs a lot of cleaning
#' 
#' 
#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)

#' # Loading packages, paths and data
#' 


library(tidyverse)

eer_data <- "/projects/data/macro_AA/EER/Corpus_EER/"

# Import list of articles extracted from SCOPUS

scopus <- read_delim(paste0(eer_data,"EER_scopus.txt"), "\t", escape_double = FALSE, trim_ws = TRUE) %>% data.table()

# removing useless lines
remove_lines <- c("ISSN","LANGUAGE OF","ABBREVIATED SOURCE","DOCUMENT TYPE","PUBLICATION STAGE","DOI","ABSTRACT","SOURCE","OPEN ACCESS","https:")

for(i in 1:length(remove_lines)){
delete <- which(str_detect(scopus$Scopus, pattern = remove_lines[i]))
scopus <- scopus[-delete]
}


# identifying ids
scopus$id <- str_detect(scopus$Scopus, pattern = "[:digit:]{6,}")

# identifying authors
name <- which(scopus$id == TRUE) - 1

scopus$author <- FALSE
scopus[name]$author <- TRUE 

#identifying title
titre <- which(scopus$id == TRUE) + 1

scopus$title <- FALSE
scopus[titre]$title <- TRUE 

#identifying informations of the article
info <- which(scopus$id == TRUE) + 2

scopus$info_art <- FALSE
scopus[info]$info_art <- TRUE 

dt <- cbind(scopus[id == TRUE]$Scopus, 
            scopus[name]$Scopus, 
            scopus[titre]$Scopus,
            scopus[info]$Scopus)
