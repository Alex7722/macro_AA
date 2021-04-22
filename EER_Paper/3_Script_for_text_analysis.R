#' ---
#' title: "Script for working on titles and abstracts of EER articles"
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
#' This script takes the `alluv_dt` data built in 
#' [2_Script_EER_dynamic_networks.md](/EER_Paper/2_Script_EER_dynamic_networks.md) 
#' and merge it with titles and abstracts of corresponding articles. We can then
#' work on the words associated to each article, and notably calculate highest 
#' tf-idf for communities and windows.
#' 
#' 
#' > WARNING: This script still needs a lot of cleaning
#' 
#' 
#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)

#' # Loading packages, paths and data
#' 
#' 
source("~/macro_AA/EER_Paper/Script_paths_and_basic_objects_EER.R")
source("~/macro_AA/functions/functions_for_network_analysis.R")
Corpus <- readRDS(paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Corpus.rds"))
alluv_dt <- readRDS(paste0(data_path,"EER/2_Raw_Networks_and_Alluv/alluv_dt.rds"))


#' # TF-IDF at the community level
#' 
#' We merge the alluv data table with the abstract in the corpus. Then we unite titles
#' and abstracts for each article, before to compute the tf-idf with communities as
#' "documents". 
alluv_with_abstract <- merge(alluv_dt, Corpus[, c("Id","abstract")], by = "Id") %>% 
  .[share_leiden_max >=0.05 & n_years >= 3] %>% 
  unite("words", Titre, abstract, sep = " ")

# adding colors (temporary)
color <- data.table(color = scico(length(unique(alluv_with_abstract$Leiden1)), palette = "roma"),
                    Leiden1 = unique(alluv_with_abstract$Leiden1))
alluv_com <- merge(alluv_with_abstract, color, by = "Leiden1")

tf_idf_com <- tf_idf(nodes = alluv_com, 
         title_column = "words", 
         com_column = "Leiden1",
         com_name_column = "new_Id_com", 
         com_size_column = "share_leiden_total", 
         color_column = "color",
         treshold_com = 0.005, 
         number_of_words = 12,
         size_title_wrap = 8, 
         lemmatize_bigrams = FALSE)

saveRDS(tf_idf_com, paste0(data_path, "EER/2_Raw_Networks_and_Alluv/tf_idf_communities.rds"))

#' # TF-IDF at the window level
#' 
#' Now, documents will be the windows.

# adding colors
color <- data.table(color_window = scico(length(unique(alluv_with_abstract$Window)), palette = "hawaii"),
                    Window = unique(alluv_with_abstract$Window))
alluv_window <- merge(alluv_with_abstract, color, by = "Window") %>% 
  select(Window, Id, words, color_window) %>% 
  unique()
alluv_window <- alluv_window[, size_window := .N/length(alluv_with_abstract$Id), by = "Window"] %>% 
  .[, window_name := paste0(Window,"-",as.integer(Window) + (time_window - 1))]

tf_idf_window <- tf_idf(nodes = alluv_window,  # we remove the color column for communities to avoid doublons
                     title_column = "words", 
                     com_column = "Window",
                     com_name_column = "window_name", 
                     com_size_column = "size_window", 
                     color_column = "color_window",
                     treshold_com = 0.001, 
                     number_of_words = 12,
                     size_title_wrap = 8, 
                     lemmatize_bigrams = FALSE)

saveRDS(tf_idf_window, paste0(data_path, "EER/2_Raw_Networks_and_Alluv/tf_idf_windows.rds"))
