#' ---
#' title: "Script for extracting unigrams and bigrams"
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
#' This script aims at creating that data frame with all the unigrams
#' and bigrams for each article. The strategy is to lemmatize them.
#' It will be use for the search engine in the platform.
#' 
#' 
#' > WARNING: This script still needs a lot of cleaning
#' 


#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)

#' # Loading packages, paths and data

#' We first load all the functions created in the functions script. 
#' We also have a separated script with all the packages needed for the scripts
#' in the `dynamic_networks` directory, as well as all the paths (for saving
#' pictures for instance). This separated script also loads our data 
#' and color palettes.

#+ r source
source("~/macro_AA/functions/functions_for_network_analysis.R")
source("~/macro_AA/dynamic_networks/Script_paths_and_basic_objects.R")

#' # Extracting words
#' 
#' We first extract the words and add a column for lemmas

word_JEL <- nodes_JEL %>% 
  unnest_tokens(word, Titre) %>% 
  anti_join(stop_words) %>% 
  select(ID_Art, word) %>% 
  mutate(lemme = textstem::lemmatize_words(word))

#' We then do the same thing but for bigrams, which
#' implies a bit more of work.
#' 

bigram_JEL <- nodes_JEL %>% 
  unnest_tokens(word, Titre, token = "ngrams", n = 2) %>% 
  separate(word, c("word1","word2"), sep = " ") %>% 
  mutate(word1 = str_remove_all(word1, "[:digit:]|'s"),
         word2 = str_remove_all(word2, "[:digit:]|'s")) %>%
  filter(! word1 %in% stop_words$word &
           ! word2 %in% stop_words$word) %>% 
  filter(! is.na(word1) & 
           ! is.na(word2) &
           word1 != "" &
           word2 != "" &
           str_length(word1) > 1 &
           str_length(word2) > 1) %>% 
  mutate(lemme1 = lemmatize_words(word1),
         lemme2 = lemmatize_words(word2)) %>% 
  select(ID_Art, word1, word2, lemme1, lemme2) %>% 
  unite(word, word1, word2, sep = " ") %>% 
  unite(lemme, lemme1, lemme2, sep = " ") %>% 
  as.data.table()

#' We reunify both and save the data
#' 
word_JEL <- rbind(word_JEL, bigram_JEL)

write_csv(word_JEL, paste0(platform_data, "word.csv"))
