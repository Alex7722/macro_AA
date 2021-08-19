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
source("EER_Paper/Script_paths_and_basic_objects_EER.R")
source("functions/functions_for_network_analysis.R")
source("functions/functions_for_topic_modelling.R")
Corpus <- readRDS(paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Corpus.rds"))
alluv_dt <- readRDS(paste0(data_path,"EER/2_Raw_Networks_and_Alluv/alluv_dt.rds"))


#' # TF-IDF at the community level
#' 
#' We merge the alluv data table with the abstract in the corpus. Then we unite titles
#' and abstracts for each article, before to compute the tf-idf with communities as
#' "documents". 
alluv_with_abstract <- merge(alluv_dt, Corpus[, c("Id","abstract")], by = "Id") %>% 
  .[share_leiden_max >=0.05] %>% 
  unite("words", Titre, abstract, sep = " ")

# removing NA value for abstract (at the end of the column)
alluv_with_abstract <- alluv_with_abstract %>% 
  mutate(words = str_remove(words, " NA$"))

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
         threshold_com = 0.005, 
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
                     threshold_com = 0.001, 
                     number_of_words = 12,
                     size_title_wrap = 8, 
                     lemmatize_bigrams = FALSE)

saveRDS(tf_idf_window, paste0(data_path, "EER/2_Raw_Networks_and_Alluv/tf_idf_windows.rds"))

#' # Topic modelling on titles and abstracts
#' 
#' We will work with two different topic models: one with just the abstract + title of
#' articles that have an abstract, using the standard
#' LDA methods (working better for document with more than 50 words). We will then
#' use the Gibbs sampling method for all the articles but with just the title.
#' 
#' ## Checking abstract distribution
#' 
#' The first thing is to check which articles have no abstract. If the articles without abstracts
#' have a stable distribution over time, it will make our results stronger.
#' 

stat_abstract  <- copy(Corpus)
stat_abstract  <- stat_abstract[JEL_id == 1][, nb_article := .N, by = Annee_Bibliographique][! is.na(abstract)]
stat_abstract  <- stat_abstract[, abstract := round(.N/nb_article,2), by = Annee_Bibliographique][, abstract := as.double(abstract)] %>%
  .[, no_abstract := (1 - abstract)] %>%
  pivot_longer(cols = ends_with("abstract"), names_to = "abstract", values_to = "proportion") %>% 
  select(Annee_Bibliographique, abstract, proportion) %>% 
  unique()

stat_abstract %>% 
  ggplot(aes(Annee_Bibliographique, proportion, fill = abstract)) +
  geom_bar(stat = "identity")

#' ## stm on abstracts only
#' 
#' ### Choosing the preprocessing steps and the number of topics
#' 
#' We will run the topic model analysis only on the articles with an abstract. As a significant
#' proportion of macro articles in the late 1970s, early 1980s lack of an abstract, we will be forced
#' in a second step to run the same analysis with the articles without abstracts.

remove_words <- data.table(word = c("paper",
                                "article",
                                "datum",
                                "contribution",
                                "study",
                                "show",
                                "find",
                                "imply",
                                "analyze",
                                "compare",
                                "literature",
                                "discuss",
                                "focus",
                                "consider",
                                "characterize",
                                "conclusion",
                                "demonstrate",
                                "finally",
                                "significantly",
                                "explore",
                                "ii")) # We remove typical abstract words and figures

remove_expressions <- c("'s", "\\.")

term_list <- Corpus %>% 
  filter(! is.na(abstract) & 
           JEL_id == 1 &
           Annee_Bibliographique <= 2007) %>% 
  unite("word", Titre, abstract, sep = " ") %>% 
  select(ID_Art, word) %>% 
  unnest_tokens(word, word, token = "ngrams", n_min = 1, n = 2) %>% 
  separate(word, into = c("word_1", "word_2"), sep = " ") %>% 
  mutate(word_2 = ifelse(is.na(word_2), "", word_2)) %>% 
  mutate(word_1 = str_remove_all(word_1, paste0(remove_expressions, collapse = "|")),
         word_2 = str_remove_all(word_2, paste0(remove_expressions, collapse = "|"))) %>% 
  filter(! str_detect(word_1, "[:digit:]") &
           ! str_detect(word_2, "[:digit:]")) %>% 
  anti_join(stop_words, by = c("word_1" = "word")) %>% 
  anti_join(stop_words, by = c("word_2" = "word")) %>% 
  mutate(word_1 = textstem::lemmatize_words(word_1), 
         word_2 = textstem::lemmatize_words(word_2)) %>% 
  anti_join(remove_words, by = c("word_1" = "word")) %>% 
  anti_join(remove_words, by = c("word_2" = "word")) %>% 
  unite(term, word_1, word_2, sep = " ") %>% 
  mutate(term = str_trim(term, "both"))

#' We will now produce different set of data depending on different filtering parameters:
#' 
hyper_grid <- expand.grid(
  upper_share = c(0.5, 0.4),
  lower_share = c(0.01, 0.015, 0.02),
  min_word = 0,
  max_word = Inf,
  prop_word = c(0.9, 1)
)

#' The first step is to use a function to create a list of words data depending on different
#' feature selection criteria (listed in `hyper_grid`).

data_set <- create_topicmodels_dataset(hyper_grid, term_list, document_name = "ID_Art")

#' The second step is to use the different data sets to create stm objects and them to fit 
#' topic models for different number of topics.
#' 

# setting up parallel process
nb_cores <- availableCores()/2 + 1
plan(multicore, workers = 2)

data_set <- create_stm(data_set) 
topic_number <- seq(20, 100, 10) 
many_models <- create_many_models(data_set, topic_number, max.em.its = 700, seed = 1989)

#' The third step is to calculate different statistics for each model and produce 
#' different plots summarising these statistics.

tuning_results <- stm_results(many_models)
#' If needed, we can save the result: 
#' `saveRDS(tuning_results, (paste0(data_path, "EER/topic_models.rds")))`.
#' 
#' And reload them at the beginning of a new session: 
#' `tuning_results <- readRDS(paste0(data_path, "EER/topic_models.rds"))`.


#' We can now project the different statistics to choose the best model(s).
plot_topic_models  <- plot_topicmodels_stat(tuning_results)

agg_png(paste0(picture_path, "tuning_topicmodels_summary.png"),
        width = 13, height = 10, units = "cm", res = 300)
plot_topic_models$summary
invisible(dev.off())

agg_png(paste0(picture_path, "tuning_topicmodels_coherence_vs_exclusivity.png"),
        width = 13, height = 10, units = "cm", res = 300)
plot_topic_models$
invisible(dev.off())

agg_png(paste0(picture_path, "tuning_topicmodels_mix_measure.png"),
                            width = 13, height = 10, units = "cm", res = 300)
plot_topic_models$
invisible(dev.off())

#' For now, we select the preprocessing 17 and 30 topics.
#' 
#' ### Working with the chosen topic model

id <- 18
nb_topics <- 30 
chosen_model <- tuning_results[preprocessing_id == id & K == nb_topics]$topic_model[[1]]


#' If we want to save the topics and the most identifying words:
#' `saveRDS(topics, paste0(data_path, "topic_model_EER.rds"))`.

plot_beta <- plot_beta_value(chosen_model, n = 15) +
  scale_fill_viridis_d()
plot_beta +
  ggsave(paste0(picture_path,"topic_model_", id, "-", nb_topics, ".png"), width = 40, height = 30, units = "cm")

#' We can use the stm package function to plot some descriptive visualisations.
#' For certain visualisations, we can extract the data to use ggplot/ggraph.
#' 
#' Here is the prevalence per topic, showing the most recurrent topics:
plot.STM(chosen_model, n = 5, labeltype = "frex", frexw = 0.4)

#' We can also extract the top terms for different measure (not just for beta).
#' This data.frame can also be used to give name to the topics. We will use it
#' for the nodes of the topic correlation network.

top_terms <- extract_top_terms(chosen_model)
set.seed(1989)
topic_corr_network <- ggraph_topic_correlation(chosen_model, top_terms, "huge", size_label = 3) +
  ggsave(paste0(picture_path,"topic_correlation", id, "-", nb_topics, ".png"), width = 40, height = 30, units = "cm")

#' We can look at some topics we find close to understand better their differences:
#' 
similar_topics <- list(c(22,4),
                       c(4,11),
                       c(23,19),
                       c(21, 2),
                       c(15, 2),
                      c(14, 27))
ragg::agg_png(paste0(picture_path,"topic_comparison", id, "-", nb_topics, ".png"), 
              width = 40, 
              height = 30, 
              res = 400, 
              units = "cm")
par(mfrow = c(3, 2))
for(i in 1:length(similar_topics)){
  plot.STM(chosen_model, type = "perspectives", 
           topics = similar_topics[[i]], 
           n = 30, 
           text.cex = 1.5)
}
invisible(dev.off())

#' ## Topic model and covariates
#' 

stm_data <- tuning_results[preprocessing_id == id & K == nb_topics]$stm[[1]]
metadata <- data.table("ID_Art" = names(stm_data$documents))
metadata <- merge(metadata, Corpus[, c("ID_Art", "Annee_Bibliographique")], by = "ID_Art")
stm_data$meta$Year <- as.integer(metadata$Annee_Bibliographique)
topic_model <- stm(stm_data$documents, 
                   stm_data$vocab, 
                   prevalence = ~Year,
                   data = stm_data$meta,
                   K = 30,
                   init.type = "Spectral",
                   seed = 1989)

prep <- estimateEffect(~Year,
               topic_model,
               metadata = stm_data$meta)
summary(prep)
data <- tidy(prep) %>% 
  select(topic, term, estimate) %>% 
  pivot_wider(names_from = term, values_from = estimate) %>% 
  rename(intercept = `(Intercept)`,
         coeff = Year)

ggplot(data, aes(x = unique(stm_data$meta$Year, group = topic))) +
  geom_line(aes(y = intercept + coeff*stm_data$meta$Year))

plot(prep, "Year",
     topics = c(1,2,3),
     method = "continuous")
--------------------------------------------------------------------------------
#' We now extract the gamma values of the topic model to see which article are in which
#' topics, and to observe the links between communities and topics
#' 

topic_gamma <- tidy(topic_model, matrix = "gamma") 
saveRDS(topic_gamma, paste0(data_path, "topic_model_EER_50_gamma.rds"))

topic_and_community <- merge(topic_gamma, 
                             alluv_with_abstract[, c("Id","Leiden1")],
                             by.x = "document",
                             by.y = "Id") %>% 
  as.data.table()
topic_and_community <- topic_and_community[, mean_gamma := mean(gamma), 
                                           by = c("Leiden1","topic")] %>% 
  .[, c("Leiden1","topic","gamma")] %>% 
  unique()

topic_and_community %>%
  mutate(Leiden1 = reorder(Leiden1, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma, color = factor(topic), fill = factor(topic))) +
  geom_boxplot(outlier.size = 0, show.legend = FALSE) +
  facet_wrap(~ Leiden1) +
  theme_minimal() +
  labs(x = "topic", y = expression(gamma)) +
  ggsave(paste0(picture_path,"topic_model_by_com.png"), width = 50, height = 40, units = "cm")

