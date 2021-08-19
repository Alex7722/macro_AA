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
source("~/macro_AA/functions/functions_for_topic_modelling.R")
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
#' We will run the topic model analysis only on the articles with an abstract. As a significant
#' proportion of macro articles in the late 1970s, early 1980s lack of an abstract, we will be forced
#' in a second step to run the same analysis with the articles without abstracts.

remove_words <- tibble(word = c("paper",
                                "article",
                                "datum",
                                "contribution",
                                "study",
                                "show",
                                "find",
                                "imply",
                                "analyze",
                                "compare")) # We remove typical abstract words and figures

term_list <- Corpus %>% 
  filter(! is.na(abstract) & JEL_id == 1) %>% 
  unite("word", Titre, abstract, sep = " ") %>% 
  select(ID_Art, word) %>% 
  unnest_tokens(word, word, token = "ngrams", n_min = 1, n = 2) %>% 
  separate(word, into = c("word_1", "word_2"), sep = " ") %>% 
  mutate(word_2 = ifelse(is.na(word_2), "", word_2)) %>% 
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
  upper_share = c(1, 0.5, 0.4),
  lower_share = c(0, 0.01, 0.02),
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
topic_number <- seq(10, 110, 10) 
many_models <- create_many_models(data_set, topic_number, max.em.its = 700)

#' The third step is to calculate different statistics for each model and produce 
#' different plots summarising these statistics.

tuning_results <- stm_results(many_models)
saveRDS(tuning_results, (paste0(data_path,"EER/topic_models.rds")))

plot_topic_models  <- plot_topicmodels_stat(tuning_results)

agg_png(paste0(picture_path, "tuning_topicmodels_summary.png"),
        width = 13, height = 10, units = "cm", res = 300)
plot_topic_models$summary
invisible(dev.off())

agg_png(paste0(picture_path, "tuning_topicmodels_coherence_vs_exclusivity.png"),
        width = 13, height = 10, units = "cm", res = 300)
plot_topic_models$
invisible(dev.off())agg_png(paste0(picture_path, "tuning_topicmodels_mix_measure.png"),
                            width = 13, height = 10, units = "cm", res = 300)
plot_topic_models$
invisible(dev.off())




#' Once we have our document term matrix, we can first test different number of topics
#' and observe how documents are split in different topics. If for each topic, you have a 
#' small number of documents with a high gamma, it means that you have a good diversity
#' of topics with several articles clearly belonging to these topics.
LDA_test_topic <- function(corpus, k = 10, ...){
  topic_model <- LDA(corpus, k = k, ...)
art_by_topics <- tidy(topic_model, matrix = "gamma") %>% 
  as.data.table()

ggplot(art_by_topics, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma)) +
  ggsave(paste0(picture_path,"test_",k,"_topics.png"), width = 40, height = 30, units = "cm")
}

for(i in seq(50, 60, 5)){
topic_test <- LDA_test_topic(corpus_dtm, k = i) 
}

#' We test with 50 topics
#' 

topic_model <- LDA(corpus_dtm, k = 50, control = list(seed = 1234))

topics <- tidy(topic_model, matrix = "beta") 
saveRDS(topics, paste0(data_path, "topic_model_EER.rds"))

top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  ggsave(paste0(picture_path,"topic_model_EER50_topics.png"), width = 40, height = 30, units = "cm")

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

#' ## Gibbs Sampling on title
#' 
#' We now do the same but only on title, and using a different topic modelling method.
#' 

title_dtm <- Corpus %>% 
  select(ID_Art, Titre) %>% 
  unnest_tokens(word, Titre) %>% 
  anti_join(stop_words) %>% 
  mutate(word = textstem::lemmatize_words(word)) %>% 
  count(ID_Art, word) %>% 
  cast_dtm(ID_Art, word, n)

#' We now test which number of topics could be the best
for(i in seq(55, 70, 5)){
  topic_test <- LDA_test_topic(title_dtm, 
                               k = i, 
                               method = "Gibbs", 
                               control = list(seed = 1234, burnin = 1000, thin = 100, iter = 1000)) 
}

#' Between 35 and 40 topics seems the best in the graphs generated above.
#' But topics appear more relevant when we choose 40.
#' 

topic_model <- LDA(title_dtm,
                   k = 40,
                   method = "Gibbs",
                   control = list(seed = 1234, burnin = 1000, thin = 100, iter = 1000))

topics <- tidy(topic_model, matrix = "beta") 
saveRDS(topics, paste0(data_path, "topic_model_EER_Title_40.rds"))

top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  ggsave(paste0(picture_path,"topic_model_EER_40_title_topics.png"), width = 40, height = 30, units = "cm")

#' We now extract the gamma values of the topic model to see which article are in which
#' topics, and to observe the links between communities and topics
#' 

topic_gamma <- tidy(topic_model, matrix = "gamma") 
saveRDS(topic_gamma, paste0(data_path, "topic_model_EER_40_title_gamma.rds"))

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
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5, show.legend = FALSE) +
  facet_wrap(~ Leiden1) +
  theme_minimal() +
  labs(x = "topic", y = expression(gamma)) +
  ggsave(paste0(picture_path,"topic_model_title_by_com.png"), width = 50, height = 40, units = "cm")
