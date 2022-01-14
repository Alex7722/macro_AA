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

################# Loading packages, paths and data ################### ------
#' # Loading packages, paths and data
#' 
#' 
source("EER_Paper/Script_paths_and_basic_objects_EER.R")
source("functions/functions_for_network_analysis.R")
source("functions/functions_for_topic_modelling.R")

# importing corpus
Corpus_EER <- readRDS(paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Corpus.rds"))
alluv_dt <- readRDS(paste0(data_path,"EER/2_Raw_Networks_and_Alluv/alluv_dt.rds"))

# Top 5 data
Corpus_top5 <- readRDS(paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/abstracts_MS_with_ID_Art.RDS"))
Corpus_top5 <- Corpus_top5[ID_Art %in% nodes_JEL$ID_Art]

################# Topic modelling on titles and abstracts ################### ------
###### Choosing the preprocessing steps and the number of topics ######## ----

#' # Topic modelling on titles and abstracts
#' 
#' 
#' ## Choosing the preprocessing steps and the number of topics
#' 
#' We will run the topic model analysis only on the articles with an abstract. As a significant
#' proportion of macro articles in the late 1970s, early 1980s lack of an abstract, we will be forced
#' in a second step to run the same analysis with the articles without abstracts.

to_remove <- c("COPYRIGHT.*",
               " \\* .*",
               "PRESIDENTIAL ADDRESS DELIVERED.*",
               "THIS ARTICLE ORIGINALLY APPEARED IN THE AMERICAN.*",
               "PREPARED FOR THE ANNUAL MEETINGS OF THE AMERICAN ECONOMIC ASSOCIATION",
               "\\(THIS ABSTRACT WAS BORROWED FROM ANOTHER VERSION OF THIS ITEM\\.\\)",
               "JEL CLASSIFICATION:",
               "KEY WORDS:")

text <- Corpus_topic %>% 
  mutate(have_abstract = ! is.na(abstract),
         abstract = toupper(abstract)) %>% 
  unite("word", Titre, abstract, sep = " ") %>% 
  select(ID_Art, word, have_abstract) %>% 
  mutate(word = str_remove(word, " NA$"),
         id = row_number()) %>% 
  mutate(word = str_replace_all(word, "EURO-", "EURO")) %>% 
  mutate(word = str_remove(word, paste0(to_remove, collapse = "|"))) 
 

position_excluded <- c("PUNCT", 
                       "CCONJ", 
                       "SCONJ", 
                       "DET",
                       "ADP",
                       "NUM", 
                       "AUX", 
                       "PART", 
                       "PRON", 
                       "INTJ")

to_keep <- c("flows", 
             "sticky", 
             "flexible", 
             "disconnected",
             "rational",
             "redistribution",
             "budgets",
             "choice",
             "equilibrium",
             "realignment",
             "tradable",
             "developed",
             "optimistic",
             "termed",
             "fischer",
             "systematic",
             "accommodation",
             "smoothness",
             "german",
             "inflows",
             "specific",
             "experiments",
             "merchants",
             "forecasts",
             "offers",
             "instantaneous",
             "in1ationary",
             "cause",
             "wealth",
             "inconsistency",
             "risk",
             "surpluses",
             "reverting",
             "restraint",
             "targeting",
             "committee",
             "prices",
             "fell",
             "insurance",
             "forward",
             "inventory",
             "shocks",
             "needs",
             "yields",
             "constraint",
             "budgets",
             "individual",
             "structure",
             "unemployment",
             "tradable",
             "developed",
             "multiplier")

remove_pattern <- c("\\.\\(this", # Weird pattern that is not removed in the tokenization because of ".("
                    "\"\\-that")
# test if we are not removing words by position we want to
# keep: `spacy_word %>% filter(pos == "CCONJ") %>% select(lemma) %>% unique()`

spacy_initialize()
spacy_word <- spacy_parse(text$word, entity = FALSE) %>% 
  mutate(lemma = str_remove(lemma, paste0(remove_pattern, collapse = "|"))) %>%  
  filter(! (pos %in% position_excluded &
           ! lemma %in% to_keep)) %>% 
  select(doc_id, token, lemma) %>% 
  mutate(doc_id = as.integer(str_remove(doc_id, "text")),
         lemma = str_trim(str_replace_all(lemma, "[:punct:]|[:digit:]", " "), "both")) %>% 
  rename(id = doc_id) %>%
  filter(! lemma == " " & 
           ! lemma == "" &
           str_count(lemma) > 1) %>% 
    as.data.table()
  
to_correct <- data.table("to_correct" = c("experiments",
                                          "forecasts",
                                          "forecasting",
                                          "offers",
                                          "inflow",
                                          "flows",
                                          "accommodation",
                                          "accommodative",
                                          "budgets",
                                          "in1ationary",
                                          "markets",
                                          "prices",
                                          "shocks",
                                          "needs",
                                          "yields"),
                         "correction" = c("experiment",
                                          "forecast",
                                          "forecast",
                                          "offer",
                                          "inflows",
                                          "flow",
                                          "accommodate",
                                          "accommodate",
                                          "budget",
                                          "inflation",
                                          "market",
                                          "price",
                                          "shock",
                                          "need",
                                          "yield"))
for(i in 1:nrow(to_correct)) {
  spacy_word[lemma == to_correct$to_correct[i]]$lemma <- to_correct$correction[i]
}

text <- merge(text[, c("id", "ID_Art", "have_abstract")], 
              spacy_word, 
              by = "id") %>% 
  group_by(ID_Art) %>% 
  mutate(word = paste0(lemma, collapse = " ")) %>% 
  select(-token, -id, -lemma) %>% 
  unique()

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
                                    "ii"),
                           lexicon = "own_built") # We remove typical abstract words and figures
stop_words<- rbind(stop_words, remove_words)
remove_expressions <- c("'s", "\\.")

term_list <- text %>% 
  unnest_tokens(word, word, token = "ngrams", n_min = 1, n = 3, drop = FALSE) %>% 
  separate(word, into = c("word_1", "word_2", "word_3"), sep = " ") %>% 
  mutate(unigram = is.na(word_2) & is.na(word_3),
         bigram = !is.na(word_2) & is.na(word_3)) %>% 
  mutate(ngram = ifelse(unigram == TRUE, "unigram", NA),
         ngram = ifelse(bigram == TRUE, "bigram", ngram),
         ngram = ifelse(is.na(ngram), "trigram", ngram)) %>% 
  mutate(word_2 = ifelse(is.na(word_2), "", word_2),
         word_3 = ifelse(is.na(word_3), "", word_3)) %>% 
  mutate(word_1 = str_remove_all(word_1, paste0(remove_expressions, collapse = "|")),
         word_2 = str_remove_all(word_2, paste0(remove_expressions, collapse = "|")),
         word_3 = str_remove_all(word_3, paste0(remove_expressions, collapse = "|"))) %>% 
  filter(! str_detect(word_1, "[:digit:]") &
           ! str_detect(word_2, "[:digit:]") &
           ! str_detect(word_3, "[:digit:]")) %>% 
  anti_join(stop_words, by = c("word_1" = "word")) %>% 
  anti_join(stop_words, by = c("word_2" = "word")) %>% 
  anti_join(stop_words, by = c("word_3" = "word")) %>% 
  unite(term, word_1, word_2, word_3, sep = " ") %>% 
  mutate(term = str_trim(term, "both")) %>% 
  select(-unigram, -bigram)
term_list <- merge(term_list, text[,c("ID_Art","have_abstract")], by = "ID_Art") %>% 
  as.data.table()

saveRDS(term_list, paste0(data_path, "EER/EER_term_list.rds"))

#' We will now produce different set of data depending on different filtering parameters:
#' 
hyper_grid <- expand.grid(
  upper_share = c(0.35), # remove a word if it is appearing in more than upper_share% of the docs
  lower_share = c(0.01, 0.02), # remove a word if it is appearing in less than lower_share% of the docs
  min_word = c(6, 12), # the min number of words in a doc
  max_word = Inf, # the max number of words in a doc
  prop_word = 1) # keep the top prop_word% of the words (in terms of occurrence)

#' The first step is to use a function to create a list of words data depending on different
#' feature selection criteria (listed in `hyper_grid`).

data_set_trigram <- create_topicmodels_dataset(hyper_grid, 
                                       term_list, 
                                       document_name = "ID_Art",
                                       different_ngram = TRUE)
data_set_trigram[, trigram := TRUE]

data_set_bigram <- create_topicmodels_dataset(hyper_grid, 
                                              filter(term_list, ngram != "trigram"), 
                                              document_name = "ID_Art",
                                              different_ngram = TRUE)
data_set_bigram[, trigram := FALSE]
data_set <- rbind(data_set_trigram, data_set_bigram)

saveRDS(data_set, paste0(data_path, "EER/EER_data_set.rds"))

#' The second step is to use the different data sets to create stm objects and them to fit 
#' topic models for different number of topics.
#' 

# setting up parallel process
nb_cores <- availableCores()/2 + 1
plan(multicore, workers = 2)

data_set <- create_stm(data_set) 
topic_number <- seq(30, 90, 10) 
many_models <- create_many_models(data_set, topic_number, max.em.its = 700, seed = 1989)

#' The third step is to calculate different statistics for each model and produce 
#' different plots summarising these statistics.

tuning_results <- stm_results(many_models)
#' If needed, we can save the result: 
#' `saveRDS(tuning_results, (paste0(data_path, "EER/topic_models.rds")))`.
#' 
#' And reload them at the beginning of a new session: 
#' `tuning_results <- readRDS(paste0(data_path, "EER/topic_models.rds"))`.

tuning_results_test <- tuning_results %>% 
  mutate(frex_data_0.3 = map(topic_model, average_frex, w = weight_1),
         frex_data_0.5 = map(topic_model, average_frex, w = weight_2))

#' We can now project the different statistics to choose the best model(s).
plot_topic_models  <- plot_topicmodels_stat(tuning_results, nb_terms = 100)

plot_topic_models$summary %>%
  ggplotly() %>% 
  htmlwidgets::saveWidget(paste0(picture_path, "tuning_topicmodels_summary.html"))

ragg::agg_png(paste0(picture_path, "tuning_topicmodels_coherence_vs_exclusivity.png"),
        width = 20, height = 15, units = "cm", res = 300)
plot_topic_models$exclusivity_coherence
invisible(dev.off())

plot_topic_models$exclusivity_coherence_mean %>%
  ggplotly() %>% 
  htmlwidgets::saveWidget(paste0(picture_path, "tuning_topicmodels_frex_general.html"))

#' If we want to look at the stat in an interactive framework, we can do:
#' 
#' - `plot_topic_models$summary %>% ggplotly()`;
#' - `plot_topic_models$exclusivity_coherence_mean %>% ggplotly()`

#' For now, we select the preprocessing 12 and 30 topics.
#' 
#' ### Working with the chosen topic model: basic description

id <- 1
nb_topics <- 60 

#' Now we can add the covariates. It seems that it is not changing the topic
#' model too much. The topics are the same, just the order of the words can
#' change. Perhaps that is not changing it at all and the small changes
#' are just linked to the random part of the stm function. 

# extracting the data
stm_data <- tuning_results[preprocessing_id == id & K == nb_topics]$stm[[1]]
metadata <- data.table("ID_Art" = names(stm_data$documents))

# merging with corpus data and selecting the covariates
metadata <- merge(metadata, Corpus_merged, by = "ID_Art", all.x = TRUE)

# temporary step to avoid NA data
metadata <- metadata %>% 
  mutate(EU_US_collab = ifelse(is.na(EU_US_collab), "Neither", EU_US_collab))

stm_data$meta$Year <- as.integer(metadata$Annee_Bibliographique)
stm_data$meta$Origin <- metadata$EU_US_collab
stm_data$meta$Journal <- metadata$Journal_type

#' We can now fit again the topic model for the same number of topics,
#' but adding the covariates for topic prevalence and topic content.

topic_model <- stm(stm_data$documents, 
                   stm_data$vocab, 
                   prevalence = ~s(Year) + Origin + Journal,
                   content = ~Journal,
                   data = stm_data$meta,
                   K = nb_topics,
                   init.type = "Spectral",
                   seed = 1989)

#' We can use the stm package function to plot some descriptive visualisations.
#' For certain visualisations, we can extract the data to use ggplot/ggraph.
#' 
#' We can also extract the top terms for different measure (not just for beta).
#' This data.frame can also be used to give name to the topics. We will use it
#' for the nodes of the topic correlation network.

top_terms <- extract_top_terms(topic_model,
                               tuning_results[preprocessing_id == id & K == nb_topics]$data[[1]],
                               nb_terms = 15,
                               frexweight = 0.3)
#' We will use this table for exploration
#' `saveRDS(top_terms, paste0(data_path, "EER/topic_model_",id,"-",nb_topics,"_top_terms.rds"))`


topics <- name_topics(top_terms, method = "frex", nb_word = 4)

# Setup Colors
color <- data.table::data.table(
  id = 1:nb_topics,
  color = c(scico(n = nb_topics/2 - 1, begin = 0, end = 0.3, palette = "roma"),
            scico(n = nb_topics/2 + 1, begin = 0.6, palette = "roma")))
topics <- merge(topics, color, by = "id")

#' We plot the terms with the highest FREX value for each topic:

top_terms_graph <- top_terms %>%
  filter(measure == "frex") %>% 
  inner_join(topics[, c("id", "color")], by = c("topic" = "id")) %>% 
  mutate(term = reorder_within(term, value, topic)) %>%
  ggplot(aes(value, term)) +
  scale_fill_identity() +
  geom_col(aes(fill = color), show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  coord_cartesian(xlim=c(0.93,1))
  
ragg::agg_png(paste0(picture_path, "topic_model_", id, "-", nb_topics, ".png"),
                width = 50, height = 40, units = "cm", res = 300)
top_terms_graph
invisible(dev.off())
  
#' We now plot the frequency of each topics:
#' 

plot_frequency <- plot_frequency(topics, topic_model) +
  dark_theme_bw()

ragg::agg_png(paste0(picture_path, "topic_model_frequency_", id, "-", nb_topics, ".png"), 
              width = 40, 
              height = 30, 
              units = "cm", 
              res = 300)
plot_frequency
invisible(dev.off())

#' We now plot the topic correlation network:
set.seed(1989)
topic_corr_network <- ggraph_topic_correlation(topic_model, 
                                               nodes = select(topics, -color),
                                               method = "simple", 
                                               size_label = 2.5,
                                               nb_topics = nb_topics) 



# add communities to the topics file:
communities <- topic_corr_network$graph %>% 
  activate(nodes) %>% 
  as.data.table %>% 
  select(topic, Com_ID)
topics <- merge(topics, communities, by = "topic")

#' #### naming communities
#' 
#' We can look at the composition of the different community: `View(topics)`

community_name <- tribble(
  ~Com_ID, ~Com_name,
  "02", "Public Finance, Distribution and Agents Decisions",
  "03", "Inflation & Business Cycles",
  "04", "International Macroeconomics",
  "05", "Fiscal & Monetary policies",
  "06", "Econometrics",
  "07", "Growth & Economic Activity",
  "08", "Investment, Capital & Permanent Income",
  "09", "Search, Expectations & Information",
  "10", "Finance, Financial Intermediation & Demand",
  "11", "Econometrics_bis",
  "12", "Money Demand",
  "13", "Econometrics_ter"
)

#' #### Plotting network with communities

community_name$com_color <- c(mypalette[1:9], "gray", "gray", "gray")
topics <- merge(topics, community_name, by = "Com_ID")
network <- topic_corr_network$graph 
network <- network %>% 
  activate(nodes) %>% 
  left_join(unique(topics[, c("Com_ID", "Com_name", "com_color")]))
network <- network %>% # mix color
  activate(edges) %>%
  mutate(color_com_ID_to = .N()$com_color[to], color_com_ID_from = .N()$com_color[from]) %>%
  mutate(color_edges = DescTools::MixColor(color_com_ID_to, color_com_ID_from, amount1 = 0.5))

graph_plot <- ggraph(network, layout = "manual", x = x, y = y) +
  geom_edge_arc0(aes(color = color_edges, width = weight), strength = 0.3, alpha = 0.6, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.5,12)) +
  scale_edge_colour_identity() +
  scale_fill_identity() +
  geom_node_label(aes(label = topic_name, fill = com_color), size = 2.5, alpha = 0.7) +
  dark_theme_bw()
  
ragg::agg_png(paste0(picture_path, "topic_correlation", id, "-", nb_topics, ".png"), 
              width = 40, 
              height = 30, 
              units = "cm", 
              res = 300)
graph_plot
invisible(dev.off())

#' We will use this table for exploration
#' `saveRDS(topics, paste0(data_path, "EER/topic_model_",id,"-",nb_topics,"_topics_summary.rds"))`
#' 

#' We can look at some topics we find close to understand better their differences:
#' 
#similar_topics <- list(c(4,22),
 #                      c(9,16),
  #                     c(23,7),
   #                    c(17,23),
    #                   c(24, 1),
     #                 c(5, 26))
#ragg::agg_png(paste0(picture_path,"topic_comparison", id, "-", nb_topics, ".png"), 
 #             width = 40, 
  #            height = 30, 
   #           res = 400, 
    #          units = "cm")
#par(mfrow = c(3, 2))
#for(i in 1:length(similar_topics)){
#  plot.STM(topic_model, type = "perspectives", 
 #          topics = similar_topics[[i]], 
  #         n = 30, 
   #        text.cex = 1.5)
#}
#invisible(dev.off())

#' ### Extracting the data from the topic model

topic_gamma <- tidy(topic_model, matrix = "gamma") 
topic_gamma <- merge(topic_gamma, topics[, c("id","topic_name")], 
                     by.x = "topic",
                     by.y = "id") %>% 
  select(-topic) %>% 
  mutate(topic_name = str_remove_all(str_replace(topic_name, "\\\n", " "), " \\/"))

topic_gamma <- pivot_wider(topic_gamma,
                           names_from = topic_name, 
                           values_from = gamma) %>% 
  mutate(ID_Art = names(stm_data$documents)) %>% 
  inner_join(Corpus_merged) %>% 
  select(document, ID_Art, Nom_ISI, Annee_Bibliographique, Titre, Journal, Journal_type, contains("Topic")) %>% 
  as.data.table()

topic_gamma_attributes <- merge(topic_gamma,
                                unique(Collabs[, c("ID_Art", "EU_US_collab")]),
                                by = "ID_Art", all.x = TRUE) %>% 
  mutate(Nom_ISI = str_remove_all(as.character(Nom_ISI), "c\\(\"|\\)|\""))

topic_gamma_attributes <- pivot_longer(topic_gamma_attributes, 
                           cols = contains("Topic"),
                           names_to = "topic_name",
                           values_to = "gamma") %>% 
  as.data.table()

#' We will use this table for exploration
#' `saveRDS(topic_gamma_attributes, paste0(data_path, "EER/topic_model_",id,"-",nb_topics,".rds"))`
#' `topic_gamma_attributes <- readr::write_csv2(topic_gamma_attributes, paste0("summary_topic_model_",id,"-",nb_topics,".csv"))`

#' ### Working with the chosen topic models: differences in terms of journals and affiliations
#' 
#' We will use covariates below to do that, but to have results easier to interpret, we also
#' observe the journal and affiliation effect on topics just by looking at the difference 
#' in the mean of each variable for each topic.
#' 

topic_diff <- copy(topic_gamma_attributes)


topic_diff[, mean_affiliation := mean(gamma), by = c("topic_name", "EU_US_collab")] %>% 
  .[, mean_journal := mean(gamma), by = c("topic_name", "Journal_type")] 
topic_diff_summary <- topic_diff %>% 
  filter(EU_US_collab %in% c("Europe Only", "USA Only")) %>% 
  select(topic_name, EU_US_collab, mean_affiliation, Journal_type, mean_journal) %>% 
  unique() 

topic_diff_summary <- pivot_wider(topic_diff_summary, 
            names_from = "EU_US_collab", 
            values_from = "mean_affiliation") %>% 
  pivot_wider(names_from = "Journal_type", 
              values_from = "mean_journal") %>% 
  mutate(diff_affiliation = `Europe Only` - `USA Only`,
         diff_journal = EER - TOP5,
         id = as.integer(str_extract(topic_name, "[:digit:]{1,2}")),
         topic_label = str_wrap(topic_name, 15),
         topic_label_2 = str_wrap(str_remove(topic_name, "Topic [:digit:]{1,2} "), 15)) %>% 
  left_join(topics[, c("id", "Com_name", "com_color")], by = "id") %>% 
  as.data.table()

#' We will use this table for exploration
#' `saveRDS(unique(topic_diff_summary[, c("topic_name", "diff_affiliation", "diff_journal")]), paste0(data_path, "EER/topic_model_",id,"-",nb_topics,"_topics_diff.rds"))`



#' We can now plot the differences in a two dimension diagram:
#' 

mean_diff_plot <- ggplot(topic_diff_summary, aes(x = diff_affiliation, y = diff_journal)) +
  geom_vline(xintercept = 0, size = 1.5, alpha = 0.9) +
  geom_hline(yintercept = 0, size = 1.5, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray", alpha = 0.5) +
  geom_label_repel(aes(label = topic_label_2, group = factor(Com_name),
                       color = com_color, fill = com_color), size = 8, alpha = 0.7, hjust = 0) +
  geom_point(aes(group = factor(Com_name),
                 color = com_color, fill = com_color), size = 5, alpha = 0.8) +
  scale_color_identity() +
  scale_fill_identity() +
  labs(title = "Topic Prevalence over journals (Difference of Means method)",
       x = "US Only (left) vs. European Only (right)",
       y = "Top 5 (down) vs. EER (up)") +
  dark_theme_classic()

ragg::agg_png(paste0(picture_path, "mean_diff_plot", id, "-", nb_topics, ".png"), 
              width = 50, 
              height = 40, 
              units = "cm", 
              res = 400)
mean_diff_plot
invisible(dev.off())

#' ### Working with the chosen topic model: covariates
#' 
#' We fit regressions for our two covariates. We use a b-spline transformation
#' for the year.
prep <- estimateEffect(~s(Year) + Origin + Journal,
               topic_model,
               metadata = filter(stm_data$meta, ! is.na(Origin)),
               nsims = 100)


#' We first look at the impact of year:
tidyprep_year <- tidystm::extract.estimateEffect(prep, 
                                                 "Year", 
                                                 topic_model, 
                                                 method = "continuous")
tidyprep_year <- merge(tidyprep_year, topics[, c("id", "topic_name", "color")], by.x = "topic", by.y = "id")
slope <- tidyprep_year %>% 
  filter(covariate.value == max(tidyprep_year$covariate.value) |
           covariate.value == min(tidyprep_year$covariate.value)) %>% 
  select(topic_name, covariate.value, estimate) %>% 
  pivot_wider(values_from = estimate, names_from = covariate.value) %>% 
  mutate(slope = `2007` - `1973`) %>% 
  select(topic_name, slope) %>% 
  arrange(slope)
tidyprep_year <- merge(tidyprep_year, slope, by = "topic_name")
tidyprep_year$topic_name <- factor(tidyprep_year$topic_name, levels = slope$topic_name)

#' We plot the impact for each topics:
topic_per_year <- ggplot(tidyprep_year, aes(x = covariate.value, y = estimate,
                   ymin = ci.lower, ymax = ci.upper,
                   group = factor(topic),
                   fill = color)) +
  scale_fill_identity() +
  facet_wrap(~ topic_name, nrow = 5) +
  geom_ribbon(alpha = .5, show.legend = FALSE) +
  geom_line() +
  theme(strip.text = element_text(size = 4)) +
  dark_theme_bw()

ragg::agg_png(paste0(picture_path, "topic_per_year_", id, "-", nb_topics, ".png"), 
              width = 40, 
              height = 30, 
              units = "cm", 
              res = 300)
topic_per_year
invisible(dev.off())

#' We now look at the importance of the geographical institutions of authors. We
#' focus on papers writtent by European-based economists only, or by US-based
#' authors only.

tidyprep_origin <- tidystm::extract.estimateEffect(prep, 
                                                   "Origin", 
                                                   model = topic_model, 
                                                   method = "difference",
                                                   cov.value1 = "Europe Only",
                                                   cov.value2 = "USA Only")

tidyprep_origin <- merge(tidyprep_origin, 
                         topics[, c("id", "topic_name", "color")], 
                         by.x = "topic", 
                         by.y = "id") 
setDT(tidyprep_origin)
tidyprep_origin$topic <- factor(tidyprep_origin$topic, levels = tidyprep_origin[order(estimate)]$topic)

#' We plot the topic prevalence depending of the country of authors' affiliation:
topic_per_origin <- ggplot(tidyprep_origin, aes(x = estimate, y = topic,
                            group = factor(topic_name),
                            color = color)) +
  geom_vline(xintercept = 0, size = 1.5, alpha = 0.5) +
  geom_segment(aes(x = ci.lower, xend = ci.upper, yend = topic, size = 1.5, alpha = 0.9), show.legend = FALSE) +
  geom_point(show.legend = FALSE, size = 4) +
  theme(legend.position = "none") +
  geom_label(aes(x = ci.upper + 0.001, label = topic_name), size = 2, hjust = 0, alpha = 0.6) +
  scale_color_identity() +
  expand_limits(x = c(0, max(tidyprep_origin$ci.upper) + 0.015)) +
  labs(title = "Topic Prevalence over authors' place",
       x = "US only (left) vs. Europe only (right)",
       y = "") +
  dark_theme_bw()

ragg::agg_png(paste0(picture_path, "topic_per_origin", id, "-", nb_topics, ".png"), 
              width = 50, 
              height = 40, 
              units = "cm", 
              res = 300)
topic_per_origin
invisible(dev.off())

#' We now look at the importance of journals for topics.

tidyprep_journal <- tidystm::extract.estimateEffect(prep, 
                                                   "Journal", 
                                                   model = topic_model, 
                                                   method = "difference",
                                                   cov.value1 = "EER",
                                                   cov.value2 = "TOP5")

tidyprep_journal <- merge(tidyprep_journal, 
                         topics[, c("id", "topic_name", "color")], 
                         by.x = "topic", 
                         by.y = "id") 
setDT(tidyprep_journal)
tidyprep_journal$topic <- factor(tidyprep_journal$topic, levels = tidyprep_journal[order(estimate)]$topic)

#' We plot the topic prevalence depending of the journal:
topic_per_journal <- ggplot(tidyprep_journal, aes(x = estimate, y = topic,
                                                group = factor(topic_name),
                                                color = color)) +
  geom_vline(xintercept = 0, size = 1.5, alpha = 0.5) +
  geom_segment(aes(x = ci.lower, xend = ci.upper, yend = topic, size = 1.5, alpha = 0.9), show.legend = FALSE) +
  geom_point(show.legend = FALSE, size = 4) +
  theme(legend.position = "none") +
  geom_label(aes(x = ci.upper + 0.001, label = topic_name), size = 2, hjust = 0) +
  scale_color_identity() +
  expand_limits(x = c(0, max(tidyprep_journal$ci.upper) + 0.015)) +
  labs(title = "Topic Prevalence over journals",
       x = "Top 5 (left) vs. EER (right)",
       y = "") +
  dark_theme_bw()

ragg::agg_png(paste0(picture_path, "topic_per_journal", id, "-", nb_topics, ".png"), 
              width = 40, 
              height = 30, 
              units = "cm", 
              res = 300)
topic_per_journal
invisible(dev.off())

tidyprep_merge <- merge(tidyprep_journal, tidyprep_origin, by = c("topic", "topic_name", "color"))
mix_prevalence_plot <- ggplot(tidyprep_merge, aes(x = estimate.x, y = estimate.y,
                             group = factor(topic_name),
                             color = color)) +
  geom_vline(xintercept = 0, size = 1.5, alpha = 0.5) +
  geom_hline(yintercept = 0, size = 1.5, alpha = 0.5) +
  geom_point(show.legend = FALSE, size = 4) +
  theme(legend.position = "none") +
  geom_label_repel(aes(label = str_wrap(topic_name, 15), fill = color), size = 2.5, alpha = 0.5, hjust = 0) +
  scale_color_identity() +
  scale_fill_identity() +
  labs(title = "Topic Prevalence over journals",
       x = "Top 5 (left) vs. EER (right)",
       y = "US Only (down) vs. European Only (up)") +
  theme_bw()

ragg::agg_png(paste0(picture_path, "mix_prevalence_plot_", id, "-", nb_topics, ".png"), 
              width = 40, 
              height = 30, 
              units = "cm", 
              res = 300)
mix_prevalence_plot
invisible(dev.off())

#' ## Describing topics with communities
#' 
#' 

communities <- readRDS(paste0(data_path,"EER/2_Raw_Networks_and_Alluv/list_networks_EER_top5_Macro.rds"))



#' # TF-IDF at the community level
#' 
#' We merge the alluv data table with the abstract in the corpus. Then we unite titles
#' and abstracts for each article, before to compute the tf-idf with communities as
#' "documents". 
#' 
alluv_dt <- readRDS(paste0(data_path, "EER/2_Raw_Networks_and_Alluv/alluv_dt.rds"))

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

