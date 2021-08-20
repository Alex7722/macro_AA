Script for working on titles and abstracts of EER articles
================
Aurélien Goutsmedt and Alexandre Truc
/ Last compiled on 2021-08-20

-   [1 What is this script for?](#what-is-this-script-for)
-   [2 Loading packages, paths and
    data](#loading-packages-paths-and-data)
-   [3 TF-IDF at the community level](#tf-idf-at-the-community-level)
-   [4 TF-IDF at the window level](#tf-idf-at-the-window-level)
-   [5 Topic modelling on titles and
    abstracts](#topic-modelling-on-titles-and-abstracts)
    -   [5.1 Checking abstract
        distribution](#checking-abstract-distribution)
    -   [5.2 stm on abstracts only](#stm-on-abstracts-only)
        -   [5.2.1 Choosing the preprocessing steps and the number of
            topics](#choosing-the-preprocessing-steps-and-the-number-of-topics)
        -   [5.2.2 Working with the chosen topic
            model](#working-with-the-chosen-topic-model)
    -   [5.3 Topic model and covariates](#topic-model-and-covariates)

# 1 What is this script for?

This script takes the `alluv_dt` data built in
[2\_Script\_EER\_dynamic\_networks.md](/EER_Paper/2_Script_EER_dynamic_networks.md)
and merge it with titles and abstracts of corresponding articles. We can
then work on the words associated to each article, and notably calculate
highest tf-idf for communities and windows.

> WARNING: This script still needs a lot of cleaning

# 2 Loading packages, paths and data

``` r
source("EER_Paper/Script_paths_and_basic_objects_EER.R")
source("functions/functions_for_network_analysis.R")
source("functions/functions_for_topic_modelling.R")
Corpus <- readRDS(paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Corpus.rds"))
alluv_dt <- readRDS(paste0(data_path,"EER/2_Raw_Networks_and_Alluv/alluv_dt.rds"))
```

# 3 TF-IDF at the community level

We merge the alluv data table with the abstract in the corpus. Then we
unite titles and abstracts for each article, before to compute the
tf-idf with communities as “documents”.

``` r
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
```

# 4 TF-IDF at the window level

Now, documents will be the windows.

``` r
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
```

# 5 Topic modelling on titles and abstracts

We will work with two different topic models: one with just the abstract
+ title of articles that have an abstract, using the standard LDA
methods (working better for document with more than 50 words). We will
then use the Gibbs sampling method for all the articles but with just
the title.

## 5.1 Checking abstract distribution

The first thing is to check which articles have no abstract. If the
articles without abstracts have a stable distribution over time, it will
make our results stronger.

``` r
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
```

## 5.2 stm on abstracts only

### 5.2.1 Choosing the preprocessing steps and the number of topics

We will run the topic model analysis only on the articles with an
abstract. As a significant proportion of macro articles in the late
1970s, early 1980s lack of an abstract, we will be forced in a second
step to run the same analysis with the articles without abstracts.

``` r
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
```

We will now produce different set of data depending on different
filtering parameters:

``` r
hyper_grid <- expand.grid(
  upper_share = c(0.5, 0.4),
  lower_share = c(0.01, 0.015, 0.02),
  min_word = 0,
  max_word = Inf,
  prop_word = c(0.9, 1)
)
```

The first step is to use a function to create a list of words data
depending on different feature selection criteria (listed in
`hyper_grid`).

``` r
data_set <- create_topicmodels_dataset(hyper_grid, term_list, document_name = "ID_Art")
```

The second step is to use the different data sets to create stm objects
and them to fit topic models for different number of topics.

``` r
# setting up parallel process
nb_cores <- availableCores()/2 + 1
plan(multicore, workers = 2)

data_set <- create_stm(data_set) 
topic_number <- seq(20, 100, 10) 
many_models <- create_many_models(data_set, topic_number, max.em.its = 700, seed = 1989)
```

The third step is to calculate different statistics for each model and
produce different plots summarising these statistics.

``` r
tuning_results <- stm_results(many_models)
```

If needed, we can save the result:
`saveRDS(tuning_results, (paste0(data_path, "EER/topic_models.rds")))`.

And reload them at the beginning of a new session:
`tuning_results <- readRDS(paste0(data_path, "EER/topic_models.rds"))`.
We can now project the different statistics to choose the best model(s).

``` r
plot_topic_models  <- plot_topicmodels_stat(tuning_results)

agg_png(paste0(picture_path, "tuning_topicmodels_summary.png"),
        width = 13, height = 10, units = "cm", res = 300)
plot_topic_models$summary
invisible(dev.off())

agg_png(paste0(picture_path, "tuning_topicmodels_coherence_vs_exclusivity.png"),
        width = 13, height = 10, units = "cm", res = 300)
plot_topic_models$exclusivity_coherence
invisible(dev.off())

agg_png(paste0(picture_path, "tuning_topicmodels_mix_measure.png"),
        width = 13, height = 10, units = "cm", res = 300)
plot_topic_models$exclusivity_coherence_mean %>% ggplotly()
invisible(dev.off())
```

If we want to look at the stat in an interactive framework, we can do:

-   `plot_topic_models$summary %>% ggplotly()`;
-   `plot_topic_models$exclusivity_coherence_mean %>% ggplotly()` For
    now, we select the preprocessing 12 and 30 topics.

### 5.2.2 Working with the chosen topic model

``` r
id <- 12
nb_topics <- 30 
chosen_model <- tuning_results[preprocessing_id == id & K == nb_topics]$topic_model[[1]]
```

If we want to save the topics and the most identifying words:
`saveRDS(topics, paste0(data_path, "topic_model_EER.rds"))`.

``` r
plot_beta <- plot_beta_value(chosen_model, n = 15) +
  scale_fill_viridis_d()
plot_beta +
  ggsave(paste0(picture_path,"topic_model_", id, "-", nb_topics, ".png"), width = 40, height = 30, units = "cm")
```

We can use the stm package function to plot some descriptive
visualisations. For certain visualisations, we can extract the data to
use ggplot/ggraph.

We can also extract the top terms for different measure (not just for
beta). This data.frame can also be used to give name to the topics. We
will use it for the nodes of the topic correlation network.

``` r
top_terms <- extract_top_terms(chosen_model)
topics <- name_topics(top_terms, "frex", nb_word = 4)

# Setup Colors
color <- data.table::data.table(
  id = 1:nb_topics,
  color = c(scico(n = nb_topics/2 - 1, begin = 0, end = 0.3, palette = "roma"),
            scico(n = nb_topics/2 + 1, begin = 0.6, palette = "roma")))
topics <- merge(topics, color, by = "id")
```

We now plot the frequency of each topics:

``` r
plot_frequency(topics) +
  ggsave(paste0(picture_path,"topic_model_frequency_", id, "-", nb_topics, ".png"), width = 40, height = 30, units = "cm")
```

We now plot the topic correlation network:

``` r
set.seed(1989)
topic_corr_network <- ggraph_topic_correlation(chosen_model, 
                                               nodes = topics,
                                               method = "huge", 
                                               size_label = 3) +
  ggsave(paste0(picture_path,"topic_correlation", id, "-", nb_topics, ".png"), width = 40, height = 30, units = "cm")
```

We can look at some topics we find close to understand better their
differences:

``` r
similar_topics <- list(c(16,24),
                       c(9,16),
                       c(23,7),
                       c(17,23),
                       c(24, 1),
                      c(5, 26))
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
```

## 5.3 Topic model and covariates

Now we can add the covariates. It seems that it is not changing the
topic model too much. The topics are the same, just the order of the
words can change. Perhaps that is not changing it at all and the small
changes are just linked to the random part of the stm function.

``` r
# extracting the data
stm_data <- tuning_results[preprocessing_id == id & K == nb_topics]$stm[[1]]
metadata <- data.table("ID_Art" = names(stm_data$documents))

# merging with corpus data and selecting the covariates
Corpus_merged <- merge(Corpus[, c("ID_Art", "Annee_Bibliographique")], 
                       unique(Institutions[, c("ID_Art", "EU_US_collab")]),
                       by = "ID_Art")
metadata <- merge(metadata, Corpus_merged, by = "ID_Art")
stm_data$meta$Year <- as.integer(metadata$Annee_Bibliographique)
stm_data$meta$Origin <- metadata$EU_US_collab
```

We can now fit again the topic model for the same number of topics, but
adding the covariates for topic prevalence and topic content.

``` r
topic_model <- stm(stm_data$documents, 
                   stm_data$vocab, 
                   prevalence = ~s(Year) + Origin,
                   content = ~Origin,
                   data = stm_data$meta,
                   K = nb_topic,
                   init.type = "Spectral",
                   seed = 1989)

top_terms <- extract_top_terms(topic_model)
topics_covariate <- name_topics(top_terms, "frex", nb_word = 4)
topics_covariate <- merge(topics_covariate, topics[, c("id", "color")], by = "id")
```

We fit regressions for our two covariates. We use a b-spline
transformation for the year.

``` r
prep <- estimateEffect(~s(Year) + Origin,
               topic_model,
               metadata = stm_data$meta,
               nsims = 50)
```

We first look at the impact of year:

``` r
tidyprep_year <- tidystm::extract.estimateEffect(prep, "Year", topic_model, method = "continuous")
tidyprep_year <- merge(tidyprep_year, topics_covariate[, c("id", "topic_name", "color")], by.x = "topic", by.y = "id")
slope <- tidyprep_year %>% 
  filter(covariate.value == max(tidyprep_year$covariate.value) |
           covariate.value == min(tidyprep_year$covariate.value)) %>% 
  select(topic_name, covariate.value, estimate) %>% 
  pivot_wider(values_from = estimate, names_from = covariate.value) %>% 
  mutate(slope = `2007` - `1974`) %>% 
  select(topic_name, slope) %>% 
  arrange(slope)
tidyprep_year <- merge(tidyprep_year, slope, by = "topic_name")
tidyprep_year$topic_name <- factor(tidyprep_year$topic_name, levels = slope$topic_name)
```

We plot the impact for each topics:

``` r
ggplot(tidyprep_year, aes(x = covariate.value, y = estimate,
                   ymin = ci.lower, ymax = ci.upper,
                   group = factor(topic),
                   fill = color)) +
  scale_fill_identity() +
  facet_wrap(~ topic_name, nrow = 5) +
  geom_ribbon(alpha = .5, show.legend = FALSE) +
  geom_line()
```

We now look at the importance of the geographical institutions of
authors. We focus on papers writtent by European-based economists only,
or by US-based authors only.

``` r
tidyprep_origin <- tidystm::extract.estimateEffect(prep, 
                                                   "Origin", 
                                                   model = topic_model, 
                                                   method = "difference",
                                                   cov.value1 = "Europe Only",
                                                   cov.value2 = "USA Only")

tidyprep_origin <- merge(tidyprep_origin, topics_covariate[, c("id", "topic_name", "color")], by.x = "topic", by.y = "id") 
setDT(tidyprep_origin)
tidyprep_origin$topic <- factor(tidyprep_origin$topic, levels = tidyprep_origin[order(estimate)]$topic)
```

We plot the topic prevalence depending of the country of authors’
affiliation:

``` r
ggplot(tidyprep_origin, aes(x = estimate, y = topic,
                            group = factor(topic_name),
                            color = color)) +
  geom_segment(aes(x = ci.lower, xend = ci.upper, yend = topic, size = 1.5, alpha = 0.9), show.legend = FALSE) +
  geom_point(show.legend = FALSE, size = 4) +
  theme(legend.position = "none") +
  geom_label(aes(x = ci.upper + 0.001, label = topic_name), size = 3, hjust = 0) +
  scale_color_identity() +
  expand_limits(x = c(0, max(tidyprep_origin$ci.upper) + 0.015)) +
  labs(title = "Topic Prevalence over authors' place",
       x = "US only (left) vs. Europe only (right)",
       y = "") +
  theme_bw()
```

We now extract the gamma values of the topic model to see which article
are in which topics, and to observe the links between communities and
topics

``` r
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
```
