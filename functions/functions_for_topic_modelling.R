#' ---
#' title: "List of the functions for text preprocessing and topic modelling"
#' author: "Aurélien Goutsmedt"
#' date: "/ Last compiled on `r format(Sys.Date())`"
#' output: 
#'   github_document:
#'     toc: true
#'     number_sections: true
#' ---

#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)

#' # What is this script for?
#' 
#' This script lists all the functions built for network analysis for the project. The functions are documenting in a way to be
#' later implemented in packages or at least for generating a help page (see below).
#' 
#' We first load the [docstring](https://cran.r-project.org/web/packages/docstring/vignettes/docstring_intro.html) package
#' to be able to write documentation in a roxygen2 style. You can then run docstring(name_of_function) to get the standard help page.

if ("docstring" %in% installed.packages() == FALSE) {
  install.packages("docstring", dependencies = TRUE)
}
library(docstring)

#' # Transforming text as data
#' 
#' ## Function for filtering the features selected
#' 
filter_terms <- function(data, upper_share = 1,
                         lower_share = 0, 
                         min_word = 0, 
                         max_word = Inf,
                         prop_word = 1,
                         document_name = "document",
                         term_name = "term") {
  
  data.table::setDT(data)
  data_dt <- copy(data)
  data.table::setnames(data_dt, 
                       c(document_name, term_name),
                       c("document", "term"))
  
  
  data_filtered <- data_dt %>% 
    .[, count := .N, by = term] %>%
    .[, nb_words := .N, by = document] %>% 
    .[, count_per_doc := .N, by = c("term", "document")] %>% 
    unique() %>% 
    .[, nb_apparition := .N/length(unique(data_dt$document)), by = term] %>% 
    .[between(nb_apparition, lower_share, upper_share) & between(nb_words, min_word, max_word)] %>% 
    slice_max(order_by = count, prop = prop_word) 
}

#' # Functions for creating the data table for different pre-processing criteria
#' 
create_topicmodels_dataset <- function(tuning_parameters, data, document_name = "document", term_name = "term") {
  data_list <- list()
  for(i in 1:nrow(hyper_grid)) {
    data_filtered <- filter_terms(data,
                                  upper_share = hyper_grid$upper_share[i],
                                  lower_share = hyper_grid$lower_share[i],
                                  min_word = hyper_grid$min_word[i],
                                  max_word = hyper_grid$max_word[i],
                                  prop_word = hyper_grid$prop_word[i],
                                  document_name = document_name,
                                  term_name = term_name)
    data_filtered <- data_filtered %>% 
      mutate(upper_share = hyper_grid$upper_share[i],
             lower_share = hyper_grid$lower_share[i],
             min_word = hyper_grid$min_word[i],
             max_word = hyper_grid$max_word[i],
             prop_word = hyper_grid$prop_word[i])
    
    data_list[[i]] <- data_filtered
    
  }
  
  data_set <- data_list %>% 
    bind_rows() %>%
    mutate(document = as.character(document)) %>% 
    nest(data = 1:(which(colnames(data_filtered) == "upper_share") - 1))
}

#' # Functions for Topic-Modelling
#' 
#' ## Creating stm object from a set of data with different preprocessing criteria
#' 
create_stm <- function(data, min_word_number = 200){ 
  data_set <- data %>% 
    mutate(dfm = furrr::future_map(data, ~tidytext::cast_dfm(data = ., document, term, count_per_doc))) %>% 
    mutate(stm = furrr::future_map(dfm, ~quanteda::convert(x = ., to = "stm")),
           preprocessing_id = row_number())
  
  list_document <- list()
  list_vocab <- list()
  for(i in 1:nrow(data_set)) {
    doc <- data_set$stm[[i]][[1]]
    vocab <- data_set$stm[[i]][[2]]
    
    list_document[[i]] <- doc
    list_vocab[[i]] <- vocab
    
  }
  
  data_set <- data.table::data.table(data_set, list_document, list_vocab) %>% 
    rename(document = list_document,
           vocab = list_vocab)
  
  # removing data sets with two few vocabularies (if vocab inferior to the number
  # of topics, the algorithm through an error)
  too_few_vocab <- which(map_dbl(data_set$vocab, length) < min_word_number)
  if(length(too_few_vocab) > 0){
    data_set <- data_set[-which(map_dbl(data_set$vocab, length) < min_word_number)]
  }
  return(data_set)
}

#' ## Creating several topic models for different preprocessing criteria and different number of topics
#' 
create_many_models <- function(data, nb_topics, max.em.its) {
  list_models <- list()
  for(i in 1:nrow(data_set)) {
    topic_model <- tibble(K = nb_topics) %>%
      mutate(topic_model = furrr::future_map(K, ~stm(data$stm[[i]][[1]],
                                                     data$stm[[i]][[2]],
                                                     init.type = "Spectral",
                                                     K = .,
                                                     verbose = TRUE,
                                                     max.em.its = max.em.its)))
    list_models[[i]] <- topic_model 
  }
  
  many_models <- data.table(data_set, list_models) %>% 
    unnest(cols = list_models)
  
}

#' ## Producing a list of statistics for each topic model
#' 
stm_results <- function(data){
  
  if(exists("tuning_results")){
    rm(tuning_results, envir = .GlobalEnv)
    message("The object `tuning_results` has been overwritten")
  }
  try(
    tuning_results <- data %>% 
      mutate(exclusivity = map(topic_model, exclusivity),
             semantic_coherence = map(topic_model, semanticCoherence(model = ., documents = document)),
             heldout = map2(document, vocab, make.heldout),
             residual = map2(topic_model, document, checkResiduals),
             bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
             lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
             lbound = bound + lfact) %>% 
      select(-bound, -lfact) %>% 
      mutate(residual = map_dbl(residual, "dispersion"),
             semantic_coherence = map_dbl(semantic_coherence, mean),
             exclusivity = map_dbl(exclusivity, mean)),
    silent = TRUE)
  
  
  if(! exists("tuning_results")){
    tuning_results <- data %>% 
      mutate(exclusivity = map(topic_model, exclusivity),
             heldout = map2(document, vocab, make.heldout),
             bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
             lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
             lbound = bound + lfact) %>% 
      select(-bound, -lfact)
    
    list_coherence <- list()
    list_residual <- list()
    
    for(i in 1:nrow(many_models)) {
      coherence <- semanticCoherence(model = data$topic_model[[i]], documents = data$document[[i]])
      residual <- checkResiduals(stmobj = data$topic_model[[i]], documents = data$document[[i]])
      list_coherence[[i]] <- coherence
      list_residual[[i]] <- residual
    }
    
    tuning_results <- data.table(tuning_results, list_coherence, list_residual)
    tuning_results <- tuning_results %>% 
      mutate(residual = map_dbl(list_residual, "dispersion"),
             semantic_coherence_mean = map_dbl(list_coherence, mean),
             exclusivity_mean = map_dbl(exclusivity, mean)) %>% 
      select(-list_coherence, -list_residual)
  }
  
  
  list_heldout <- list()
  for(i in 1:nrow(tuning_results)) {
    topic_model <- tuning_results$topic_model[[i]]
    missing_heldout <- tuning_results$heldout[[i]]$missing 
    eval_heldout <- eval.heldout(topic_model, missing_heldout)
    list_heldout[[i]] <- eval_heldout$expected.heldout
  }
  
  tuning_results <- cbind(tuning_results, list_heldout) %>% 
    rename(heldout_likelihood = list_heldout) %>% 
    mutate(heldout_likelihood = as.double(heldout_likelihood))
}

#' ## Harmonic mean of coherence and exclusivity
#' 
#' This will be used in the next function to build the harmonic mean between coherence and exclusivity
harmonic_mean_exclusivity_coherence <- function(weight, semantic_coherence_mean, exclusivity_mean) {
  measure <- (weight/semantic_coherence_mean + (1 - weight)/exclusivity_mean)^(-1)
}

#' ## Building multiple plots for summing up the topic models statistics
#' 
plot_topicmodels_stat <- function(data, size = 1, weight_1 = 0.5, weight_2 = 0.3){
  
  results_summary <- data %>%
    transmute(K,
              preprocessing_id,
              upper_share,
              lower_share,
              min_word,
              max_word,
              prop_word,
              residual,
              semantic_coherence_mean,
              heldout_likelihood,
              exclusivity_mean) %>%
    gather(Metric, Value, -c(preprocessing_id,upper_share,lower_share,min_word,max_word,prop_word,K))
  
  
  plot_summary <- ggplot(results_summary, aes(K, Value, color = as.factor(preprocessing_id))) +
    geom_point(size = size) + 
    geom_line(size = size, alpha = 0.8) +
    facet_wrap(~Metric, scales = "free_y") +
    theme_bw() +
    labs(x = "K (number of topics)",
         y = NULL,
         title = "Model diagnostics by number of topics and preprocessing type")
  
  plot_exclusivity_coherence <- data %>% 
    mutate(id = paste0(preprocessing_id, "-", K)) %>% 
    ggplot(aes(semantic_coherence_mean, exclusivity_mean, group = id, color = as.factor(K))) +
    geom_label(aes(label = as.factor(preprocessing_id))) +
    theme_bw() +
    labs(x = "Semantic Coherence",
         y = "Exclusivity",
         title = "Exclusivity versus Coherence by number of topics and preprocessing type")
  
  mix_measure <- tuning_results %>% 
    mutate(mix_measure_1 = harmonic_mean_exclusivity_coherence(weight_1, semantic_coherence_mean, exclusivity_mean),
           mix_measure_2 = harmonic_mean_exclusivity_coherence(weight_2, semantic_coherence_mean, exclusivity_mean)) %>% 
    select(preprocessing_id, K, mix_measure_1, mix_measure_2)
  setnames(mix_measure, c("mix_measure_1","mix_measure_2"), c(paste0("mean_weight_",weight_1), paste0("mean_weight_",weight_2)))
  plot_mix_measure <- mix_measure %>% 
    pivot_longer(cols = starts_with("mean"), names_to = "measure", values_to = "measure_value") %>% 
    ggplot(aes(K, measure_value, color = as.factor(preprocessing_id), group = as.factor(preprocessing_id))) +
    geom_point(size = size, alpha = 0.7) +
    geom_line() +
    facet_wrap(~measure, scales = "free_y") +
    theme_bw() +
    labs(x = "Number of topics",
         y = "Harmonic mean",
         title = "Harmonic mean of Exclusivity versus Coherence for different weights")
  
  plot_list <- list("summary" = plot_summary, 
                    "exclusivity_coherence" = plot_exclusivity_coherence, 
                    "exclusivity_coherence_mean" = plot_mix_measure)
}
