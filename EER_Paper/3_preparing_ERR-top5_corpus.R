#' ---
#' title: "Script for creating the EER + Top5 corpus"
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
#' This script takes the EER corpus (merging WoS and Scopus data) and the Top 5 corpus
#' (merging WoS and Microsoft Academic data), to build the common corpus of the two. We
#' add information about the geographical collaborations and the type of journal. At the
#' end of the script, we have the data ready for text analysis.
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
source(here::here(
  "EER_Paper",
  "Script_paths_and_basic_objects_EER.R"
))

# EER data
Corpus_EER <- readRDS(here(
  eer_data,
  "1_Corpus_Prepped_and_Merged",
  "Corpus.rds"
))

#' We load the merged top 5 articles with Microsoft Academic. But we merge it with
#' the macro corpus of the top 5 articles to keep article which have not been matched
#' with Microsoft Academic.

Corpus_top5_microsoft <- readRDS(here(
  eer_data,
  "1_Corpus_Prepped_and_Merged",
  "abstracts_MS_with_ID_Art.RDS"
)) %>% 
  filter(! is.na(ID_Art)) %>% 
  select(ID_Art, ABSTRACT) %>%
  unique()

top_5 <- c("AMERICAN ECONOMIC REVIEW",
           "ECONOMETRICA",
           "JOURNAL OF POLITICAL ECONOMY",
           "QUARTERLY JOURNAL OF ECONOMICS",
           "REVIEW OF ECONOMIC STUDIES")
Corpus_top5_macro <- nodes_JEL %>% 
  filter(Revue %in% top_5)
  

#' # Creating the corpus for topic-modelling
#'
#' ## Excluding non relevant articles
#'
#' We remove all articles which are comments/reply thanks to their titles. For the moment,
#' we keep only the top 5 articles from MS that we have matched with WoS,
#' as we need the affiliations data from WoS. We keep only the macro articles and we
#' set the lower and upper year depending on our article project and data disponibility. We
#' want to stop before the Great Financial Crisis, and abstracts for top 5 and EER are
#' scarce and weirdly distributed before 1973.
#'
#' > Outside of the presence of abstracts before 1973, there is also the issue that
#' > we don't have JEL code for EER before 1973 and so we cannot classify these papers as
#' > macroeconomics.

Corpus_EER <- Corpus_EER %>%
  filter(!str_detect(Titre, "- COMMENT$|- COMMENTS$|- REPLY$") &
    JEL_id == 1) %>%
  select(ID_Art, Titre, Annee_Bibliographique, Nom_ISI, abstract) %>%
  mutate(Journal = "EUROPEAN ECONOMIC REVIEW")

Corpus_top5 <- Corpus_top5_macro %>% 
  left_join(Corpus_top5_microsoft) %>% 
  select(ID_Art, Titre, Annee_Bibliographique, Nom, ABSTRACT, Revue) %>%
  filter(!str_detect(Titre, ": COMMENT$|: COMMENT \\[|- REPLY$"))

colnames(Corpus_top5) <- colnames(Corpus_EER)
Corpus_topic <- rbind(Corpus_top5, Corpus_EER) %>%
  filter(between(Annee_Bibliographique, 1973, 2002)) %>%
  mutate(across(contains(c("abstract", "Titre", "Nom")), ~ toupper(.)))


# nettoyage des doublons possibles
Corpus_topic <- Corpus_topic %>%
  mutate(test_doublon = paste(Titre, Annee_Bibliographique, Journal)) %>%
  mutate(doublon = duplicated(test_doublon)) %>%
  filter(doublon == FALSE) %>%
  select(-contains("doublon"))

#' We add extra information:
#'
#' - Type of geographical collaborations
#' - Type of journal (EER or Top5)
#'

Collabs <- readRDS(here(
  eer_data,
  "1_Corpus_Prepped_and_Merged",
  "collab_top5_EER.rds"
))
Corpus_topic <- Corpus_topic %>%
  left_join(Collabs) %>%
  mutate(Journal_type = ifelse(Journal == "EUROPEAN ECONOMIC REVIEW", "EER", "TOP5"))

saveRDS(
  Corpus_topic,
  here(
    eer_data,
    "1_Corpus_Prepped_and_Merged",
    "corpus_top5_ERR.rds"
  )
)

#' ## Checking abstract distribution
#'
#' The first thing is to check which articles have no abstract. If the articles without abstracts
#' have a stable distribution over time, it will make our results stronger. For this, we have
#' to clean the corpus from articles that are mere comment/discussion, as they obviously
#' have no abstracts and have a title that is redundant (for instance, in ISoM special
#' issues, you have two discussions for each paper). These discussion/comment papers
#' have a particular format of the type "- COMENT", to be distinguished from papers
#' with abstract that are more generally answering to another paper.
#'

stat_abstract <- copy(Corpus_topic)
stat_abstract <- stat_abstract[, nb_article := .N, by = Annee_Bibliographique][!is.na(abstract)]
stat_abstract <- stat_abstract[, abstract := round(.N / nb_article, 2), by = Annee_Bibliographique][, abstract := as.double(abstract)] %>%
  .[, no_abstract := (1 - abstract)] %>%
  pivot_longer(cols = ends_with("abstract"), names_to = "abstract", values_to = "proportion") %>%
  select(Annee_Bibliographique, abstract, proportion) %>%
  unique()

stat_abstract %>%
  ggplot(aes(Annee_Bibliographique, proportion, fill = abstract)) +
  geom_bar(stat = "identity")
