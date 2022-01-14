Script for building the networks for different sub-periods
================
Aurélien Goutsmedt and Alexandre Truc
/ Last compiled on 2021-03-11

# What is this script for?

This script load the different networks built in the former
[script](/2_Script_Static_Network_Analysis.md) and extract the unigrams
and bigrams in their title, to measure the tf-idf of these terms with
the documents of the Inverse-Document-Frequency being the communities.
It allows us to extract the terms that identify the most each community.

# Loading packages, paths and data

## External scripts

``` r
source("~/macro_AA/functions/functions_for_network_analysis.R")
source("~/macro_AA/Static_Network_Analysis/Script_paths_and_basic_objects.R")
```

# Saving TF-IDF data

``` r
for (i in 1:length(start_date)) {
  graph_coupling <- readRDS(paste0(graph_data_path, "graph_coupling_", start_date[i], "-", end_date[i], ".rds"))
  
  TF_IDF <- tf_idf(graph = graph_coupling)
  
  saveRDS(TF_IDF, paste0(graph_data_path, "tf-idf_coupling_", start_date[i], "-", end_date[i], ".rds"))
  
  # Extracting the nodes of the network
  graph_authors_coupling <- readRDS(paste0(graph_data_path, "graph_authors_coupling_", start_date[i], "-", end_date[i], ".rds"))
  
  Nodes_authors_coupling <- graph_authors_coupling %>%
    activate(nodes) %>%
    select(Id, nb_cit_author, Com_ID, Size_com, color, Community_name) %>%
    as.data.table()
  
  # Merging with the authors table, to have the list of the authors
  Nodes_authors_coupling <- merge(Nodes_authors_coupling, authors_JEL[, c("ID_Art", "Nom", "Titre")], by.x = "Id", by.y = "Nom")
  
  Nodes_authors_coupling <- merge(Nodes_authors_coupling, nodes_JEL[between(Annee_Bibliographique, start_date[i], end_date[i]), c("ID_Art", "Annee_Bibliographique")], by = "ID_Art")
  
  # Calculating the tf-idf for the titles in each community
  TF_IDF_authors <- tf_idf(nodes = Nodes_authors_coupling)
  
  saveRDS(TF_IDF_authors, paste0(graph_data_path, "tf-idf_authors_coupling_", start_date[i], "-", end_date[i], ".rds"))
  
  # Extracting the nodes of the network
  graph_cocit <- readRDS(paste0(graph_data_path, "graph_cocit_", start_date[i], "-", end_date[i], ".rds"))
  
  Nodes_cocit <- graph_cocit %>%
    activate(nodes) %>%
    select(Id, Titre, nb_cit, Com_ID, Size_com, color, Community_name) %>%
    as.data.table()
  
  # Calculating the tf-idf for the titles in each community
  TF_IDF_cocit <- tf_idf(nodes = Nodes_cocit)
  
  saveRDS(TF_IDF_cocit, paste0(graph_data_path, "tf-idf_cocit_", start_date[i], "-", end_date[i], ".rds"))
}
```
