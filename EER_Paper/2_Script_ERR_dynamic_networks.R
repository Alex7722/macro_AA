#' ---
#' title: "Script for building the EER networks for moving time window"
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
#' This script aims at replicating the [1_building_dynamic_networks.md](/dynamic_networks/1_building_dynamic_networks.md) 
#' but for the EER. It uses the data extracted in [1_Script_EER_corpus.md](/EER_Paper/1_Script_EER_corpus.md) and 
#' it creates the networks of EER publications for different time windows. 
#' We want one-year moving time windows on the whole period (1969-2016) and we need functions automating the creation
#' of the different windows. This script creates the networks, finds communities, integrates the name 
#' of communities, calculates coordinates and saves the nodes and edges data in a long format.
#' 
#' 
#' > WARNING: This script still needs a lot of cleaning
#' 
#' 
#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)

#' # Loading packages, paths and data
#' 


source("EER_Paper/Script_paths_and_basic_objects_EER.R")
source("functions/functions_for_network_analysis.R")

#' # Creating the networks for each time windows
#' 
#' The first step is to build networks for different moving time window. You need to fix 
#' the length of the time window, and different criteria on the creation of edges.

#+ r list

# renaming columns in ref dt for use in the function
setnames(eer_ref, c("ID_Art_Source","ItemID_Ref_Target"), c("ID_Art","ItemID_Ref"))

tbl_coup_list <- dynamic_biblio_coupling(corpus = eer_nodes[between(Annee_Bibliographique, 1974, 2018)], 
                                         direct_citation_dt = eer_ref, 
                                         source = "ID_Art",
                                         source_as_ref = "ItemID_Ref",
                                         ref = "ItemID_Ref", 
                                         time_variable = "Annee_Bibliographique",
                                         coupling_method = "coupling_strength",
                                         time_window_length = 7,
                                         time_window_move = 0,
                                         weight_treshold = 1,
                                         nodes_threshold = 0,
                                         controlling_nodes = FALSE,
                                         controlling_edges = TRUE,
                                         nodes_limit = 10000,
                                         edges_limit = 400000,
                                         distribution_pruning = FALSE,
                                         quantile_threshold = 1,
                                         quantile_move = 0)

#' ## Finding Communities

#' We use the leiden_workflow function of the networkflow package (it uses the 
#' leidenAlg package). We set the number of iteration at 10000 to be sure but 
#' it seems to converge well before.

#+ r communities
tbl_coup_list <- lapply(tbl_coup_list, leiden_workflow, niter = 10000)
