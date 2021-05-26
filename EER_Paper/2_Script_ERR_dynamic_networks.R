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
set.seed(500)
#' # Loading packages, paths and data
#' 
library(png)
library(grid)
library(ggnewscale)
library(vite)
library(RMySQL)
library(NetworkToolbox)
library(broom)
library(igraph)
library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)
library(tm)
library(tidyr)
library(tidytext)
library('cluster')
library('ggraph')
library('tibble')
library('tidygraph')
library(ggrepel)
library(readr)
library(leiden)
library(ggraph)
library(ggnewscale)
library(remotes)
library(vite)
library("reticulate")
library(reticulate)
library(leiden)
library(tidygraph)
library(rlang)
library(leiden)
library(ggforce)
library(d3Network)
library(scales)
library(RColorBrewer)
require(DescTools)
require(stringr)
library(docstring)
library(quanteda)
library(pander)
library(DT)
require(forcats)
require(tidyverse)
library(sigmajs)
setwd("/projects/data/macro_AA")


source("~/macro_AA/EER_Paper/Script_paths_and_basic_objects_EER.R")
source("~/macro_AA/functions/functions_for_network_analysis.R")
source("/home/alexandre/functions_dynamics_networks.R")
source("/home/alexandre/functions_networks.R")

Corpus <- readRDS(file = paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Corpus.rds"))
Institutions <- readRDS(file = paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Institutions.rds"))
Authors <- readRDS(paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Authors.rds"))
Refs <- readRDS(paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Refs.rds"))

#' # Creating the networks for each time windows
#' 
#' The first step is to build networks for different moving time window. You need to fix 
#' the length of the time window, and different criteria on the creation of edges.

#+ r list

# renaming columns in ref dt for use in the function
#setnames(eer_ref, c("ID_Art_Source","ItemID_Ref_Target"), c("ID_Art","ItemID_Ref"))

# time_window <- 7
# tbl_coup_list <- dynamic_biblio_coupling(corpus = Corpus,
#                                           direct_citation_dt = Refs,
#                                           source = "ID_Art",
#                                           source_as_ref = "ItemID_Ref",
#                                           ref = "ItemID_Ref",
#                                           time_variable = "Annee_Bibliographique",
#                                           coupling_method = "coupling_strength",
#                                           time_window_length = time_window,
#                                           weight_treshold = 1)
#' ## Finding Communities

#' We use the leiden_workflow function of the networkflow package (it uses the 
#' leidenAlg package). We set the number of iteration at 10000 to be sure but 
#' it seems to converge well before.

#+ r communities
# tbl_coup_list <- lapply(tbl_coup_list, leiden_workflow, niter = 10000)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Dynamic Networks and Communities ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
time_window <- 7
first_year <- Corpus[order(Annee_Bibliographique), head(.SD, 1)]$Annee_Bibliographique
last_year <- (as.numeric(Corpus[order(-Annee_Bibliographique), head(.SD, 1)]$Annee_Bibliographique) - time_window + 1) # +1 to get the very last year in the window
all_years <- first_year:last_year

tbl_coup_list <- dynamics_coupling_networks(corpus = Corpus, 
                                             references = Refs, 
                                             source = "ID_Art", 
                                             target = "ItemID_Ref", 
                                             time_variable = Annee_Bibliographique,
                                             time_window = time_window, 
                                             weight_treshold_value = 2)

#### Citations Total ####
who_cites <- fread("EER/Corpus_EER/who_cites_EER.csv", quote="") %>% data.table
who_cites[,ItemID_Ref:=as.character(ItemID_Ref)]
who_cites[,ID_Art:=as.character(ID_Art)]
tbl_coup_list <- lapply(tbl_coup_list, function(tbl){
  Years <- tbl %>% activate(nodes) %>% as.data.table() %>% .[,Annee_Bibliographique]
  max <- as.numeric(max(Years))
  min <- as.numeric(min(Years))
  citations_this_period <- who_cites[Annee_Bibliographique>=min & Annee_Bibliographique<=max]
  nb_cit_wos <- citations_this_period[,.N,ItemID_Ref]
  colnames(nb_cit_wos)[colnames(nb_cit_wos) == "N"] <- "nb_cit_wos"
  tbl <- tbl %>% activate(nodes) %>% left_join(nb_cit_wos)
  tbl <- tbl %>% activate(nodes) %>% mutate(nb_cit_wos = ifelse(is.na(nb_cit_wos)==TRUE,0,nb_cit_wos))
})

# Main components and com
tbl_coup_list <- lapply(tbl_coup_list, main_components)
tbl_coup_list <- lapply(tbl_coup_list, detect_leidenalg, niter = 10000)
#' We name communities:
tbl_coup_list <- intertemporal_naming_function(tbl_coup_list, treshold_similarity = 0.55)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Alluvial ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
alluv_dt <- make_into_alluv_dt(tbl_coup_list)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Positions ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
list_networks <- lapply(tbl_coup_list, function(tbl){tbl %>% activate(nodes) %>% mutate(Leiden1=new_Id_com)})
list_networks <- lapply(list_networks, function(tbl){tbl %>% activate(nodes) %>% mutate(nb_cit=size)})
#What will be computed as size
list_networks <- lapply(list_networks, function(tbl){tbl %>% activate(nodes) %>% mutate(size=nb_cit_wos)})

list_graph_position <- list()
for (Year in all_years) {
  if(is.null(list_networks[[paste0(Year-1)]])){
    list_graph_position[[paste0(Year)]] <- layout_fa2_java(list_networks[[paste0(Year)]])
  }
  if(!is.null(list_networks[[paste0(Year-1)]])){
    past_position <- list_graph_position[[paste0(Year-1)]] %>% activate(nodes) %>% as.data.table()
    past_position <- past_position[,.(ID_Art,x,y)]
    
    tbl <- list_networks[[paste0(Year)]] %>% activate(nodes) %>% left_join(past_position)
    
    list_graph_position[[paste0(Year)]] <- layout_fa2_java(tbl)
  }
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Saving ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

saveRDS(list_graph_position, file = paste0(data_path,"EER/2_Raw_Networks_and_Alluv/list_networks.rds"))
saveRDS(alluv_dt, file = paste0(data_path,"EER/2_Raw_Networks_and_Alluv/alluv_dt.rds"))
