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
data_path <- "/projects/data/macro_AA/"

source("~/macro_AA/EER_Paper/Script_paths_and_basic_objects_EER.R")
source("~/macro_AA/functions/functions_for_network_analysis.R")
source("/home/alexandre/functions_dynamics_networks.R")
source("/home/alexandre/functions_networks.R")

# Corpus1 <- readRDS(file = paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Corpus.rds"))
# Institutions <- readRDS(file = paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Institutions.rds"))
# Authors <- readRDS(paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Authors.rds"))
# Refs1 <- readRDS(paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Refs.rds"))
# Refs1 <- Refs1[ItemID_Ref_Target!=0]

Corpus2 <- readRDS("/projects/data/macro_AA/Corpus_Econlit_Matched_WoS/JEL_matched_corpus_nodes.rds")
Refs2 <- readRDS(file = "/projects/data/macro_AA/Corpus_Econlit_Matched_WoS/JEL_matched_corpus_references_info.rds")

Refs3 <- readRDS(file = "/projects/data/macro_AA/Corpus_Econlit_Matched_WoS/Old_JEL_matched_corpus_references_info.rds")
Corpus3 <- readRDS("/projects/data/macro_AA/Corpus_Econlit_Matched_WoS/Old_JEL_matched_corpus_nodes.rds")

Refs2 <- Refs2[ItemID_Ref!=0]
Refs3 <- Refs3[ItemID_Ref!=0]
Corpus

Corpus <- rbind(Corpus2, Corpus3, fill=TRUE)
Refs <- rbind(Refs2, Refs3, fill=TRUE)
Corpus <- Corpus[Code_Revue=="9662" | Code_Revue=="13694" | Code_Revue=="4695" | Code_Revue=="13992" | Code_Revue=="758" | Code_Revue=="5200"]
Refs <- Refs[ID_Art %in% Corpus$ID_Art]

Top5_abstract <- fread("EER/Top5/TOP5_AB.csv")
long_ab <- spread(Top5_abstract, Ordre, Abstract)
long_ab <- tidyr::unite(long_ab, Abstract, -Id_Art) 

Top5_art <- fread("EER/Top5/TOP5_ART.csv")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Dynamic Networks and Communities ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
time_window <- 7
first_year <- Corpus[order(Annee_Bibliographique), head(.SD, 1)]$Annee_Bibliographique
last_year <- (as.numeric(Corpus[order(-Annee_Bibliographique), head(.SD, 1)]$Annee_Bibliographique) - time_window + 1) # +1 to get the very last year in the window
last_year <- 1990
all_years <- first_year:last_year
all_years <- c(1975,1980,1985,1990,1995,2000,2005)

Corpus <- Corpus[Annee_Bibliographique>=1975]
Corpus[,ID_Art:=as.character(ID_Art)]
Corpus[,ItemID_Ref:=as.character(ItemID_Ref)]
Refs[,ID_Art:=as.character(ID_Art)]
Refs[,ItemID_Ref:=as.character(ItemID_Ref)]

tbl_coup_list <- dynamics_coupling_networks(corpus = Corpus, 
                                            references = Refs, 
                                            source = "ID_Art", 
                                            target = "ItemID_Ref", 
                                            time_variable = Annee_Bibliographique,
                                            time_window = time_window, 
                                            weight_treshold_value = 2)

# Main components and com
tbl_coup_list <- lapply(tbl_coup_list, main_components)
tbl_coup_list <- lapply(tbl_coup_list, detect_leidenalg, niter = 30000)
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

list_graph_position <- list(list_networks$"1975",list_networks$"1980",list_networks$"1985",list_networks$"1990",list_networks$"1995",list_networks$"2000",list_networks$"2005")

list_graph_position <- list()
for (Year in all_years){
  list_graph_position[[as.character(Year)]] <- list_networks[[paste0(Year)]]
}

list_graph_position <- lapply(list_graph_position, layout_fa2_java)


list_chosen_journals <- list()
for (Year in all_years){
  
  list_chosen_journals[[as.character(Year)]] <- list_networks[[paste0(Year)]] %>% activate(nodes) %>% as.data.table %>% .[,.N,.(Revue)] %>% .$Revue %>% as.data.table()
}

journal_list <- rbindlist(list_chosen_journals)
journal_list <- journal_list %>% rename(Revue = ".")



color = data.table(
  Revue_EER_bin = c("EER","Not EER","Other"),
  color = brewer.pal(n = 3, name = 'Set1'))

list_graph_position <- lapply(list_graph_position, function(tbl){tbl %>% activate(nodes) %>% mutate(Revue_EER_bin = ifelse(Code_Revue==5200,"EER","Not EER"))})
list_graph_position <- lapply(list_graph_position, color_tbl)
list_graph_position2 <- list_graph_position
list_ggplot <- list()

for (Year in all_years) {
  
  
  label_com <- list_graph_position2[[paste0(Year)]] %>% activate(nodes) %>% as.data.table()
  label_com <- label_com[,mean_coord_x:=mean(x), Revue_EER_bin]
  label_com <- label_com[,mean_coord_y:=mean(y), Revue_EER_bin]
  label_com <- label_com[color!="grey"][, head(.SD, 1), Revue_EER_bin]
  # label_com <- merge(label_com, label[,.(new_Id_com, Label_com)], all.x = TRUE)
  network_size <- label_com[,mean((mean_coord_x+mean_coord_y)/2)]
  
  # label_disc <- list_graph_position2[[paste0(Year)]] %>% activate(nodes) %>% as.data.table()
  # label_disc <- label_disc[,mean_coord_x:=mean(x), ESpecialite_custom]
  # label_disc <- label_disc[,mean_coord_y:=mean(y), ESpecialite_custom]
  # label_disc <- label_disc[color!="grey"][, head(.SD, 1), ESpecialite_custom]
  # label_disc <- merge(label_disc, label[,.(new_Id_com, Label_com)], all.x = TRUE)
  # network_size <- label_disc[,mean((mean_coord_x+mean_coord_y)/2)]
  
  list_ggplot[[as.character(Year)]] <- ggraph(list_graph_position2[[paste0(Year)]], "manual", x = x, y = y) +
    geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.5, strength =0.2) +
    geom_node_point(aes(fill = color, size = size), pch=21) +
    scale_edge_width_continuous(range = c(0.5, 1)) +
    theme_void() +
    ggrepel::geom_label_repel(data = label_com, aes(x = mean_coord_x, y = mean_coord_y, label = as.character(Revue_EER_bin), fill = color)) +
    theme(legend.position = "none") +
    scale_fill_identity() +
    scale_edge_colour_identity() +
    labs(title = paste0(as.character(Year),"-",as.character(Year+time_window-1)))
  # ggsave("Networks/coup_2000.png", width=30, height=20, units = "cm")
  
}
g <- ggpubr::ggarrange(plotlist=list_ggplot, common.legend = TRUE, legend="none")


tbl_coup_list <- dynamics_direct_networks(corpus = Corpus, 
                                          references = Refs, 
                                          source = "ID_Art", 
                                          target = "ItemID_Ref", 
                                          time_variable = Annee_Bibliographique,
                                          time_window = time_window, 
                                          weight_treshold_value = 1)

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

list_graph_position <- list(list_networks$"1975",list_networks$"1980",list_networks$"1985",list_networks$"1990",list_networks$"1995",list_networks$"2000",list_networks$"2005")

list_graph_position <- list()
for (Year in all_years){
  list_graph_position[[as.character(Year)]] <- list_networks[[paste0(Year)]]
}

list_graph_position <- lapply(list_graph_position, layout_fa2_java, niter=30000)

list_chosen_journals <- list()
for (Year in all_years){
  
  list_chosen_journals[[as.character(Year)]] <- list_networks[[paste0(Year)]] %>% activate(nodes) %>% as.data.table %>% .[,.N,.(Revue)] %>% .$Revue %>% as.data.table()
}

journal_list <- rbindlist(list_chosen_journals)
journal_list <- journal_list %>% rename(Revue = ".")



color = data.table(
  Revue_EER_bin = c("EER","Not EER","Other"),
  color = brewer.pal(n = 3, name = 'Set1'))

list_graph_position <- lapply(list_graph_position, function(tbl){tbl %>% activate(nodes) %>% mutate(Revue_EER_bin = ifelse(Code_Revue==5200,"EER","Not EER"))})
list_graph_position <- lapply(list_graph_position, color_tbl)
list_graph_position2 <- list_graph_position
list_ggplot2 <- list()

for (Year in all_years) {
  
  
  label_com <- list_graph_position2[[paste0(Year)]] %>% activate(nodes) %>% as.data.table()
  label_com <- label_com[,mean_coord_x:=mean(x), Revue_EER_bin]
  label_com <- label_com[,mean_coord_y:=mean(y), Revue_EER_bin]
  label_com <- label_com[color!="grey"][, head(.SD, 1), Revue_EER_bin]
  # label_com <- merge(label_com, label[,.(new_Id_com, Label_com)], all.x = TRUE)
  network_size <- label_com[,mean((mean_coord_x+mean_coord_y)/2)]
  
  # label_disc <- list_graph_position2[[paste0(Year)]] %>% activate(nodes) %>% as.data.table()
  # label_disc <- label_disc[,mean_coord_x:=mean(x), ESpecialite_custom]
  # label_disc <- label_disc[,mean_coord_y:=mean(y), ESpecialite_custom]
  # label_disc <- label_disc[color!="grey"][, head(.SD, 1), ESpecialite_custom]
  # label_disc <- merge(label_disc, label[,.(new_Id_com, Label_com)], all.x = TRUE)
  # network_size <- label_disc[,mean((mean_coord_x+mean_coord_y)/2)]
  
  list_ggplot2[[as.character(Year)]] <- ggraph(list_graph_position2[[paste0(Year)]], "manual", x = x, y = y) +
    geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.5, strength =0.2) +
    geom_node_point(aes(fill = color, size = size), pch=21) +
    scale_edge_width_continuous(range = c(0.5, 1)) +
    theme_void() +
    ggrepel::geom_label_repel(data = label_com, aes(x = mean_coord_x, y = mean_coord_y, label = as.character(Revue_EER_bin), fill = color)) +
    theme(legend.position = "none") +
    scale_fill_identity() +
    scale_edge_colour_identity() +
    labs(title = paste0(as.character(Year),"-",as.character(Year+time_window-1)))
  # ggsave("Networks/coup_2000.png", width=30, height=20, units = "cm")
  
}

g2 <- ggpubr::ggarrange(plotlist=list_ggplot2, common.legend = TRUE, legend="none")
