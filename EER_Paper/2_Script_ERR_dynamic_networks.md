Script for building the EER networks for moving time window
================
Aurélien Goutsmedt and Alexandre Truc
/ Last compiled on 2021-04-02

  - [1 What is this script for?](#what-is-this-script-for)
  - [2 Loading packages, paths and
    data](#loading-packages-paths-and-data)
  - [3 Creating the networks for each time
    windows](#creating-the-networks-for-each-time-windows)
      - [3.1 Finding Communities](#finding-communities)

# 1 What is this script for?

This script aims at replicating the
[1\_building\_dynamic\_networks.md](/dynamic_networks/1_building_dynamic_networks.md)
but for the EER. It uses the data extracted in
[1\_Script\_EER\_corpus.md](/EER_Paper/1_Script_EER_corpus.md) and it
creates the networks of EER publications for different time windows. We
want one-year moving time windows on the whole period (1969-2016) and we
need functions automating the creation of the different windows. This
script creates the networks, finds communities, integrates the name of
communities, calculates coordinates and saves the nodes and edges data
in a long format.

> WARNING: This script still needs a lot of cleaning

# 2 Loading packages, paths and data

``` r
source("~/macro_AA/EER_Paper/Script_paths_and_basic_objects_EER.R")
source("~/macro_AA/functions/functions_for_network_analysis.R")
source("/home/alexandre/functions_dynamics_networks.R")
source("/home/alexandre/functions_networks.R")

Corpus <- readRDS(file = paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Corpus.rds"))
Institutions <- readRDS(file = paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Institutions.rds"))
Authors <- readRDS(paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Authors.rds"))
Refs <- readRDS(paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Refs.rds"))
```

# 3 Creating the networks for each time windows

The first step is to build networks for different moving time window.
You need to fix the length of the time window, and different criteria on
the creation of edges.

``` r
# renaming columns in ref dt for use in the function
#setnames(eer_ref, c("ID_Art_Source","ItemID_Ref_Target"), c("ID_Art","ItemID_Ref"))

time_window <- 7
tbl_coup_list <- dynamic_biblio_coupling(corpus = Corpus,
                                          direct_citation_dt = Refs,
                                          source = "ID_Art",
                                          source_as_ref = "ItemID_Ref",
                                          ref = "ItemID_Ref",
                                          time_variable = "Annee_Bibliographique",
                                          coupling_method = "coupling_strength",
                                          time_window_length = time_window,
                                          weight_treshold = 1)
```

## 3.1 Finding Communities

We use the leiden\_workflow function of the networkflow package (it uses
the leidenAlg package). We set the number of iteration at 10000 to be
sure but it seems to converge well before.

``` r
tbl_coup_list <- lapply(tbl_coup_list, leiden_workflow, niter = 10000)
```

We name communities:

``` r
tbl_coup_list <- intertemporal_naming_function(tbl_coup_list)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Alluvial ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
alluv_dt <- make_into_alluv_dt(tbl_coup_list)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Positions ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
list_networks <- lapply(tbl_coup_list, function(tbl){tbl %>% activate(nodes) %>% mutate(Leiden1=new_Id_com)})
list_networks <- lapply(list_networks, function(tbl){tbl %>% activate(nodes) %>% mutate(size=nb_cit)})

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

list_graph_position <- make_into_alluv_dt(list_graph_position)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Saving ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

saveRDS(list_graph_position, file = paste0(data_path,"EER/2_Raw_Networks_and_Alluv/list_networks.rds"))
saveRDS(alluv_dt, file = paste0(data_path,"EER/2_Raw_Networks_and_Alluv/alluv_dt.rds"))
```
