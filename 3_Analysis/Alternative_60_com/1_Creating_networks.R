#' ---
#' title: "Script for building the networks for moving time window"
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
#' This script aims at creating the networks for different time windows. We want one-year moving time 
#' windows on the whole period (1969-2016) and we need functions automating the creation
#' of the 44 windows. This script creates the networks, finds communities, integrates the name 
#' of communities, calculates coordinates and saves the nodes and edges data in a long format,
#' used for producing the platform.
#' 
#' 
#' > WARNING: This script still needs a lot of cleaning
#' 


#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)

#' # Loading packages, paths and data

#' We first load all the functions created in the functions script. 
#' We also have a separated script with all the packages needed for the scripts
#' in the `dynamic_networks` directory, as well as all the paths (for saving
#' pictures for instance). This separated script also loads our data 
#' and color palettes.

#+ r source
source("functions/functions_dynamics_networks_alex.R")
# source("functions/functions_networks_alex.R")
source("functions/functions_for_network_analysis.R")
source("functions/Script_paths_and_basic_objectsV2.R")

set.seed(3155210)

intertemporal_naming_function <- function(tbl_list = tbl_list, 
                                          community_column = Leiden1,
                                          individual_ids = Id,
                                          threshold_similarity = 0.55){
  #' This function 
  #' 
  #' @tbl_list
  #' A list of tbl with the name of the elements as the year of the oldest publications
  
  ######################### Prepare everything **********************
  
  set.seed(500) # set seed otherwise the name of communities change everytime the function is run
  all_years <- as.numeric(names(list_graph)) # get all years to study
  unique_ids <- stri_rand_strings(10000, 8) # unique Ids
  
  intertemporal_naming <- list()
  for (Year in all_years) {
    
    ######################### For the first year, let's just give them unique ids **********************
    
    if(is.null(list_graph[[paste0(Year-1)]])){
      
      dt_year <- setnames(list_graph[[paste(Year)]] %>% activate(nodes) %>% as.data.table(),
                          c(community_column,individual_ids), 
                          c("Leiden1","ID_Art"))
      dt_year <- dt_year[,.(ID_Art,Leiden1)]
      
      #give a unique ids to com
      dt_id_com <- data.table(Id_com = unique_ids[1:dt_year[,.N,Leiden1][,.N]],
                              Leiden1 = dt_year[,.N,Leiden1]$Leiden1)
      
      #remove ids taken from list
      unique_ids <- unique_ids[-c(1:dt_year[,.N,Leiden1][,.N])]
      #merge with unique id
      dt_year <- merge(dt_year, dt_id_com, by= "Leiden1")
      dt_year <- dt_year[,new_Id_com:=Id_com]
      dt_year <- dt_year[,.(ID_Art,new_Id_com)]
      
      tbl <- list_graph[[paste0(Year)]] %>% activate(nodes) %>% left_join(dt_year, by = "ID_Art")
      intertemporal_naming[[paste0(Year)]] <- tbl
      
    }
    
    ######################### For other years, we need to take the previous years and give new names to community of the new year **********************
    
    else if(!is.null(list_graph[[paste0(Year-1)]])){
      
      ######################### Communities from previous year **********************
      
      dt_year <- intertemporal_naming[[paste0(Year-1)]] %>% activate(nodes) %>% as.data.table()
      dt_year <- dt_year[,.(ID_Art,new_Id_com)]
      #give a unique ids to com
      dt_year <- dt_year[,Id_com:=new_Id_com]
      
      
      ######################### Communities from this year **********************
      dt_year2 <- setnames(list_graph[[paste(Year)]] %>% activate(nodes) %>% as.data.table(),
                           c(community_column,individual_ids), 
                           c("Leiden1","ID_Art"))
      dt_year2 <- dt_year2[,.(ID_Art,Leiden1)]
      #give a unique ids to com
      dt_id_com <- data.table(Id_com = unique_ids[1:dt_year2[,.N,Leiden1][,.N]], Leiden1 = dt_year2[,.N,Leiden1]$Leiden1)
      #remove ids taken from list
      unique_ids <- unique_ids[-c(1:dt_year2[,.N,Leiden1][,.N])]
      #merge with unique id
      dt_year2 <- merge(dt_year2, dt_id_com, by= "Leiden1")
      
      
      # Let's compute the shares of movements relatively to all articles, y articles, and x articles.(add  <,all.y = TRUE, all.x= TRUE)> to merge for taking NAs into account)
      alluv <- merge(dt_year, dt_year2, by= "ID_Art")
      
      #  Calculating the share above total move of community for each combination of this community with other communities
      alluv <- alluv[,n_com_move_full:=.N, by = c("Id_com.x", "Id_com.y")] %>% 
        .[,share_full:=n_com_move_full/nrow(alluv)]
      
      alluv[,n_com_y:=.N, by = "Id_com.y"][,share_full_y:=n_com_move_full/n_com_y] 
      alluv[,n_com_x:=.N, by ="Id_com.x"][,share_full_x:=n_com_move_full/n_com_x]
      
      alluv <- alluv[,.N,.(Id_com.x, Id_com.y, share_full, share_full_x, share_full_y)][order(share_full, share_full_y, share_full_x)]
      
      # Make into a list by community X, so that we can find what happen to each community X
      
      alluv_list <- split(alluv, c(alluv$Id_com.x))
      
      # For each community X we look at what happen to their nodes and we extract a list to merge to give new names to com.y
      
      for (com.x in names(alluv_list)) {
        
        dt <- alluv_list[[paste0(com.x)]][order(-share_full)] # On ordonne par l'importance du mouvement entre deux communautés
        
        # On regarde le lien le plus fort de chaque communauté x, 
        # si z% des noeuds vont vers une communauté unique, 
        # + cette communauté est composée à z% par les articles de Y, alors c'est la même communauté
        
        if(dt$share_full_x[[1]] > threshold_similarity & dt$share_full_y[[1]] > threshold_similarity){
          new_name <- dt[order(-share_full)]$Id_com.y[[1]]
          alluv_list[[paste0(com.x)]] <- data.table(new_Id_com.y = paste0(com.x), Id_com.y = new_name)
        }
        
        # Sinon on garde l'identifiatn unique (la commaunuté est nouvelle, un merge ou le résutlat d'un split)
        
        else{
          new_name <- dt[order(-share_full)]$Id_com.y[[1]]
          alluv_list[[paste0(com.x)]] <- data.table(new_Id_com.y = paste0(com.x), Id_com.y = paste0(com.x))
        }
      }
      
      # On bind notre list qui nous donne les nouveaux noms des communautés Y
      alluv_all <- rbindlist(alluv_list)
      # Et on merge
      new_dt_year2 <- merge(dt_year2, alluv_all, by.x = "Id_com", by.y = "Id_com.y", all.x = TRUE)
      # Si la communauté n'a pas de nouveau nom, elle garde l'ancien nom
      new_dt_year2 <- new_dt_year2[is.na(new_Id_com.y)==TRUE, new_Id_com.y:=Id_com]
      # Renommer la colonne
      new_dt_year2 <- new_dt_year2 %>% rename(new_Id_com = new_Id_com.y)
      
      # Garde-fou: Break the loop if the number of community change after procedure
      if(new_dt_year2[,.N,new_Id_com][,.N]!=dt_year2[,.N,Leiden1][,.N]) {
        print("We lost communities in ", paste0(as.character(Year-1),"-",as.character(Year)," transformation. Check for",as.character(Year)," data.table." ))
        break;
      }
      
      #  V(list_graph[[paste0(Year)]])$Id <- as.character(V(list_graph[[paste0(Year)]])$Id)
      #new_dt_year2$Id <- as.character(new_dt_year2$Id)
      # On injecte les nouveaux noms dans le tbl, et on refait la liste des tbl avec les nouveaux noms
      tbl <- list_graph[[paste0(Year)]] %>% activate(nodes) %>% left_join(new_dt_year2, by = "ID_Art")
      intertemporal_naming[[paste0(Year)]] <- tbl
      
    }
  }
  return (intertemporal_naming)
}

make_into_alluv_dt <- function(intertemporal_networks, 
                               community_column = new_Id_com){
  
  #' This function 
  #' 
  #' @tbl_list
  #' A list of tbl with the name of the elements as the year of the oldest publications
  
  new_Id_com <- deparse(substitute(community_column))
  
  networks <- lapply(intertemporal_networks, function(tbl)(tbl %>% activate(nodes) %>% as.data.table))
  
  networks <- lapply(networks, function(dt)(dt[,.(ID_Art,new_Id_com,Titre)]))
  
  alluv_dt<- rbindlist(networks, idcol = "Window")
  alluv_dt[,n_id_window:=.N, Window]
  alluv_dt[,share:=1/n_id_window]
  
  alluv_dt[,Leiden1:=new_Id_com]
  alluv_dt[,share_leiden_total:=.N/alluv_dt[,.N],Leiden1]
  
  alluv_dt[,tot_window_leiden:=.N,.(Window,Leiden1)]
  alluv_dt[,tot_window:=.N,.(Window)]
  alluv_dt[,share_leiden:=tot_window_leiden/tot_window]
  alluv_dt[,share_leiden:=max(share_leiden),Leiden1]
  # alluv_dt<-merge(alluv_dt, unique_ids_color[,.(Leiden1,color)], by="Leiden1",all.x = TRUE)
  
  n_years <- alluv_dt[, head(.SD, 1), .(Window,Leiden1)][,.N,Leiden1][order(N)]
  n_years <- n_years %>% rename(n_years = N)
  
  alluv_dt <- merge(alluv_dt, n_years, by="Leiden1",all.x = TRUE)
  return (alluv_dt)
}

#' # Building the networks
#' 
#' ## Creation of the networks

#' We prepare our list. We want a weight threshold of 2 until 1986 (included). As the network are changing
#' a lot with 1991, because of the changes in the JEL classification, we prefer to change the weight
#' threshold manually at this point. Thus we move to 3 as soon as 1987-1991. 
#' 
#' For the moment, we have decided to keep 3 until the end of the period, at least for computing the 
#' Leiden.

#+ r list


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Tbl list ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

tbl_coup_list <- dynamic_biblio_coupling(corpus = nodes_JEL[between(Annee_Bibliographique, 1969, 2015)], 
                                         direct_citation_dt = edges_JEL[new_id!=0], 
                                         source = "ID_Art",
                                         source_as_ref = "ItemID_Ref",
                                         ref = "new_id", 
                                         time_variable = "Annee_Bibliographique",
                                         coupling_method = "coupling_strength",
                                         time_window_length = 5,
                                         time_window_move = 0,
                                         weight_threshold = 2,
                                         nodes_threshold = 0,
                                         controlling_nodes = FALSE,
                                         controlling_edges = FALSE,
                                         nodes_limit = 10000,
                                         edges_limit = 400000,
                                         distribution_pruning = FALSE,
                                         quantile_threshold = 1,
                                         quantile_move = 0)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Community Detection ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' ## Finding Communities

#' We use the leiden_workflow function of the networkflow package (it uses the 
#' leidenAlg package). 

#+ r communities
tbl_coup_list <- lapply(tbl_coup_list, leiden_workflow)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Intertemporal Naming ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' ## Intertemporal Naming + size
tbl_coup_list <- lapply(tbl_coup_list, 
                        function(tbl){tbl %>% 
                            activate(nodes) %>% 
                            mutate(size = nb_cit,
                                   ID_Art = as.character(ID_Art))})

list_graph <- tbl_coup_list
intertemporal_naming <- intertemporal_naming_function(list_graph, 
                                                      community_column = "Com_ID", 
                                                      individual_ids = "ID_Art", 
                                                      threshold_similarity = 0.55)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Into Alluvial ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

alluv_dt <- make_into_alluv_dt(intertemporal_naming)

alluv_dt$new_Id_com   <- as.factor(alluv_dt$new_Id_com)
alluv_dt$new_Id_com <- fct_reorder(alluv_dt$new_Id_com, alluv_dt$share_leiden_total,min, .desc = FALSE)
#alluv_dt$new_Id_com <- fct_reorder(alluv_dt$new_Id_com, alluv_dt$n_years,min, .desc = FALSE)

######################### Colors **********************
color <- brewer.pal(8, name = "Dark2")
color2 <- brewer.pal(12, name = "Paired")
color3 <- brewer.pal(12, name = "Set3")
scico <- scico(20, palette = "tokyo")
color_final <- append(color, color2)
color_final <- append(color_final, color3)
color_final <- append(color_final, scico)
mypalette <- append(color_final, mypalette)
unique_ids_color <- data.table(
  Leiden1 = as.character(alluv_dt[n_years>0 & share_leiden>=0.03,.N,new_Id_com]$new_Id_com), 
  color = mypalette[c(1:alluv_dt[n_years>0 & share_leiden>=0.03,.N,new_Id_com][,.N])])
alluv_dt<-merge(alluv_dt, unique_ids_color[,.(Leiden1,color)], by="Leiden1",all.x = TRUE)
alluv_dt[is.na(color)==TRUE,color:="grey"]

#' ## Projecting the alluvials and saving

######################### Label **********************
label <- copy(alluv_dt)
label <- label[,Window:=round(mean(as.numeric(Window))),new_Id_com][color!="grey", head(.SD, 1), .(new_Id_com)]
label[,Label:=new_Id_com]

alluv_dt<-merge(alluv_dt,label[,.(new_Id_com,Window,Label)], by = c("new_Id_com","Window"), all.x = TRUE) 

######################### Minimize Crossing **********************
alluv_dt[,Id:=ID_Art]
alluv_dt<- minimize_crossing(alluv_dt)
alluv_dt$new_Id_com <- fct_reorder(alluv_dt$new_Id_com, alluv_dt$order,min, .desc = TRUE)
alluv_dt[,order:=NULL]

plot_alluvial <- ggplot(alluv_dt, aes(x = Window, y=share, stratum = new_Id_com, alluvium = ID_Art, fill = color, label = new_Id_com)) +
  scale_fill_identity("Disciplines", guide = "legend") +
  geom_flow() +
  geom_stratum(alpha =1, size=1/10) +
  theme(legend.position = "none") +
  geom_label(stat = "stratum", size = 5, aes(label = Label)) +
  ggtitle("")

# ggsave(here(picture_path,"Benchmark", "alluvial_55_raw.png"), plot = plot_alluvial, width = 40, height = 30, units = "cm")

alluv_dt <- alluv_dt[, Com_ID := new_Id_com]
alluv_dt <- alluv_dt[, share_max := max(share_leiden), by = "Com_ID"]

# adding a more simple label for naming communities later
ID_bis <- unique(alluv_dt[color != "grey"][order(Window,-n_years), "Com_ID"])
ID_bis <- ID_bis[, ID_bis:= 1:.N]
alluv_dt <- merge(alluv_dt, ID_bis, by = "Com_ID", all.x = TRUE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### tf-idf ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' ## Search tf-idf values for each community

################## search for tf-idf ############################

tf_idf_results <- tf_idf(nodes = alluv_dt[color != "grey"],
                         com_name_column = "new_Id_com",
                         number_of_words = 20, 
                         threshold_com = 0.03,
                         com_size_column = "share_max",
                         size_title_wrap = 10)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Saving it all####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# saving the entire dt and just the community identifiers in csv
write_csv2(unique(alluv_dt[color != "grey",c("ID_bis","Com_ID")]), here(graph_data_path,"Alt_Networks", "community_list_55_no_name.csv"))