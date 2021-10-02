#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Make a List of tbl - coupling ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

dynamics_coupling_networks <- function(corpus = Corpus, 
                                   references = refs, 
                                   source = "ID_Art", 
                                   target = "ItemID_Ref", 
                                   time_variable = Annee_Bibliographique,
                                   time_window = 5, 
                                   weight_treshold_value = 1)
{  
  #' This function create a list of tbl from a corpus and its references
  #' 
  #' @corpus
  #' The corpus with all nodes and articles
  #' 
  #' @references
  #' The list of all references (NULL references willbe removed )
  #'
  #' @source
  #' The column name of the citing articles.
  #'
  #' @target
  #' The column name of the references that are cited.
  #' 
  #' @time_variable
  #' Variable that identify the year of publication
  #' 
  #' @time_window
  #' How much time should be covered by the network
  #' 
  #' @weight_treshold_value
  #' Treshold value for coupling (see function)

  Nodes_coupling <- Corpus
  Nodes_coupling[,ID_Art_Source:=as.character(get(source))]
  Nodes_coupling[,Id:=as.character(get(source))]
  Nodes_coupling[,ItemID_Ref_Target:=as.character(get(target))]
  
  Edges <- references
  Edges[,ID_Art_Source:=as.character(get(source))]
  Edges[,Id:=as.character(get(source))]
  Edges[,ItemID_Ref_Target:=as.character(get(target))]
  Edges <- Edges[ItemID_Ref_Target!="NULL",.(ID_Art_Source, ItemID_Ref_Target)]
  
  ######################### Dynamics networks **********************
  Nodes_coupling <- Nodes_coupling[order(Annee_Bibliographique)]
  
  # Find the time_window
  first_year <- Nodes_coupling[order(Annee_Bibliographique), head(.SD, 1)]$Annee_Bibliographique
  last_year <- (as.numeric(Nodes_coupling[order(-Annee_Bibliographique), head(.SD, 1)]$Annee_Bibliographique) - time_window +1) # +1 to get the very last year in the window
  all_years <- first_year:last_year
  
  # Prepare our list
  tbl_coup_list <- list()
  
  for (Year in all_years) {
    nodes_of_the_year <- Nodes_coupling[Annee_Bibliographique>=Year & Annee_Bibliographique<Year+time_window]
    edges_of_the_year <- Edges[ID_Art_Source %in% nodes_of_the_year$ID_Art_Source]
    # size of nodes
    nb_cit <- edges_of_the_year[, .N, ItemID_Ref_Target]
    colnames(nb_cit)[colnames(nb_cit) == "N"] <- "size"
    nodes_of_the_year <- merge(nodes_of_the_year, nb_cit, by.x = "ItemID_Ref_Target", by.y = "ItemID_Ref_Target", all.x = TRUE)
    nodes_of_the_year[is.na(size),size:=0]
    # coupling
    edges_of_the_year <- coupling_strength_bibliographic_coupling(edges_of_the_year, "ID_Art_Source", "ItemID_Ref_Target", weight_threshold = weight_treshold_value, output_in_character = TRUE)
    
    # remove nodes with no edges
    nodes_of_the_year <- nodes_of_the_year[ID_Art_Source %in% edges_of_the_year$from | ID_Art_Source %in% edges_of_the_year$to]
    # make tbl
    tbl_coup_list[[as.character(Year)]] <- tbl_graph(nodes = nodes_of_the_year, edges = edges_of_the_year, directed = FALSE, node_key = "Id")
    print(Year)
  }
  
  return (tbl_coup_list)
}

dynamics_direct_networks <- function(corpus = Corpus, 
                                       references = refs, 
                                       source = "ID_Art", 
                                       target = "ItemID_Ref", 
                                       time_variable = Annee_Bibliographique,
                                       time_window = 5, 
                                       weight_treshold_value = 1)
{  
  #' This function create a list of tbl from a corpus and its references
  #' 
  #' @corpus
  #' The corpus with all nodes and articles
  #' 
  #' @references
  #' The list of all references (NULL references willbe removed )
  #'
  #' @source
  #' The column name of the citing articles.
  #'
  #' @target
  #' The column name of the references that are cited.
  #' 
  #' @time_variable
  #' Variable that identify the year of publication
  #' 
  #' @time_window
  #' How much time should be covered by the network
  #' 
  #' @weight_treshold_value
  #' Treshold value for coupling (see function)
  
  Nodes_coupling <- Corpus
  Nodes_coupling[,ID_Art_Source:=as.character(get(source))]
  Nodes_coupling[,Id:=as.character(get(source))]
  Nodes_coupling[,ItemID_Ref_Target:=as.character(get(target))]
  
  Edges <- references
  Edges[,ID_Art_Source:=as.character(get(source))]
  Edges[,Id:=as.character(get(source))]
  Edges[,ItemID_Ref_Target:=as.character(get(target))]
  Edges <- Edges[ItemID_Ref_Target!="NULL",.(ID_Art_Source, ItemID_Ref_Target)]
  
  ######################### Dynamics networks **********************
  Nodes_coupling <- Nodes_coupling[order(Annee_Bibliographique)]
  
  # Find the time_window
  first_year <- Nodes_coupling[order(Annee_Bibliographique), head(.SD, 1)]$Annee_Bibliographique
  last_year <- (as.numeric(Nodes_coupling[order(-Annee_Bibliographique), head(.SD, 1)]$Annee_Bibliographique) - time_window +1) # +1 to get the very last year in the window
  all_years <- first_year:last_year
  
  # Prepare our list
  tbl_coup_list <- list()
  
  for (Year in all_years) {
    nodes_of_the_year <- Nodes_coupling[Annee_Bibliographique>=Year & Annee_Bibliographique<Year+time_window]
    edges_of_the_year <- Edges[ID_Art_Source %in% nodes_of_the_year$ID_Art_Source]
    # size of nodes
    nb_cit <- edges_of_the_year[, .N, ItemID_Ref_Target]
    colnames(nb_cit)[colnames(nb_cit) == "N"] <- "size"
    nodes_of_the_year <- merge(nodes_of_the_year, nb_cit, by.x = "ItemID_Ref_Target", by.y = "ItemID_Ref_Target", all.x = TRUE)
    nodes_of_the_year[is.na(size),size:=0]
    # Direct links
    edges_of_the_year <- edges_of_the_year[ItemID_Ref_Target  %in% nodes_of_the_year$ItemID_Ref_Target]
    
    nodes_of_the_year[,ID_Art_Target:=ID_Art_Source]
    
    edges_of_the_year <- merge(edges_of_the_year, nodes_of_the_year[,.(ItemID_Ref_Target,ID_Art_Target)], by="ItemID_Ref_Target")
    edges_of_the_year[,Source:=ID_Art_Source]
    edges_of_the_year[,from:=ID_Art_Source]
    
    edges_of_the_year[,Target:=ID_Art_Target]
    edges_of_the_year[,to:=ID_Art_Target]
    edges_of_the_year[,weight:=1]
    
    
    # remove nodes with no edges
    nodes_of_the_year <- nodes_of_the_year[ID_Art_Source %in% edges_of_the_year$from | ID_Art_Source %in% edges_of_the_year$to]
    # make tbl
    tbl_coup_list[[as.character(Year)]] <- tbl_graph(nodes = nodes_of_the_year, edges = edges_of_the_year, directed = FALSE, node_key = "Id")
    print(Year)
  }
  
  return (tbl_coup_list)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Intertemporal Positions ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


dynamics_layoutfa2<- function(list_of_network = list_graph)
{  
  #' This function create a list of tbl from a corpus and its references
  #' 
  #' @corpus
  #' The corpus with all nodes and articles
  
  list_graph_position <- list()
  for (Year in all_years) {
    if(is.null(list_of_network[[paste0(Year-1)]])){
      list_graph_position[[paste0(Year)]] <- layout_fa2(list_of_network[[paste0(Year)]])
    }
    if(!is.null(list_of_network[[paste0(Year-1)]])){
      past_position <- list_graph_position[[paste0(Year-1)]] %>% activate(nodes) %>% as.data.table()
      past_position <- past_position[,.(ID_Art,x,y)]
      
      tbl <- list_of_network[[paste0(Year)]] %>% activate(nodes) %>% left_join(past_position)
      
      list_graph_position[[paste0(Year)]] <- layout_fa2(tbl)
    }
  }
  return (list_graph_position)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Intertemporal Naming: Find Communities Across Time ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

intertemporal_naming_function <- function(list_graph = tbl_list, 
                                community_column = Leiden1,
                                individual_ids = Id,
                                treshold_similarity = 0.55){
  #' This function 
  #' 
  #' @tbl_list
  #' A list of tbl with the name of the elements as the year of the oldest publications
  
  ######################### Prepare everything **********************
  set.seed(500) # set seed otherwise the name of communities change everytime the function is run
  all_years <- as.numeric(names(list_graph)) # get all years to study
  unique_ids <- stringi::stri_rand_strings(10000, 8) # unique Ids
  
  # Important variables
  Leiden1 <- deparse(substitute(community_column))
  Id <- deparse(substitute(individual_ids))
  
  
  intertemporal_naming <- list()
  for (Year in all_years) {
    
    ######################### For the first year, let's just give them unique ids **********************
    
    if(is.null(list_graph[[paste0(Year-1)]])){
      
      dt_year <- list_graph[[paste0(Year)]] %>% activate(nodes) %>% as.data.table()
      dt_year <- dt_year[,.(Id,Leiden1)]
      #n_com
      n_com <- dt_year[,.N,Leiden1][,.N]
      #give a unique ids to com
      dt_id_com <- data.table(Id_com = unique_ids[1:n_com], Leiden1 = dt_year[,.N,Leiden1]$Leiden1)
      #remove ids taken from list
      unique_ids <- unique_ids[-c(1:n_com)]
      #merge with unique id
      dt_year <- merge(dt_year, dt_id_com, by= Leiden1)
      dt_year <- dt_year[,new_Id_com:=Id_com]
      dt_year <- dt_year[,.(Id,new_Id_com)]
      
      tbl <- list_graph[[paste0(Year)]] %>% activate(nodes) %>% left_join(dt_year)
      intertemporal_naming[[paste0(Year)]] <- tbl
      
    }
    
    ######################### For other years, we need to take the previous years and give new names to community of the new year **********************
    
    else if(!is.null(list_graph[[paste0(Year-1)]])){
      
      ######################### Communities from previous year **********************
      
      dt_year <- intertemporal_naming[[paste0(Year-1)]] %>% activate(nodes) %>% as.data.table()
      dt_year <- dt_year[,.(Id,new_Id_com)]
      #give a unique ids to com
      dt_year <- dt_year[,Id_com:=new_Id_com]
      
      
      
      ######################### Communities from this year **********************
      
      dt_year2 <- list_graph[[paste0(Year)]] %>% activate(nodes) %>% as.data.table()
      dt_year2 <- dt_year2[,.(Id,Leiden1)]
      #n_com
      n_com <- dt_year2[,.N,Leiden1][,.N]
      #give a unique ids to com
      dt_id_com <- data.table(Id_com = unique_ids[1:n_com], Leiden1 = dt_year2[,.N,Leiden1]$Leiden1)
      #remove ids taken from list
      unique_ids <- unique_ids[-c(1:n_com)]
      #merge with unique id
      dt_year2 <- merge(dt_year2, dt_id_com, by= Leiden1)
      
      
      # Let's compute the shares of movements relatively to all articles, y articles, and x articles.(add  <,all.y = TRUE, all.x= TRUE)> to merge for taking NAs into account)
      alluv <- merge(dt_year, dt_year2, by= "Id")
      alluv[,n_com_full:=.N]
      alluv[,n_com_move_full:=.N,.(Id_com.x, Id_com.y)]
      alluv[,share_full:=n_com_move_full/n_com_full]
      
      alluv[,n_com_y:=.N, Id_com.y]
      alluv[,n_com_move_full:=.N,.(Id_com.x, Id_com.y)]
      alluv[,share_full_y:=n_com_move_full/n_com_y]
      
      alluv[,n_com_x:=.N, Id_com.x]
      alluv[,n_com_move_full:=.N,.(Id_com.x, Id_com.y)]
      alluv[,share_full_x:=n_com_move_full/n_com_x]
      
      alluv <- alluv[,.N,.(Id_com.x, Id_com.y, share_full, share_full_x, share_full_y)][order(share_full, share_full_y, share_full_x)]
      
      # Make into a list by community X, so that we can find what happen to each community X
      
      alluv_list <- split(alluv, c(alluv$Id_com.x))
      
      # For each community X we look at what happen to their nodes and we extract a list to merge to give new names to com.y
      
      for (com.x in names(alluv_list)) {
        
        dt <- alluv_list[[paste0(com.x)]][order(-share_full)] # On ordonne par l'importance du mouvement entre deux communautés
        
        # On regarde le lien le plus fort de chaque communauté x, 
        # si z% des noeuds vont vers une communauté unique, 
        # + cette communauté est composée à z% par les articles de Y, alors c'est la même communauté
        
        if(dt$share_full_x[[1]]>treshold_similarity & dt$share_full_y[[1]]>treshold_similarity){
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
      new_dt_year2 <- merge(dt_year2, alluv_all, by.x= "Id_com", by.y = "Id_com.y", all.x = TRUE)
      # Si la communauté n'a pas de nouveau nom, elle garde l'ancien nom
      new_dt_year2 <- new_dt_year2[is.na(new_Id_com.y)==TRUE, new_Id_com.y:=Id_com]
      # Renommer la colonne
      new_dt_year2 <- new_dt_year2 %>% rename(new_Id_com = new_Id_com.y)
      
      # Garde-fou: Break the loop is the number of community change after procedure
      if(new_dt_year2[,.N,new_Id_com][,.N]!=n_com) {
        print(paste0("We lost communities in ", as.character(Year-1),"-",as.character(Year)," transformation. Check for ",as.character(Year)," data.table." ))
        break;
      }
      
      # On injecte les nouveaux noms dans le tbl, et on refait la liste des tbl avec les nouveaux noms
      tbl <- list_graph[[paste0(Year)]] %>% activate(nodes) %>% left_join(new_dt_year2)
      intertemporal_naming[[paste0(Year)]] <- tbl
      
    }
  }
  return (intertemporal_naming)
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Compute values for alluv ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

make_into_alluv_dt <- function(intertemporal_networks = intertemporal_networks, community_column=new_Id_com){
  
  #' This function 
  #' 
  #' @tbl_list
  #' A list of tbl with the name of the elements as the year of the oldest publications

  new_Id_com <- deparse(substitute(community_column))
  
  networks <- lapply(intertemporal_networks, function(tbl)(tbl %>% activate(nodes) %>% as.data.table))
  
  networks <- lapply(networks, function(dt)(dt[,.(Id,new_Id_com,Titre)]))
  
  alluv_dt<- rbindlist(networks, idcol = "Window")
  alluv_dt[,n_id_window:=.N, Window]
  alluv_dt[,share:=1/n_id_window]
  
  alluv_dt[,Leiden1:=new_Id_com]
  alluv_dt[,share_leiden_total:=.N/alluv_dt[,.N],Leiden1]
  
  alluv_dt[,tot_window_leiden:=.N,.(Window,Leiden1)]
  alluv_dt[,tot_window:=.N,.(Window)]
  alluv_dt[,share_leiden:=tot_window_leiden/tot_window]
  alluv_dt[,share_leiden_max:=max(share_leiden),Leiden1]
  # alluv_dt<-merge(alluv_dt, unique_ids_color[,.(Leiden1,color)], by="Leiden1",all.x = TRUE)
  
  n_years <- alluv_dt[, head(.SD, 1), .(Window,Leiden1)][,.N,Leiden1][order(N)]
  n_years <- n_years %>% rename(n_years = N)
  
  alluv_dt <- merge(alluv_dt, n_years, by="Leiden1",all.x = TRUE)
  return (alluv_dt)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Order Alluv to Minimize Crossing ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

minimize_crossing <- function(alluv_dt = alluv_dt, stratum = new_Id_com, alluvium = Id, x = Window){
  
  #' This function 
  #' 
  #' @alluv_dt
  #' The dt used for the alluvial
  #' @stratum
  #' Stratum column
  #' @alluvium
  #' Alluvium column
  #' @x
  #' x column
  require(tidyverse)
  require(data.table)
  require(ggalluvial)
  require(tidygraph)
  require(ggplot2)
  require(forcats)
  
  new_Id_com <- deparse(substitute(stratum))
  Id <- deparse(substitute(alluvium))
  Window <- deparse(substitute(x))
  
  dt<-alluv_dt[order(Id,Window)][,.(new_Id_com, Id, Window)]
  
  dt[,tot_window_leiden:=.N,.(Window,new_Id_com)]
  
  dt[,Source:=new_Id_com,Id]
  dt[,Target:=shift(new_Id_com),Id]
  
  
  dt <- dt %>% rename(tot_window_leiden_Source = tot_window_leiden)
  dt[,tot_window_leiden_Target:=shift(tot_window_leiden_Source),Id]
  
  dt <- dt[Source > Target, c("Target", "Source") := list(Source, Target)] # exchanging
  dt <- dt[Source > Target, c("tot_window_leiden_Target", "tot_window_leiden_Source") := list(tot_window_leiden_Source, tot_window_leiden_Target)] # exchanging

  
  dt[,link_strength:=.N,.(Source,Target,Window)]
  
  dt <- dt[is.na(Target)==FALSE & Source!=Target]
  
  dt[,cosine_strength:=link_strength/sqrt(tot_window_leiden_Target*tot_window_leiden_Source)]
  dt[,max_cosine_strength:=max(cosine_strength),.(Source,Target)]
  
  dt<-dt[,.N,.(Source,Target,max_cosine_strength)][order(-N)]
  
  #Make the dt for naming
  edges_meta<-dt
  edges_meta[,Source:=as.character(Source)]
  edges_meta[,Target:=as.character(Target)]
  edges_meta[,from:=Source]
  edges_meta[,to:=Target]
  edges_meta[,weight:=max_cosine_strength]
  
  nodes_meta <-alluv_dt[,.N,new_Id_com]
  nodes_meta <-nodes_meta[,new_Id_com:=as.character(new_Id_com)]
  nodes_meta <-nodes_meta[,Id:=new_Id_com]
  
  tbl_meta<-tbl_graph(nodes = nodes_meta, edges = edges_meta, directed = FALSE, node_key = "new_Id_com")
  components <- tbl_meta %>% 
    activate(nodes) %>% 
    mutate(components_att = group_components(type = "weak")) %>% 
    as.data.table()
  
  components[,size_compo:=.N,components_att][order(-N)]
  components <- components[size_compo==1,components_att:=0]
  setnames(components, "components_att", paste0("components_att_","0"))
  components <- components[,.(new_Id_com, "components_att_0"= get("components_att_0"))]
  
  for (links_to_remove in unique(dt[order(max_cosine_strength)]$max_cosine_strength)) {
    dt<-dt[max_cosine_strength>links_to_remove]
    edges_meta<-dt
    edges_meta[,Source:=as.character(Source)]
    edges_meta[,Target:=as.character(Target)]
    edges_meta[,from:=Source]
    edges_meta[,to:=Target]
    edges_meta[,weight:=max_cosine_strength]
    
    nodes_meta <-alluv_dt[,.N,new_Id_com]
    nodes_meta <-nodes_meta[,new_Id_com:=as.character(new_Id_com)]
    nodes_meta <-nodes_meta[,Id:=new_Id_com]
    
    tbl_meta<-tbl_graph(nodes = nodes_meta, edges = edges_meta, directed = FALSE, node_key = "Id")
    
    components2 <- tbl_meta %>% 
      activate(nodes) %>% 
      mutate(components_att = group_components(type = "weak")) %>% 
      as.data.table()
    
    components2[,size_compo:=.N,components_att][order(-N)]
    components2 <- components2[size_compo==1,components_att:=0]
    name <- paste0("components_att_", links_to_remove)
    setnames(components2, "components_att", name)
    components2 <- components2[,.(new_Id_com, get(name))]
    setnames(components2, "V2", name)
    
    components <- merge(components, components2, all.x = TRUE, all.y = TRUE, by= "new_Id_com")
    
  }
  
  columns_to_paste <- names(components)
  columns_to_paste <- columns_to_paste[columns_to_paste != "new_Id_com"] 
  
  community_order <- components %>% unite(order, c(columns_to_paste), sep = " ", remove = FALSE)
  community_order <- community_order[,.(new_Id_com,order)][order(order)]
  
  
  alluv_dt_meta <-merge(alluv_dt,community_order, by="new_Id_com", all.x = TRUE)
  
  alluv_dt_meta$new_Id_com <- fct_reorder(alluv_dt_meta$new_Id_com, alluv_dt_meta$order,min, .desc = TRUE)
  return(alluv_dt_meta)
}

meta_grouping <- function(alluv_dt = alluv_dt, stratum = new_Id_com, alluvium = Id, x = Window, treshold_meta_groups = 0.35){
  
  #' This function 
  #' 
  #' @alluv_dt
  #' The dt used for the alluvial
  #' @stratum
  #' Stratum column
  #' @alluvium
  #' Alluvium column
  #' @x
  #' x column
  require(tidyverse)
  require(data.table)
  require(ggalluvial)
  require(tidygraph)
  require(ggplot2)
  require(forcats)
  
  new_Id_com <- deparse(substitute(stratum))
  Id <- deparse(substitute(alluvium))
  Window <- deparse(substitute(x))
  
  dt<-alluv_dt[order(Id,Window)][,.(new_Id_com, Id, Window)]
  
  dt[,tot_window_leiden:=.N,.(Window,new_Id_com)]
  
  dt[,Source:=new_Id_com,Id]
  dt[,Target:=shift(new_Id_com),Id]
  
  
  dt <- dt %>% rename(tot_window_leiden_Source = tot_window_leiden)
  dt[,tot_window_leiden_Target:=shift(tot_window_leiden_Source),Id]
  
  dt <- dt[Source > Target, c("Target", "Source") := list(Source, Target)] # exchanging
  dt <- dt[Source > Target, c("tot_window_leiden_Target", "tot_window_leiden_Source") := list(tot_window_leiden_Source, tot_window_leiden_Target)] # exchanging

  
  dt[,link_strength:=.N,.(Source,Target,Window)]
  
  dt <- dt[is.na(Target)==FALSE & Source!=Target]
  
  dt[,cosine_strength:=link_strength/sqrt(tot_window_leiden_Target*tot_window_leiden_Source)]
  dt[,max_cosine_strength:=max(cosine_strength),.(Source,Target)]
  
  dt<-dt[,.N,.(Source,Target,max_cosine_strength)][order(-N)]
  
  #Make the dt for naming
  edges_meta<-dt
  edges_meta[,Source:=as.character(Source)]
  edges_meta[,Target:=as.character(Target)]
  edges_meta[,from:=Source]
  edges_meta[,to:=Target]
  edges_meta[,weight:=max_cosine_strength]
  
  nodes_meta <-alluv_dt[,.N,new_Id_com]
  nodes_meta <-nodes_meta[,new_Id_com:=as.character(new_Id_com)]
  nodes_meta <-nodes_meta[,Id:=new_Id_com]
  
  tbl_meta<-tbl_graph(nodes = nodes_meta, edges = edges_meta, directed = FALSE, node_key = "new_Id_com")
  components <- tbl_meta %>% 
    activate(nodes) %>% 
    mutate(components_att = group_components(type = "weak")) %>% 
    as.data.table()
  
  components[,size_compo:=.N,components_att][order(-N)]
  components <- components[size_compo==1,components_att:=0]
  setnames(components, "components_att", paste0("components_att_","0"))
  components <- components[,.(new_Id_com, "components_att_0"= get("components_att_0"))]
  
  treshold <- treshold_meta_groups
  
  for (links_to_remove in treshold) {
    dt<-dt[max_cosine_strength>links_to_remove]
    edges_meta<-dt
    edges_meta[,Source:=as.character(Source)]
    edges_meta[,Target:=as.character(Target)]
    edges_meta[,from:=Source]
    edges_meta[,to:=Target]
    edges_meta[,weight:=max_cosine_strength]
    
    nodes_meta <-alluv_dt[,.N,new_Id_com]
    nodes_meta <-nodes_meta[,new_Id_com:=as.character(new_Id_com)]
    nodes_meta <-nodes_meta[,Id:=new_Id_com]
    
    
    tbl_meta<-tbl_graph(nodes = nodes_meta, edges = edges_meta, directed = FALSE, node_key = "new_Id_com")
    
    components2 <- tbl_meta %>% 
      activate(nodes) %>% 
      mutate(components_att = group_components(type = "weak")) %>% 
      as.data.table()
    
    components2[,size_compo:=.N,components_att][order(-N)]
    components2 <- components2[size_compo==1,components_att:=0]
    name <- paste0("components_att_", links_to_remove)
    setnames(components2, "components_att", name)
    components2 <- components2[,.(new_Id_com, get(name))]
    setnames(components2, "V2", name)
    
    components <- merge(components, components2, all.x = TRUE, all.y = TRUE, by= "new_Id_com")
    
  }
  
  columns_to_paste <- names(components)
  columns_to_paste <- columns_to_paste[columns_to_paste != "new_Id_com"] 
  
  community_order <- components %>% unite(meta_group, c(columns_to_paste), sep = " ", remove = FALSE)
  community_order <- community_order[,.(new_Id_com,meta_group)][order(meta_group)]
  
  
  alluv_dt_meta <-merge(alluv_dt,community_order, by="new_Id_com", all.x = TRUE)
  
  return(alluv_dt_meta)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Color and Labels ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
