# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################## PART I: LOADING PACKAGES, PATH AND DATA ####################################--------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
##################### Packages, paths, etc. ############################################--------------

source("~/macro_AA/Static_Network_Analysis/functions_for_network_analysis.R")
source("~/macro_AA/Interactive_and_dynamic_networks/Script_paths_and_basic_objects.R")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################## PART I: LOADING PACKAGES, PATH AND DATA ####################################--------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################## 1) Make a List of tbl - coupling ##########################-----------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# loading the files 
list_graph <- readRDS(paste0(graph_data_path, "list_graph_", first_year, "-", last_year + time_window - 1, ".rds"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Intertemporal Naming: Find Communities Across Time ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

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
                          c("Leiden1","Id"))
      dt_year <- dt_year[,.(Id,Leiden1)]

      #give a unique ids to com
      dt_id_com <- data.table(Id_com = unique_ids[1:dt_year[,.N,Leiden1][,.N]],
                              Leiden1 = dt_year[,.N,Leiden1]$Leiden1)
      
      #remove ids taken from list
      unique_ids <- unique_ids[-c(1:dt_year[,.N,Leiden1][,.N])]
      #merge with unique id
      dt_year <- merge(dt_year, dt_id_com, by= "Leiden1")
      dt_year <- dt_year[,new_Id_com:=Id_com]
      dt_year <- dt_year[,.(Id,new_Id_com)]
      
      list_graph[[paste0(Year)]] <- list_graph[[paste0(Year)]] %>% activate(nodes) %>% left_join(dt_year)
      intertemporal_naming[[paste0(Year)]] <- list_graph[[paste0(Year)]]
      
    }
    
    ######################### For other years, we need to take the previous years and give new names to community of the new year **********************
    
    else if(!is.null(list_graph[[paste0(Year-1)]])){
      
      ######################### Communities from previous year **********************
      
      dt_year <- intertemporal_naming[[paste0(Year-1)]] %>% activate(nodes) %>% as.data.table()
      dt_year <- dt_year[,.(Id,new_Id_com)]
      #give a unique ids to com
      dt_year <- dt_year[,Id_com:=new_Id_com]
      
      
      ######################### Communities from this year **********************
      dt_year2 <- setnames(list_graph[[paste(Year)]] %>% activate(nodes) %>% as.data.table(),
                          c(community_column,individual_ids), 
                          c("Leiden1","Id"))
      dt_year2 <- dt_year2[,.(Id,Leiden1)]
      #give a unique ids to com
      dt_id_com <- data.table(Id_com = unique_ids[1:dt_year2[,.N,Leiden1][,.N]], Leiden1 = dt_year2[,.N,Leiden1]$Leiden1)
      #remove ids taken from list
      unique_ids <- unique_ids[-c(1:dt_year2[,.N,Leiden1][,.N])]
      #merge with unique id
      dt_year2 <- merge(dt_year2, dt_id_com, by= "Leiden1")
      
      
      # Let's compute the shares of movements relatively to all articles, y articles, and x articles.(add  <,all.y = TRUE, all.x= TRUE)> to merge for taking NAs into account)
      alluv <- merge(dt_year, dt_year2, by= "Id")
    
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
        
        if(dt$share_full_x[[1]]>threshold_similarity & dt$share_full_y[[1]]>threshold_similarity){
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
      
      # On injecte les nouveaux noms dans le tbl, et on refait la liste des tbl avec les nouveaux noms
      tbl <- list_graph[[paste0(Year)]] %>% activate(nodes) %>% left_join(new_dt_year2)
      intertemporal_naming[[paste0(Year)]] <- tbl
      
    }
  }
  return (intertemporal_naming)
}
