# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################## PART I: LOADING PACKAGES, PATH AND DATA ####################################--------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
##################### Packages, paths, etc. ############################################--------------

source("~/macro_AA/functions/functions_for_network_analysis.R")
source("~/macro_AA/dynamic_networks/Script_paths_and_basic_objects.R")


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
      
    #  V(list_graph[[paste0(Year)]])$Id <- as.character(V(list_graph[[paste0(Year)]])$Id)
      #new_dt_year2$Id <- as.character(new_dt_year2$Id)
      # On injecte les nouveaux noms dans le tbl, et on refait la liste des tbl avec les nouveaux noms
      tbl <- list_graph[[paste0(Year)]] %>% activate(nodes) %>% left_join(new_dt_year2, by = "ID_Art")
      intertemporal_naming[[paste0(Year)]] <- tbl
      
    }
  }
  return (intertemporal_naming)
}

intertemporal_naming <- intertemporal_naming_function(tbl_list = list_graph, community_column = "Com_ID", individual_ids = "ID_Art")

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

alluv_dt <- make_into_alluv_dt(intertemporal_networks = intertemporal_naming)

alluv_dt$new_Id_com   <- as.factor(alluv_dt$new_Id_com)
alluv_dt$new_Id_com <- fct_reorder(alluv_dt$new_Id_com, alluv_dt$share_leiden_total,min, .desc = FALSE)
######################### Colors **********************
#color <- brewer.pal(8, name = "Dark2")
#color2 <- brewer.pal(12, name = "Paired")
#color3 <- brewer.pal(12, name = "Set3")
#color_final <- append(color, color2)
#color_final <- append(color_final, color3)
unique_ids_color <- data.table(
  Leiden1 = as.character(alluv_dt[n_years>2 & share_leiden>=0.05,.N,new_Id_com]$new_Id_com), 
  color = mypalette[c(1:alluv_dt[n_years>2 & share_leiden>=0.05,.N,new_Id_com][,.N])])
alluv_dt<-merge(alluv_dt, unique_ids_color[,.(Leiden1,color)], by="Leiden1",all.x = TRUE)
alluv_dt[is.na(color)==TRUE,color:="grey"]

######################### Label **********************
label <- copy(alluv_dt)
label <- label[,Window:=round(mean(as.numeric(Window))),new_Id_com][color!="grey", head(.SD, 1), .(new_Id_com)]
label[,Label:=new_Id_com]

alluv_dt<-merge(alluv_dt,label[,.(new_Id_com,Window,Label)], by = c("new_Id_com","Window"), all.x = TRUE) 

ggplot(alluv_dt, aes(x = Window, y=share, stratum = new_Id_com, alluvium = ID_Art, fill = color, label = new_Id_com)) +
  scale_fill_identity("Disciplines", labels = alluv$new_Id_com.x, breaks = alluv$color, guide = "legend") +
  geom_flow() +
  geom_stratum(alpha =1, size=1/10) +
  theme(legend.position = "none") +
  geom_label(stat = "stratum", size = 5, aes(label = Label)) +
  ggtitle("") +
  ggsave(paste0(picture_path,"alluvial.png"), width = 40, height = 30, units = "cm")

################## search for tf-idf ############################
alluv_dt<- alluv_dt[, Com_ID := new_Id_com]
tf_idf_results <- tf_idf(tbl_graph(alluv_dt), unique_ids_color, 15, 4, treshold_com = 0.05, size_title_wrap=10)
tf_idf_results 

saveRDS(tf_idf_results, paste0(graph_data_path, "tf_idf_alluvial", first_year, "-", last_year + time_window - 1, ".rds"))

######################### Label **********************
alluv_dt<-alluv_dt[, c("Label"):=NULL]
label[new_Id_com=="c6Nqp2v2", Label:="Macroeconomics"]
label[new_Id_com=="EEEzS4Be", Label:="Energy"]
label[new_Id_com=="piHtlAjF", Label:="Finance: Bubbles"]
label[new_Id_com=="p45jKmQu", Label:="Finance"]
label[new_Id_com=="LxxETsng", Label:="Finance: Taxes"]
label[new_Id_com=="GrfdrlDw", Label:="Environment: Technologies"]
label[new_Id_com=="82I5q6j7", Label:="Environment: Agriculture"]
label[new_Id_com=="CtSjS1j0", Label:="Knowledge Economics"]
label[new_Id_com=="Goht2842", Label:="Urban Economics"]
label[new_Id_com=="gjLRNe3f", Label:="Market: Labor"]
label[new_Id_com=="5hejpPdE", Label:="Logistics"]
label[new_Id_com=="rh5rsLvO", Label:="Interactions"]
label[new_Id_com=="VcbF4o2X", Label:="Applied ABM and GT"]
label[new_Id_com=="2ouXdo0x", Label:="Supply Chain"]
label[,Label_com:=Label]
alluv_dt<-merge(alluv_dt,label[,.(new_Id_com,Window,Label)], by = c("new_Id_com","Window"), all.x = TRUE )
label[,Leiden1:=Label_com]
alluv_dt<-alluv_dt[, c("Leiden1"):=NULL]
alluv_dt <- merge(alluv_dt,label[,.(new_Id_com,Leiden1)], by = c("new_Id_com"), all.x = TRUE )
alluv_dt[is.na(Leiden1)==TRUE,Leiden1:=new_Id_com]


alluv_dt$new_Id_com <- fct_reorder(alluv_dt$new_Id_com, alluv_dt$n_years,min, .desc = FALSE)
alluv_dt$new_Id_com <- fct_reorder(alluv_dt$new_Id_com, alluv_dt$color2,min, .desc = TRUE)
ggplot(alluv_dt,
       aes(x = Window, y=share, stratum = new_Id_com, alluvium = Id,
           fill = color2, label = new_Id_com)) +
  scale_fill_identity("Disciplines", labels = alluv$new_Id_com.x, breaks = alluv$color, guide = "legend") +
  geom_flow(aes.bind = "flows") +
  geom_stratum(alpha =1, size=1/10,) +
  theme(legend.position = "none") +
  geom_label_repel(stat = "stratum", size = 5, aes(label = Label)) +
  # geom_label_repel(aes(label = Label)) +
  ggtitle("") +
  ggsave("Graphs/Intertemporal_communities.png", width=30, height=20, units = "cm")


alluv_dt$new_Id_com <- as.character(alluv_dt$new_Id_com)
ggplot(alluv_dt,
       aes(x = Window, y=share, stratum = Leiden1, alluvium = Id,
           fill = color2, label = Leiden1)) +
  scale_fill_identity("Disciplines", labels = alluv$new_Id_com.x, breaks = alluv$color, guide = "legend") +
  geom_flow(aes.bind = "flows") +
  geom_stratum(alpha =1, size=1/10,) +
  theme(legend.position = "none") +
  geom_label_repel(stat = "stratum", size = 5, aes(label = Label)) +
  # geom_label_repel(aes(label = Label)) +
  ggtitle("") +
  ggsave("Graphs/Intertemporal_communities2.png", width=30, height=20, units = "cm")

