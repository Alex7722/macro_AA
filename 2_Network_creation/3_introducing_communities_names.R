#' ---
#' title: "Script for introducing the manual names of the communities"
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
#' > WARNING: This script is not finished at all

#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)
#' # Loading packages paths and data

set.seed(3155210)
source("functions/functions_dynamics_networks_alex.R")
# source("functions/functions_networks_alex.R")
source("functions/functions_for_network_analysis.R")
source("functions/Script_paths_and_basic_objectsV2.R")
tbl_coup_list <- readRDS(here(data_path,"macro_AA","4_Networks","list_graph_position_intertemporal_naming_1969-2011.RDS"))

Year <- 2010
list_graph_position[[paste0(Year)]] <- layout_fa2_java(tbl_coup_list[[paste0(Year)]],
                                                       niter = 6000,
                                                       threads = 16)


ragg::agg_png(here(picture_path, "coupling_normal
                   png"),
        width = 60, height = 40, units = "cm", res = 300)
list_graph_position[[paste0(Year)]] %>% 
  ggraph("manual", x = x, y = y) + 
  geom_edge_arc0(aes(color = "grey", width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,2)) +
  geom_node_point(aes(x=x, y=y, size = nb_cit, fill = new_Id_com), pch = 21, alpha = 0.9, show.legend = FALSE) +
  scale_size_continuous(range = c(0.01,6)) +
  #  new_scale("size") +
  #  geom_text_repel(data=top_nodes, aes(x=x, y=y, label = Label), size = 3, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  #  geom_label_repel(data=community_labels, aes(x=x, y=y, label = Community_name, fill = color, size = Size_com), fontface="bold", alpha = 0.8, point.padding=NA, show.legend = FALSE) +
  #  scale_size_continuous(range = c(1,5)) +
  theme_void()
invisible(dev.off())

tbl_backbone <- backbone::disparity(list_graph_position[[paste0(Year)]], alpha = 0.05, narrative = TRUE, class = "igraph")

tbl_filtered <- tbl_coup_list[[paste0(Year)]] %>% activate(edges) %>% filter(weight>0.01)
tbl_filtered <- tbl_filtered %>% activate(nodes) %>% mutate(components_att = group_components(type = "weak")) %>% filter(components_att==1)


tbl_filtered <- layout_fa2_java(tbl_filtered,
                                                       niter = 1200,
                                                       threads = 16)


ragg::agg_png(here(picture_path, "coupling_filtered..png"),
              width = 60, height = 40, units = "cm", res = 300)
tbl_filtered %>% 
  ggraph("manual", x = x, y = y) + 
  geom_edge_arc0(aes(color = "grey", width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,2)) +
  geom_node_point(aes(x=x, y=y, size = nb_cit, fill = new_Id_com), pch = 21, alpha = 0.9, show.legend = FALSE) +
  scale_size_continuous(range = c(0.01,6)) +
  #  new_scale("size") +
  #  geom_text_repel(data=top_nodes, aes(x=x, y=y, label = Label), size = 3, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  #  geom_label_repel(data=community_labels, aes(x=x, y=y, label = Community_name, fill = color, size = Size_com), fontface="bold", alpha = 0.8, point.padding=NA, show.legend = FALSE) +
  #  scale_size_continuous(range = c(1,5)) +
  theme_void()
invisible(dev.off())

ggraph(tbl_filtered, "manual", x = x, y = y) +
  geom_edge_arc(aes(color = "grey", width = weight), alpha = 0.5, strength =0.2) +
  geom_node_point(aes(fill = new_Id_com, size = size), pch=21) +
  scale_edge_width_continuous(range = c(0.5, 1)) +
  theme_void() +
  # geom_label_repel(data = label_com, aes(x = mean_coord_x, y = mean_coord_y, label = as.character(Label_com), fill = color)) +
  theme(legend.position = "none") 
  # scale_fill_identity() +
  # scale_edge_colour_identity() +
  # labs(title = paste0(as.character(Year),"-",as.character(Year+time_window-1)))
ggsave(here(picture_path, "coupling_filtered.png"), width=50, height=40, units = "cm")

tbl_filtered <- layout_fa2_java(tbl_filtered,
                                                       niter = 6000,
                                                       threads = 16)


# 
# ######################### Integrating community names and creating their colors #########################-----
# community_names <- fread(paste0(graph_data_path, "Community_names.csv")) %>% data.table()
# community_names <- community_names[, Com_ID := gsub("\"\"", "", Com_ID)]
# 
# # Adding "first rank" colors to the data
# color_com <- data.table(
#   "Titre_provisoire" = sort(unique(community_names$Titre_provisoire)),
#   "Color_Com_1" = palette_com_1[1:length(unique(community_names$Titre_provisoire))],
#   "Color_Com_2" = palette_com_2[1:length(unique(community_names$Titre_provisoire))]
# )
# community_names <- merge(community_names, color_com, by = "Titre_provisoire")
# 
# # Manipulating the data table to have "second rank" colors
# color_com <- unique(community_names[order(Titre_provisoire, Titre_provisoire_long), c("Titre_provisoire", "Titre_provisoire_long", "Color_Com_1", "Color_Com_2")])
# color_com <- color_com[, `:=`(Order = 1:.N, total_titles = .N), by = list(Titre_provisoire)][, alpha_index := Order / total_titles]
# color_com <- color_com[, Color_Com := MixColor(color_com$Color_Com_1, color_com$Color_Com_2, amount1 = color_com$alpha_index)]
# 
# community_names <- merge(community_names, color_com[, c("Titre_provisoire_long", "Color_Com")], by = "Titre_provisoire_long")
# community_names$Titre_provisoire_long <- gsub("\\\\n", "\\\n", community_names$Titre_provisoire_long)


















list_graph_position <- list()
for (Year in all_years) {
  message(paste0("Running Force Atlas for the ", Year, "-", Year + time_window - 1, " window."))
  if(is.null(tbl_coup_list[[paste0(Year-1)]])){
    list_graph_position[[paste0(Year)]] <- layout_fa2_java(tbl_coup_list[[paste0(Year)]],
                                                           niter = 6000,
                                                           threads = 16)
  }
  if(!is.null(tbl_coup_list[[paste0(Year-1)]])){
    past_position <- list_graph_position[[paste0(Year-1)]] %>% activate(nodes) %>% as.data.table()
    past_position <- past_position[,.(ID_Art,x,y)]
    
    tbl <- tbl_coup_list[[paste0(Year)]] %>% 
      activate(nodes) %>% 
      left_join(past_position)
    
    list_graph_position[[paste0(Year)]] <- layout_fa2_java(tbl,
                                                           niter = 6000,
                                                           threads = 16)
    print(Year)
  }
}

saveRDS(list_graph_position, here(graph_data_path, paste0("list_graph_position", first_year, "-", last_year + time_window - 1, ".rds")))


agg_png(here(picture_path, "coupling_graph_plot_test.png"),
        width = 60, height = 40, units = "cm", res = 300)
list_graph_position[["1988"]] %>% 
  ggraph("manual", x = x, y = y) + 
  geom_edge_arc0(aes(color = Com_ID, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,2)) +
  geom_node_point(aes(x=x, y=y, size = nb_cit, fill = Com_ID), pch = 21, alpha = 0.9, show.legend = FALSE) +
  scale_size_continuous(range = c(0.01,6)) +
  #  new_scale("size") +
  #  geom_text_repel(data=top_nodes, aes(x=x, y=y, label = Label), size = 3, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  #  geom_label_repel(data=community_labels, aes(x=x, y=y, label = Community_name, fill = color, size = Size_com), fontface="bold", alpha = 0.8, point.padding=NA, show.legend = FALSE) +
  #  scale_size_continuous(range = c(1,5)) +
  theme_void()
invisible(dev.off())

# Rescaling coordinates and size
list_graph_position <- readRDS(here(graph_data_path, paste0("list_graph_position", first_year, "-", last_year + time_window - 1, ".rds")))

list_graph_position <- lapply(list_graph_position, 
                              function(tbl){tbl %>% 
                                  activate(nodes) %>% 
                                  mutate(original_x = x,
                                         original_y = y,
                                         x = rescale(x, c(0, 1200)),
                                         y = rescale(y, c(0, 1000)),
                                         size = rescale(nb_cit, c(1,100)))})


agg_png(here(picture_path, "coupling_graph_plot_rescaled.png"),
        width = 60, height = 40, units = "cm", res = 300)
list_graph_position[["1988"]] %>% 
  ggraph("manual", x = x, y = y) + 
  geom_edge_arc0(aes(color = Com_ID, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,2)) +
  geom_node_point(aes(x=x, y=y, size = nb_cit, fill = Com_ID), pch = 21, alpha = 0.9, show.legend = FALSE) +
  scale_size_continuous(range = c(0.01,6)) +
  #  new_scale("size") +
  #  geom_text_repel(data=top_nodes, aes(x=x, y=y, label = Label), size = 3, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  #  geom_label_repel(data=community_labels, aes(x=x, y=y, label = Community_name, fill = color, size = Size_com), fontface="bold", alpha = 0.8, point.padding=NA, show.legend = FALSE) +
  #  scale_size_continuous(range = c(1,5)) +
  theme_void()
invisible(dev.off())

saveRDS(list_graph_position, paste0(graph_data_path, "list_graph_position", first_year, "-", last_year + time_window - 1, ".rds"))




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
  dt[,new_Id_com:=as.character(new_Id_com)]
  
  dt[,tot_window_leiden:=.N,.(Window,new_Id_com)]
  
  dt[,Source:=new_Id_com,Id]
  dt[,Target:=shift(new_Id_com),Id]
  
  
  dt <- dt %>% rename(tot_window_leiden_Source = tot_window_leiden)
  dt[,tot_window_leiden_Target:=shift(tot_window_leiden_Source),Id]
  
  dt <- dt[,head(.SD,1),.(new_Id_com,Id)]
  
  dt <- dt[Source > Target, c("tot_window_leiden_Target", "tot_window_leiden_Source") := list(tot_window_leiden_Source, tot_window_leiden_Target)] # exchanging
  dt <- dt[Source > Target, c("Target", "Source") := list(Source, Target)] # exchanging

  
  dt[,link_strength:=.N,.(Source,Target)] # before: dt[,link_strength:=.N,.(Source,Target,Window)]
  
  dt <- dt[is.na(Target)==FALSE & Source!=Target]
  
  dt[,cosine_strength:=link_strength/sqrt(tot_window_leiden_Target*tot_window_leiden_Source)] #before: dt[,cosine_strength:=link_strength/sqrt(tot_window_leiden_Target*tot_window_leiden_Source)]
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
  dt[,new_Id_com:=as.character(new_Id_com)]
  
  id_nb_cit <- dt[,.N,.(new_Id_com, Window)]
  id_nb_cit <- id_nb_cit[,mean(N),new_Id_com]
  # dt[,tot_window_leiden:=.N,.(Window,new_Id_com)]
  
  dt[,Source:=new_Id_com,Id]
  dt[,Target:=shift(new_Id_com),Id]
  
  # 
  # dt <- dt %>% rename(tot_window_leiden_Source = tot_window_leiden)
  # dt[,tot_window_leiden_Target:=shift(tot_window_leiden_Source),Id]
  
  dt <- dt[,head(.SD,1),.(new_Id_com,Id)]
  
  # dt <- dt[Source > Target, c("tot_window_leiden_Target", "tot_window_leiden_Source") := list(tot_window_leiden_Source, tot_window_leiden_Target)] # exchanging
  dt <- dt[Source > Target, c("Target", "Source") := list(Source, Target)] # exchanging
  
  dt <- merge(dt, id_nb_cit, by.x = "Target", by.y = "new_Id_com")
  setnames(dt, "V1", "tot_window_leiden_Target")
  dt <- merge(dt, id_nb_cit, by.x = "Source", by.y = "new_Id_com")
  setnames(dt, "V1", "tot_window_leiden_Source")
  
  dt[,link_strength:=.N,.(Source,Target)] # before: dt[,link_strength:=.N,.(Source,Target,Window)]
  
  dt <- dt[is.na(Target)==FALSE & Source!=Target]
  
  dt[,cosine_strength:=link_strength/sqrt(tot_window_leiden_Target*tot_window_leiden_Source)] #before: dt[,cosine_strength:=link_strength/sqrt(tot_window_leiden_Target*tot_window_leiden_Source)]
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

#' # Renaming using the new community names (in construction)
com_list <- fread(paste0(graph_data_path, "community_list_", first_year, "-", last_year + time_window - 1, ".csv"))
com_list <- com_list %>% rename(new_Id_com = Com_ID)

intertemporal_naming <- readRDS(paste0(graph_data_path, "list_graph_position_intertemporal_naming", first_year, "-", last_year + time_window - 1, ".rds"))

# alluv_dt_noorder <- readRDS(paste0(graph_data_path, "alluv_dt_", first_year, "-", last_year + time_window - 1, ".rds"))

# intertemporal_naming_filtered <- lapply(intertemporal_naming, function(tbl)
#   (
#     tbl <- tbl %>% filter(new_Id_com %in% com_list$new_Id_com)
#   )
# )


alluv_dt_noorder <- make_into_alluv_dt(intertemporal_naming)
alluv_dt_noorder[,Id:=ID_Art]
alluv_dt_noorder <- minimize_crossing(alluv_dt_noorder)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Giving Names and Colors to Networks ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# sample
intertemporal_naming$"1999" %>% filter(new_Id_com %in% com_list$new_Id_com)
intertemporal_naming$"1999" %>% activate(nodes) %>% as.data.table()


#%%%%%%%%%%%%%%%% give names to nodes %%%%%%%%%%%%%%%%#
networks_final <- lapply(intertemporal_naming, function(tbl)
  (
    tbl <- tbl %>% 
      activate(nodes) %>% 
      left_join(com_list)
    )
  )

# sample
networks_final$"1999"
networks_final$"1999" %>% activate(nodes) %>% as.data.table()


#%%%%%%%%%%%%%%%% get all nodes of all networks to have the full list of names and make color tables %%%%%%%%%%%%%%%%#
networks_final_nodes <- lapply(networks_final, function(tbl)(tbl %>% activate(nodes) %>% as.data.table()))
networks_final_nodes <- rbindlist(networks_final_nodes, fill=TRUE)

# list meta-name
list_meta_name <- networks_final_nodes[,.N,`meta-name`]
list_sub_name <- networks_final_nodes[,.N,.(`meta-name`,`sub-name`)][order(`meta-name`)]

# Primary colors are qualitative pallet (grey if NA)
list_meta_name[,primary_color:=brewer.pal(list_meta_name[,.N],"Paired")]
list_meta_name[is.na(`meta-name`),primary_color:="#B2B2B2"]

# Secondary colors are viridis pallets, restarting each group
sec_color <-viridis(list_sub_name[,.N,`meta-name`][order(-N)][head(1)]$N)
list_sub_name[,secondary_color:=rep(sec_color, length.out = .N),`meta-name`]

color_table <- merge(list_sub_name, list_meta_name, by="meta-name", all.x=TRUE)
color_table[, c("N.x","N.y"):=NULL]
color_table[,color_nodes:= MixColor(primary_color, secondary_color, amount1 = 0.6)]
color_table[is.na(`meta-name`),color_nodes:="#B2B2B2"]


#%%%%%%%%%%%%%%%% color tbl %%%%%%%%%%%%%%%%#
networks_final <- lapply(networks_final, function(tbl)
  (
    tbl <- tbl %>% 
      activate(nodes) %>% 
      left_join(color_table)
  )
)



rm(networks)
rm(intertemporal_naming)
gc()


color_edges <- function(tbl=tbl, color_col="color_nodes")
{
  print(tbl %>% activate(nodes) %>% as.data.table() %>% .[,min(Annee_Bibliographique)])
  #Mix color for edges of different color
  tbl <- tbl %>% #mix color
    activate(edges) %>%
    mutate(com_ID_to = .N()[[color_col]][to], com_ID_from = .N()[[color_col]][from]) %>%
    mutate(color_edges = MixColor(com_ID_to, com_ID_from, amount1 = 0.5))  # .N() makes the node data available while manipulating edges
  
  return (tbl)
}


networks_final$"1969" %>% #mix color
  activate(edges) %>%
  mutate(com_ID_to = .N()[[color_col]][to], com_ID_from = .N()[[color_col]][from]) %>%
  mutate(color_edges = MixColor(com_ID_to, com_ID_from, amount1 = 0.5))  # .N() makes the node data available while manipulating edges


networks_final <- lapply(networks_final, color_edges, color_col="color_nodes")


#%%%%%%%%%%%%%%%% Saving %%%%%%%%%%%%%%%%#
nodes_lf <- lapply(networks_final, function(tbl) (tbl %>% activate(nodes) 
                                                  %>% rename(sub_name = "sub-name") 
                                                  %>% rename(meta_name = "meta-name") 
                                                  %>% as.data.table()))
nodes_lf <- lapply(nodes_lf, function(dt) (dt[, .(ID_Art, x, y, original_x, original_y, size, nb_cit, Com_ID=new_Id_com, color=color_nodes, meta_name, sub_name, Size_com)]))
nodes_lf <- rbindlist(nodes_lf, idcol = "window")
nodes_lf <- nodes_lf[, window := paste0(window, "-", as.integer(window) + time_window-1)][order(ID_Art, window)]

edges_lf <- lapply(networks_final, function(tbl) (tbl %>% activate(edges) %>% as.data.table()))
edges_lf <- lapply(edges_lf, function(dt) (dt[, .(Source, Target, weight, color_edges)]))
edges_lf <- rbindlist(edges_lf, idcol = "window")
edges_lf <- edges_lf[, window := paste0(window, "-", as.integer(window) + time_window-1)]

nodes_info <- lapply(networks_final, function(tbl) (tbl %>% activate(nodes) %>% as.data.table()))
nodes_info <- rbindlist(nodes_info,fill=TRUE)
nodes_info <- nodes_info[, head(.SD, 1), .(ID_Art)]
nodes_info <- nodes_info[, .(ID_Art, Titre, Annee_Bibliographique, Label, Revue, ESpecialite)]

write_csv(nodes_lf, paste0(platform_data, "nodes_lf.csv"))
write_csv(edges_lf, paste0(platform_data, "edges_lf.csv"))
write_csv(nodes_info, paste0(platform_data, "nodes_info.csv"))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Giving Names and Colors to Alluv####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# loading the data
alluv_dt<- copy(alluv_dt_noorder)
alluv_dt <- alluv_dt %>% left_join(com_list)
alluv_dt <- alluv_dt %>% left_join(color_table)

alluv_dt <- alluv_dt %>% rename(meta_name = "meta-name")
alluv_dt <- alluv_dt %>% rename(sub_name = "sub-name")
alluv_dt[!is.na(meta_name) & !is.na(sub_name), Label_com_name:=paste0(meta_name,"\n",sub_name)]


label <- copy(alluv_dt)
label <- label[,Window:=round(mean(as.numeric(Window))),Label_com_name]
label <- label[,.N,.(Label_com_name, Window)]
label[, N:=NULL]
label[,Label_com_name_unique := Label_com_name]

alluv_dt_graph <- merge(alluv_dt,label, by = c("Label_com_name","Window"), all.x = TRUE)

# alluv_dt_graph[!is.na(Label_com_unique), Label_com_unique:=meta_name]
alluv_dt_graph[,Window := as.character(Window)]

alluv_dt_graph$new_Id_com <- fct_reorder(alluv_dt_graph$new_Id_com, alluv_dt_graph$order,min, .desc = TRUE)

ggplot(alluv_dt_graph, aes(x = Window, y=share, stratum = new_Id_com, alluvium = Id, fill = color_nodes, label = new_Id_com)) +
  geom_stratum(alpha =1, size=1/10) +
  geom_flow() +
  theme(legend.position = "none") +
  theme_minimal() +
  scale_fill_identity() +
  ggtitle("") +
  ggrepel::geom_label_repel(stat = "stratum", size = 3, aes(label = Label_com_name_unique), max.overlaps = Inf)
  # ggsave("EER/Graphs/Intertemporal_communities2.png", width=30, height=20, units = "cm")

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