#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################## PART I: LOADING PACKAGES, PATH AND DATA ####################################--------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
##################### Packages, paths, etc. ############################################--------------

source("~/macro_AA/Static_Network_Analysis/functions_for_network_analysis.R")
source("~/macro_AA/Interactive_and_dynamic_networks/Script_paths_and_basic_objects.R")

##################### Loading Data ############################################--------------

nodes_JEL <- readRDS(paste0(data_path,"JEL_matched_corpus_nodes.rds"))
nodes_old_JEL <- readRDS(paste0(data_path,"Old_JEL_matched_corpus_nodes.rds"))
nodes_JEL <- rbind(nodes_JEL,nodes_old_JEL)
rm("nodes_old_JEL")

edges_JEL <- readRDS(paste0(data_path,"JEL_matched_corpus_edges.rds"))
edges_old_JEL <- readRDS(paste0(data_path,"Old_JEL_matched_corpus_edges.rds"))
edges_JEL <- rbind(edges_JEL,edges_old_JEL)
rm("edges_old_JEL")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################ 1) Bibliographic Coupling #################################-------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

time_window <- 5
weight_treshold_value <- 1
nodes_JEL <- nodes_JEL[order(Annee_Bibliographique)]

#Edges <- Edges[aut_id==author]
# Find the time_window
first_year <- nodes_JEL[order(Annee_Bibliographique), head(.SD, 1)]$Annee_Bibliographique
last_year <- (nodes_JEL[order(-Annee_Bibliographique), head(.SD, 1)]$Annee_Bibliographique - time_window +1) # +1 to get the very last year in the window
all_years <- first_year:1990
# Prepare our list
tbl_coup_list <- list()
for (Year in all_years) {
  nodes_of_the_year <- nodes_JEL[Annee_Bibliographique>=Year & Annee_Bibliographique<Year+time_window]
  edges_of_the_year <- edges_JEL[ID_Art %in% nodes_of_the_year$ID_Art]
  # size of nodes
  nb_cit <- edges_of_the_year[, .N, ItemID_Ref]
  colnames(nb_cit)[colnames(nb_cit) == "N"] <- "size"
  nodes_of_the_year <- merge(nodes_of_the_year, nb_cit, by.x = "ItemID_Ref", by.y = "ItemID_Ref", all.x = TRUE)
  nodes_of_the_year[is.na(size),size:=0]
  # coupling
  edges_of_the_year <- bibliographic_coupling(edges_of_the_year, "ID_Art", "New_id2", 
                                              normalized_weight_only = TRUE, weight_threshold = 3, output_in_character = TRUE)
  # remove nodes with no edges
  nodes_of_the_year <- nodes_of_the_year[ID_Art %in% edges_of_the_year$from | ID_Art %in% edges_of_the_year$to]
  nodes_of_the_year$ID_Art <- as.character(nodes_of_the_year$ID_Art)
  # make tbl
  tbl_coup_list[[as.character(Year)]] <- tbl_graph(nodes = nodes_of_the_year, edges = edges_of_the_year, directed = FALSE, node_key = "ID_Art")
  
  gc() # cleaning what is not useful anymore
}

rm(list = c("edges_JEL","nb_cit","edges_of_the_year","nodes_JEL","nodes_of_the_year"))

list_graph <- lapply(tbl_coup_list, leiden_improved)
list_graph <- lapply(list_graph, FUN = community_colors, palette = mypalette)
list_graph_position <- list()
for (Year in all_years) {
  if(is.null(list_graph[[paste0(Year-1)]])){
    list_graph_position[[paste0(Year)]] <- force_atlas(list_graph[[paste0(Year)]], kgrav = 4, change_size = FALSE)
  }
  if(!is.null(list_graph[[paste0(Year-1)]])){
    past_position <- list_graph_position[[paste0(Year-1)]] %>% activate(nodes) %>% as.data.table()
    past_position <- past_position[,.(ID_Art,x,y)]
    tbl <- list_graph[[paste0(Year)]] %>% activate(nodes) %>% left_join(past_position)
    list_graph_position[[paste0(Year)]] <- force_atlas(tbl, kgrav = 4, change_size = FALSE)
  }
  gc()
}

list_ggplot <- list()
for (Year in all_years) {
  list_ggplot[[as.character(Year)]] <- ggraph(list_graph_position[[paste0(Year)]], "manual", x = x, y = y) +
    geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.5, strength =0.2) +
    geom_node_point(aes(fill = color, size = size), pch=21) +
    scale_edge_width_continuous(range = c(0.5, 1)) +
    theme_void() +
    # geom_label_repel(data = label_com, aes(x = mean_coord_x, y = mean_coord_y, label = as.character(Leiden1), size = 4, fill = color)) +
    theme(legend.position = "none") +
    scale_fill_identity() +
    scale_edge_colour_identity() +
    labs(title = paste0(as.character(Year),"-",as.character(Year+time_window-1)))
  # ggsave("Networks/coup_2000.png", width=30, height=20, units = "cm")
}

library(gridExtra)
do.call(grid.arrange, list_ggplot)


              