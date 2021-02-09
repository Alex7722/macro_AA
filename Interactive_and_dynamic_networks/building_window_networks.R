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
all_years <- 1970:1980
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
  tbl_coup_list[[as.character(Year)]] <- networkflow::tbl_main_component(nodes = nodes_of_the_year, edges = edges_of_the_year, directed = FALSE, node_key = "ID_Art", nb_components = 2)
  
  gc() # cleaning what is not useful anymore
}

rm(list = c("edges_JEL","nb_cit","edges_of_the_year","nodes_JEL","nodes_of_the_year"))

# finding communities 
list_graph <- lapply(tbl_coup_list, leiden_improved)
# list_graph <- lapply(list_graph, FUN = community_colors, palette = mypalette)

################ Running force atlas ##################

list_graph_position <- list()

for (Year in all_years) {
  if(is.null(list_graph[[paste0(Year-1)]])){
    list_graph_position[[paste0(Year)]] <- force_atlas(list_graph[[paste0(Year)]], kgrav = 4, change_size = FALSE)
  }
  if(!is.null(list_graph[[paste0(Year-1)]])){
    past_position <- list_graph_position[[paste0(Year-1)]] %>% activate(nodes) %>% as.data.table()
    past_position <- past_position[,.(Id,x,y)]
    tbl <- list_graph[[paste0(Year)]] %>% activate(nodes) %>% left_join(past_position)
    list_graph_position[[paste0(Year)]] <- force_atlas(tbl, kgrav = 4, change_size = FALSE)
  }
  gc()
}

################################## Integrating Community names (temporary) ############################## 
# Listing all the graph computed in `static_network_analysis.R`
all_nodes <- data.table("Id" = c(), "Annee_Bibliographique" = c(), "Titre" = c(), "Label" = c(), "color" = c(), "Community_name" = c())
for(i in 1:length(start_date)){
  graph <- readRDS(paste0(graph_data_path,"graph_coupling_",start_date[i],"-",end_date[i],".rds"))
  graph <- graph %>% 
    activate(nodes) %>%
    select(Id, Annee_Bibliographique, Titre, Label, color, Community_name) %>%
    as.data.table()
  all_nodes <- rbind(all_nodes, graph)
}

for (Year in all_years) {
  nodes <- list_graph_position[[paste0(Year)]] %>% 
    activate(nodes) %>%
    select(ID_Art, Annee_Bibliographique, Titre, Label, Com_ID) %>%
    as.data.table()
  communities <- all_nodes[between(Annee_Bibliographique,Year, Year + 4)]
  
  communities <- merge(nodes,communities[, c("Id","color","Community_name")], by.x = "ID_Art", by.y = "Id")
  communities <- communities[, size_com := .N, by = "Com_ID"][, .N, by = c("Com_ID","size_com","Community_name","color")]
  
  communities <- communities %>% 
    group_by(Com_ID)  %>% 
    arrange(-N) %>%
    mutate(share = N/size_com) %>%
    select(Com_ID,Community_name,color,share) %>%
    slice(1)
  
  list_graph_position[[paste0(Year)]] <- list_graph_position[[paste0(Year)]] %>%
    activate(nodes) %>%
    left_join(communities)
    
    # Mix color for edges of different color
    list_graph_position[[paste0(Year)]] <- list_graph_position[[paste0(Year)]] %>%
    activate(edges) %>%
    mutate(color_com_ID_to = .N()$color[to], color_com_ID_from = .N()$color[from]) %>%
    mutate(color_edges = DescTools::MixColor(color_com_ID_to, color_com_ID_from, amount1 = 0.5))
  
}

saveRDS(list_graph_position, paste0(graph_data_path,"list_graph.rds"))

################# Projecting graphs #################

list_graph_position <- readRDS(paste0(graph_data_path,"list_graph.rds"))

com_label <- list()

for (Year in all_years) {
com_label[[paste0(Year)]] <- label_com(list_graph_position[[paste0(Year)]],biggest_community = TRUE, community_threshold = 0.01)
}

list_ggplot <- list()
for (Year in all_years) {
  list_ggplot[[as.character(Year)]] <- ggraph(list_graph_position[[paste0(Year)]], "manual", x = x, y = y) +
    geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.5, strength =0.2) +
    geom_node_point(aes(fill = color, size = size), pch=21) +
    scale_edge_width_continuous(range = c(0.5, 1)) +
    theme_void() +
    geom_label_repel(data=com_label[[paste0(Year)]], aes(x=x, y=y, label = Community_name, fill = color, size = Size_com*2), fontface="bold", alpha = 0.9, point.padding=NA, show.legend = FALSE) +
    theme(legend.position = "none") +
    scale_fill_identity() +
    scale_edge_colour_identity() +
    labs(title = paste0(as.character(Year),"-",as.character(Year+time_window-1)))
  # ggsave("Networks/coup_2000.png", width=30, height=20, units = "cm")
}

do.call(grid.arrange, list_ggplot)


              