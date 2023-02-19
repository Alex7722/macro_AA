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
source("functions/functions_for_network_analysis.R")
source("dynamic_networks/Script_paths_and_basic_objects.R")
#source("functions/functions_dynamics_networks_alex.R")
#source("functions/functions_networks_alex.R")

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
tbl_coup_list <- dynamic_biblio_coupling(corpus = nodes_JEL[between(Annee_Bibliographique, 1969, 2015)], 
                                    direct_citation_dt = edges_JEL, 
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


# cleaning now useless objects
gc()
rm(list = c("edges_JEL", "nodes_JEL","tbl_coup_list_bis"))

#' ## Finding Communities

#' We use the leiden_workflow function of the networkflow package (it uses the 
#' leidenAlg package). 

#+ r communities
tbl_coup_list <- lapply(tbl_coup_list, leiden_workflow)
# list_graph <- lapply(list_graph, FUN = community_colors, palette = mypalette)

#+ r saving_com, include = FALSE
# intermediary saving
saveRDS(tbl_coup_list, paste0(graph_data_path, "list_graph_", names(tbl_coup_list[1]), "-", 
                              as.integer(names(tbl_coup_list[length(tbl_coup_list)])), ".rds"))

#' ## Running force atlas 

tbl_coup_list <- readRDS(paste0(graph_data_path, "list_graph_", first_year, "-", last_year, ".rds"))
tbl_coup_list <- lapply(tbl_coup_list, 
                        function(tbl){tbl %>% 
                            activate(nodes) %>% 
                            mutate(size = nb_cit,
                                   ID_Art = as.character(ID_Art))})

list_graph_position <- list()

for (Year in all_years) {
  message(paste0("Running Force Atlas for the ", Year, "-", Year + time_window - 1, " window."))
  if(is.null(tbl_coup_list[[paste0(Year-1)]])){
    list_graph_position[[paste0(Year)]] <- layout_fa2_java(tbl_coup_list[[paste0(Year)]],
                                                           niter = 4000,
                                                           threads = 8)
  }
  if(!is.null(tbl_coup_list[[paste0(Year-1)]])){
    past_position <- list_graph_position[[paste0(Year-1)]] %>% activate(nodes) %>% as.data.table()
    past_position <- past_position[,.(ID_Art,x,y)]
    
    tbl <- tbl_coup_list[[paste0(Year)]] %>% 
      activate(nodes) %>% 
      left_join(past_position)
    
    list_graph_position[[paste0(Year)]] <- layout_fa2_java(tbl,
                                                           niter = 4000,
                                                           threads = 8)
    print(Year)
  }
}

saveRDS(list_graph_position, paste0(graph_data_path, "list_graph_position", first_year, "-", last_year + time_window - 1, ".rds"))


agg_png(paste0(picture_path, "coupling_graph_plot_test.png"),
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
list_graph_position <- readRDS(paste0(graph_data_path, "list_graph_", first_year, "-", last_year + time_window - 1, ".rds"))

list_graph_position <- lapply(list_graph_position, 
                        function(tbl){tbl %>% 
                            activate(nodes) %>% 
                            mutate(original_x = x,
                                   original_y = y,
                                   x = rescale(x, c(0, 1200)),
                                   y = rescale(y, c(0, 1000)),
                                   size = rescale(nb_cit, c(1,100)))})

saveRDS(list_graph_position, paste0(graph_data_path, "list_graph_position", first_year, "-", last_year + time_window - 1, ".rds"))


#' ## Integrating Community names (temporary)

#+ r names
# Listing all the graph computed in `static_network_analysis.R`
all_nodes <- data.table("Id" = c(), "Annee_Bibliographique" = c(), "Titre" = c(), "Label" = c(), "color" = c(), "Community_name" = c())
for (i in 1:length(start_date)) {
  graph <- readRDS(paste0(graph_data_path, "graph_coupling_", start_date[i], "-", end_date[i], ".rds"))
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
  communities <- all_nodes[between(Annee_Bibliographique, Year, Year + 4)]

  communities <- merge(nodes, communities[, c("Id", "color", "Community_name")], by.x = "ID_Art", by.y = "Id")
  communities <- communities[, size_com := .N, by = "Com_ID"][, .N, by = c("Com_ID", "size_com", "Community_name", "color")]

  communities <- communities %>%
    group_by(Com_ID) %>%
    arrange(-N) %>%
    mutate(share = N / size_com) %>%
    select(Com_ID, Community_name, color, share) %>%
    slice(1)

  list_graph_position[[paste0(Year)]] <- list_graph_position[[paste0(Year)]] %>%
    activate(nodes) %>%
    left_join(communities)

  # Mix color for edges of different color
  list_graph_position[[paste0(Year)]] <- list_graph_position[[paste0(Year)]] %>%
    activate(edges) %>%
    mutate(color_com_ID_to = .N()$color[to], color_com_ID_from = .N()$color[from]) %>%
    mutate(color_edges = DescTools::MixColor(color_com_ID_to, color_com_ID_from, amount1 = 0.5))

  # Cleaning progressively
  gc()
}

saveRDS(list_graph_position, paste0(graph_data_path, "list_graph_", first_year, "-", last_year + time_window - 1, ".rds"))


#' ## Projecting graphs

#' This step is not necessary for producting the data for the online platform.
#' If you want to run this part, set `run` to "TRUE".

#+ r graph
run = FALSE

if(run == TRUE){
list_graph_position <- readRDS(paste0(graph_data_path, "list_graph_", first_year, "-", last_year + time_window - 1, ".rds"))

com_label <- list()

for (Year in all_years) {
  com_label[[paste0(Year)]] <- label_com(list_graph_position[[paste0(Year)]], biggest_community = TRUE, community_threshold = 0.01)
}

list_ggplot <- list()
for (Year in all_years) {
  list_ggplot[[as.character(Year)]] <- ggraph(list_graph_position[[paste0(Year)]], "manual", x = x, y = y) +
    geom_edge_link0(aes(color = color_edges, width = weight), alpha = 0.5) +
    geom_node_point(aes(fill = color, size = size), pch = 21) +
    scale_edge_width_continuous(range = c(0.1, 0.5)) +
    scale_size_continuous(range = c(0.1, 3)) +
    theme_void() +
    # new_scale("size") +
    geom_label_repel(data = com_label[[paste0(Year)]], aes(x = x, y = y, label = Community_name, fill = color), size = 0.5, fontface = "bold", alpha = 0.9, point.padding = NA, show.legend = FALSE) +
    theme(legend.position = "none") +
    scale_fill_identity() +
    scale_edge_colour_identity() +
    labs(title = paste0(as.character(Year), "-", as.character(Year + time_window - 1)))
  # ggsave("Networks/coup_2000.png", width=30, height=20, units = "cm")
}


benchmark <- do.call(grid.arrange, list_ggplot[c(1, 5, 9, 13, 17, 22)])
ggsave(paste0(picture_path, "benchmark_edge_threshold_3.png"), benchmark, width = 30, height = 30, unit = "cm")

ggsave(plot = g, paste0("Graphs/", author, "_networks.png"), width = 30, height = 40, units = "cm")

library(ggpubr)
for (i in c(1, 9, 18, 27, 35)) {
  g <- ggarrange(plotlist = list_ggplot[i:(i + 7)], common.legend = TRUE, legend = "none")
  ggsave(plot = g, paste0(picture_path, "Graph_", i), width = 30, height = 40, units = "cm")
  gc()
}
}

#' # Extracting networks data for the platform
#'
#' ## Transforming in long format 

#+ r platform
# loading the data
list_graph_position <- readRDS(paste0(graph_data_path, "list_graph_", first_year, "-", last_year + time_window - 1, ".rds"))

# creating a table with the data for nodes and edges for each window

nodes_lf <- lapply(list_graph_position, function(tbl) (tbl %>% activate(nodes) %>% as.data.table()))
nodes_lf <- lapply(nodes_lf, function(dt) (dt[, .(ID_Art, x, y, size, Com_ID, color)]))
nodes_lf <- rbindlist(nodes_lf, idcol = "window")
nodes_lf <- nodes_lf[, window := paste0(window, "-", as.integer(window) + 4)][order(ID_Art, window)]

edges_lf <- lapply(list_graph_position, function(tbl) (tbl %>% activate(edges) %>% as.data.table()))
edges_lf <- lapply(edges_lf, function(dt) (dt[, .(Source, Target, weight, Com_ID, color_edges)]))
edges_lf <- rbindlist(edges_lf, idcol = "window")
edges_lf <- edges_lf[, window := paste0(window, "-", as.integer(window) + 4)]

nodes_info <- nodes_JEL[ID_Art %in% unique(nodes_lf$ID_Art)][, c("ID_Art", "Titre", "Annee_Bibliographique", "Nom", "Label", "Revue", "ESpecialite")]

write_csv(nodes_lf, paste0(platform_data, "nodes_lf.csv"))
write_csv(edges_lf, paste0(platform_data, "edges_lf.csv"))
write_csv(nodes_info, paste0(platform_data, "nodes_info.csv"))
