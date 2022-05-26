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
require(viridis)
require(scales)
set.seed(3155210)

source("functions/functions_dynamics_networks_alex.R")
# source("functions/functions_networks_alex.R")
source("functions/functions_for_network_analysis.R")
source("functions/Script_paths_and_basic_objectsV2.R")

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


tbl_coup_list <- readRDS(here(data_path,"macro_AA","4_Networks","list_graph_position_intertemporal_naming_1969-2011.RDS"))

com_list <- fread(here(data_path,"macro_AA","4_Networks","community_list_1969-2015.csv"))
com_list[,new_Id_com:=Com_ID]
com_list[,Com_ID:=NULL]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Giving Names and Colors to Networks ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# sample
tbl_coup_list$"1999" %>% filter(new_Id_com %in% com_list$new_Id_com)
tbl_coup_list$"1999" %>% activate(nodes) %>% as.data.table()


#%%%%%%%%%%%%%%%% give names to nodes %%%%%%%%%%%%%%%%#
networks_final <- lapply(tbl_coup_list, function(tbl)
  (
    tbl <- tbl %>% 
      activate(nodes) %>% 
      left_join(com_list)
  )
)

#%%%%%%%%%%%%%%%% get all nodes of all networks to have the full list of names and make color tables %%%%%%%%%%%%%%%%#
networks_final_nodes <- lapply(networks_final, function(tbl)(tbl %>% activate(nodes) %>% as.data.table()))
networks_final_nodes <- rbindlist(networks_final_nodes, fill=TRUE)

# list meta-name
list_meta_name <- networks_final_nodes[,.N,`meta-name`]
list_sub_name <- networks_final_nodes[order(`meta-name`,time_variable),.N,.(`meta-name`,`sub-name`)]

# Primary colors are qualitative pallet (grey if NA)
list_meta_name[!is.na(`meta-name`) & `meta-name`!="Public Finance",
               primary_color:=brewer.pal(list_meta_name[!is.na(`meta-name`) & `meta-name`!="Public Finance",.N],"Set1")]
list_meta_name[is.na(`meta-name`),primary_color:="#B2B2B2"]
list_meta_name[`meta-name`=="Public Finance",primary_color:="#B9BBB6"]
list_meta_name[`meta-name`=="Keynesian Economics",primary_color:="#E41A1C"]
list_meta_name[`meta-name`=="Monetary & Financial Economics",primary_color:="#F781BF"]
list_meta_name[`meta-name`=="Growth",primary_color:="#FF7F00"]
list_meta_name[`meta-name`=="International Macroeconomics",primary_color:="#FF9287"]

# Secondary colors are viridis pallets, restarting each group
sec_color <- viridis(list_sub_name[,.N,`meta-name`][order(-N)][head(1)]$N)
# sec_color <- as.data.table(sec_color) %>% .[sample(nrow(.))] %>% .[[1]]
list_sub_name[,secondary_color:=rep(sec_color, length.out = .N),`meta-name`]

color_table <- merge(list_sub_name, list_meta_name, by="meta-name", all.x=TRUE)
color_table[, c("N.x","N.y"):=NULL]

# Grey for finance
color_table[,color_nodes:= MixColor(primary_color, secondary_color, amount1 = 0.7)]
color_table[is.na(`meta-name`),color_nodes:="#B2B2B2"]
show_col(color_table$color_nodes)

#%%%%%%%%%%%%%%%% color tbl %%%%%%%%%%%%%%%%#
networks_final <- lapply(networks_final, function(tbl)
  (
    tbl <- tbl %>% 
      activate(nodes) %>% 
      left_join(color_table)
  )
)


color_edges <- function(tbl=tbl, color_col="color_nodes")
{
  print(tbl %>% activate(nodes) %>% as.data.table() %>% .[,min(Annee_Bibliographique)])
  #Mix color for edges of different color
  tbl <- tbl %>% #mix color
    activate(edges) %>%
    mutate(com_ID_to = .N()[[color_col]][to], com_ID_from = .N()[[color_col]][from]) %>%
    mutate(color_edges = MixColor(com_ID_to, com_ID_from, amount1 = 0.6))  # .N() makes the node data available while manipulating edges
  
  return (tbl)
}

networks_final <- lapply(networks_final, color_edges, color_col="color_nodes")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Into Alluvial ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

networks_final <- lapply(networks_final, function(tbl)
  (
    tbl <- tbl %>% 
      activate(nodes) %>% 
      mutate(Id=ID_Art)
  )
)
alluv_dt <- make_into_alluv_dt(networks_final)
alluv_dt[,Id:=ID_Art]
alluv_dt <- minimize_crossing(alluv_dt)
alluv_dt_graph <- alluv_dt %>% left_join(com_list) %>% left_join(color_table)

label <- copy(alluv_dt_graph)
label <- label[,Window:=round(mean(as.numeric(Window))),`sub-name`][, head(.SD, 1), .(`sub-name`)]
label[,Label:=`sub-name`]
alluv_dt_graph<-merge(alluv_dt_graph,label[,.(new_Id_com,Window,Label)], by = c("new_Id_com","Window"), all.x = TRUE) 
alluv_dt_graph$new_Id_com <- fct_reorder(alluv_dt_graph$new_Id_com, alluv_dt_graph$order,min, .desc = TRUE)


plot_alluvial <- ggplot(alluv_dt_graph, aes(x = Window, y=share, stratum = new_Id_com, alluvium = ID_Art, fill = color_nodes)) +
  geom_stratum(alpha =1, size=1/10) +
  geom_flow() +
  theme(legend.position = "none") +
  theme_minimal() +
  scale_fill_identity() +
  ggtitle("") +
  ggrepel::geom_label_repel(stat = "stratum", size = 5, aes(label = Label)) 
ggsave(here(picture_path, "alluvial_colored_named.png"), plot = plot_alluvial, width = 60, height = 50, units = "cm")

# label <- copy(alluv_dt_graph)
# label <- label[,Window:=round(mean(as.numeric(Window))),`meta-name`][, head(.SD, 1), .(`new_Id_com`)]
# label[,Label:=`meta-name`]
# alluv_dt_graph[,Label:=NULL]
# alluv_dt_graph<-merge(alluv_dt_graph,label[,.(new_Id_com,Window,Label)], by = c("new_Id_com","Window"), all.x = TRUE) 
# alluv_dt_graph$new_Id_com <- fct_reorder(alluv_dt_graph$new_Id_com, alluv_dt_graph$order,min, .desc = TRUE)
# 
# 
# plot_alluvial <- ggplot(alluv_dt_graph, aes(x = Window, y=share, stratum = new_Id_com, alluvium = ID_Art, fill = color_nodes)) +
#   geom_stratum(alpha =1, size=1/10) +
#   geom_flow() +
#   theme(legend.position = "none") +
#   theme_minimal() +
#   scale_fill_identity() +
#   ggtitle("") +
#   ggrepel::geom_label_repel(stat = "stratum", size = 5, aes(label = Label)) 
# ggsave(here(picture_path, "alluvial_colored_named_meta.png"), plot = plot_alluvial, width = 60, height = 50, units = "cm")
# 
# 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Sample ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

networks_final_example <- layout_fa2_java(networks_final[[paste0(Year)]],
                                          niter = 16000,
                                          threads = 16)

ggraph(networks_final_example, "manual", x = x, y = y) +
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.5, strength =0.2) +
  geom_node_point(aes(fill = color_nodes, size = size), pch=21) +
  scale_edge_width_continuous(range = c(0.5, 1)) +
  theme_void() +
  # ggrepel::geom_label_repel(data = label_com, aes(x = mean_coord_x, y = mean_coord_y, label = as.character(`sub-name`), fill = color)) +
  theme(legend.position = "none") +
  scale_fill_identity() +
  scale_edge_colour_identity() +
  labs(title = paste0(as.character(Year),"-",as.character(Year+time_window-1)))
ggsave(here(picture_path, "network_example_16k.png"), width = 60, height = 50, units = "cm")

networks_final_example <- layout_fa2_java(networks_final[[paste0(Year)]],
                                          niter = 25000,
                                          threads = 16)

ggraph(networks_final_example, "manual", x = x, y = y) +
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.5, strength =0.2) +
  geom_node_point(aes(fill = color_nodes, size = size), pch=21) +
  scale_edge_width_continuous(range = c(0.5, 1)) +
  theme_void() +
  # ggrepel::geom_label_repel(data = label_com, aes(x = mean_coord_x, y = mean_coord_y, label = as.character(`sub-name`), fill = color)) +
  theme(legend.position = "none") +
  scale_fill_identity() +
  scale_edge_colour_identity() +
  labs(title = paste0(as.character(Year),"-",as.character(Year+time_window-1)))
ggsave(here(picture_path, "network_example_25k.png"), width = 60, height = 50, units = "cm")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FA2 ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


list_graph_position <- list()
for (Year in all_years) {
  message(paste0("Running Force Atlas for the ", Year, "-", Year + time_window - 1, " window."))
  if(is.null(networks_final[[paste0(Year-1)]])){
    list_graph_position[[paste0(Year)]] <- layout_fa2_java(networks_final[[paste0(Year)]],
                                                           niter = 25000,
                                                           threads = 16)
  }
  if(!is.null(networks_final[[paste0(Year-1)]])){
    past_position <- list_graph_position[[paste0(Year-1)]] %>% activate(nodes) %>% as.data.table()
    past_position <- past_position[,.(ID_Art,x,y)]
    
    tbl <- networks_final[[paste0(Year)]] %>% 
      activate(nodes) %>% 
      left_join(past_position)
    
    list_graph_position[[paste0(Year)]] <- layout_fa2_java(tbl,
                                                           niter = 25000,
                                                           threads = 16)
    print(Year)
  }
}

saveRDS(list_graph_position, here(graph_data_path, paste0("list_graph_position", first_year, "-", last_year + time_window - 1, ".rds")))
list_graph_position <- readRDS(here(graph_data_path, paste0("list_graph_position", first_year, "-", last_year + time_window - 1, ".rds")))

graph_90 <- list_graph_position[["1990"]]
plot_90 <- ggraph(graph_90, "manual", x = x, y = y) +
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.5, strength =0.2) +
  geom_node_point(aes(fill = color_nodes, size = size), pch=21) +
  scale_edge_width_continuous(range = c(0.5, 1)) +
  theme_void() +
  # ggrepel::geom_label_repel(data = label_com, aes(x = mean_coord_x, y = mean_coord_y, label = as.character(`sub-name`), fill = color)) +
  theme(legend.position = "none") +
  scale_fill_identity() +
  scale_edge_colour_identity()
ggsave(here(picture_path, "FA2_90.png"), plot_90, device = ragg::agg_png, width = 60, height = 50, units = "cm") 

graph_00 <- list_graph_position[["2000"]] %>% activate(edges) %>% filter(weight>0.05)
plot_00 <- ggraph(graph_00, "manual", x = x, y = y) +
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.5, strength =0.2) +
  geom_node_point(aes(fill = color_nodes, size = size), pch=21) +
  scale_edge_width_continuous(range = c(0.5, 1)) +
  theme_void() +
  # ggrepel::geom_label_repel(data = label_com, aes(x = mean_coord_x, y = mean_coord_y, label = as.character(`sub-name`), fill = color)) +
  theme(legend.position = "none") +
  scale_fill_identity() +
  scale_edge_colour_identity()
ggsave(here(picture_path, "FA2_00.png"), plot_00, device = ragg::agg_png, width = 60, height = 50, units = "cm") 

graph_10 <- list_graph_position[["2010"]] %>% activate(edges) %>% filter(weight>0.05)
plot_10 <- ggraph(graph_10, "manual", x = x, y = y) +
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.5, strength =0.2) +
  geom_node_point(aes(fill = color_nodes, size = size), pch=21) +
  scale_edge_width_continuous(range = c(0.5, 1)) +
  theme_void() +
  # ggrepel::geom_label_repel(data = label_com, aes(x = mean_coord_x, y = mean_coord_y, label = as.character(`sub-name`), fill = color)) +
  theme(legend.position = "none") +
  scale_fill_identity() +
  scale_edge_colour_identity()
ggsave(here(picture_path, "FA2_10.png"), plot_10, device = ragg::agg_png, width = 60, height = 50, units = "cm") 


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### SAVING ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
list_graph_position <- readRDS(here(graph_data_path, paste0("list_graph_position", first_year, "-", last_year + time_window - 1, ".rds")))
# Rescaling coordinates and size
list_graph_position <- lapply(list_graph_position, 
                              function(tbl){tbl %>% 
                                  activate(nodes) %>% 
                                  mutate(original_x = x,
                                         original_y = y,
                                         x = rescale(x, c(0, 1200)),
                                         y = rescale(y, c(0, 1000)),
                                         size = rescale(nb_cit, c(1,100)))})

# Nodes
nodes_lf <- lapply(list_graph_position, function(tbl) (tbl %>% activate(nodes) 
                                                  %>% rename(sub_name = "sub-name") 
                                                  %>% rename(meta_name = "meta-name") 
                                                  %>% as.data.table()))
nodes_lf <- lapply(nodes_lf, function(dt) (dt[, .(ID_Art, x, y, original_x, original_y, size, nb_cit, Com_ID=new_Id_com, color=color_nodes, meta_name, sub_name, Size_com)]))
nodes_lf <- rbindlist(nodes_lf, idcol = "window")
nodes_lf <- nodes_lf[, window := paste0(window, "-", as.integer(window) + time_window-1)][order(ID_Art, window)]
# Authors Table
authors <- readRDS(here(data_path, "macro_AA","OST_generic_data", "all_aut.RDS")) %>% .[ID_Art %in% nodes_lf$ID_Art]
authors[,ID_Art:=as.character(ID_Art)]
# keep one letter after name
authors <- authors[, name_short := str_replace_all(Nom,"\\.", "-")] # replace . by -
authors <- authors[, name_short := str_replace_all(name_short,"(?<=\\-.).*", "")]
authors$name_short <- toupper(authors$name_short)
authors <- authors[, Nom := name_short]
authors[, c("name_short") := NULL]
# Authors in one cell
nodes_aut <- copy(authors)
nodes_aut[,Ordre := paste0("aut_",Ordre)]
nodes_aut <- dcast(nodes_aut, ID_Art ~ Ordre, value.var = "Nom")
nodes_aut[is.na(nodes_aut)] <- ""
column_to_paste <- c(paste0(str_subset(ls(nodes_aut),"[:digit:]")))
nodes_aut <- setDT(nodes_aut)[,c("Authors") := do.call(paste, c(.SD, sep = " ")), .SDcols = column_to_paste] # apply paste on all .SD cols
nodes_aut <- nodes_aut[,.SD,.SDcols = c("ID_Art","Authors")]
nodes_aut <- nodes_aut[, c("Authors") := str_squish(get("Authors"))][, c("Authors") := str_replace_all(get("Authors")," ",", ")]
# Nodes table final
nodes_lf <-merge(nodes_lf, nodes_aut, by="ID_Art",all.x = TRUE)

edges_lf <- lapply(list_graph_position, function(tbl) (tbl %>% activate(edges) %>% as.data.table()))
edges_lf <- lapply(edges_lf, function(dt) (dt[, .(Source, Target, weight, color_edges)]))
edges_lf <- rbindlist(edges_lf, idcol = "window")
edges_lf <- edges_lf[, window := paste0(window, "-", as.integer(window) + time_window-1)]

nodes_info <- lapply(list_graph_position, function(tbl) (tbl %>% activate(nodes) %>% as.data.table()))
nodes_info <- rbindlist(nodes_info,fill=TRUE)
nodes_info <- nodes_info[, head(.SD, 1), .(ID_Art)]
nodes_info <- nodes_info[, .(ID_Art, Titre, Annee_Bibliographique, Label, Revue, ESpecialite)]

write_csv(nodes_lf, here(data_path,"macro_AA","5_platform_data","nodes_lf.csv"))
write_csv(edges_lf, here(data_path,"macro_AA","5_platform_data","edges_lf.csv"))
write_csv(nodes_info, here(data_path,"macro_AA","5_platform_data","nodes_info.csv"))




