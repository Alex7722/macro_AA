#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################## PART I: LOADING PACKAGES, PATH AND DATA ####################################--------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

##################### Packages ############################################--------------

package_list <-  c("data.table","magrittr","tidyverse", "ggnewscale", "igraph",
                   "tm","tidytext","ggraph","tidygraph", "ggrepel", "vite",
                   "reticulate","leiden","reshape2","scales","scico",
                   "ggforce","directlabels","patchwork","DescTools")
for(p in package_list){
  if (p %in% installed.packages()==FALSE){install.packages(p,dependencies = TRUE)}
  library(p,character.only=TRUE)
}

#py_install("python-igraph")
#py_install("leidenalg", forge = TRUE)

######################### Paths and data ##########################################------------

data_path <- "/projects/data/macro_AA/Corpus_Econlit_Matched_WoS/"
graph_data_path <- "/projects/data/macro_AA/Graphs/"
picture_path <- "/home/aurelien/macro_AA/Static_Network_Analysis/"

source("/home/aurelien/macro_AA/Static_Network_Analysis/functions_for_network_analysis.R")

mypalette <- c("#1969B3","#01A5D8","#DA3E61","#3CB95F","#E0AF0C","#E25920","#6C7FC9","#DE9493","#CD242E","#6F4288","#B2EEF8","#7FF6FD","#FDB8D6","#8BF9A9","#FEF34A","#FEC57D","#DAEFFB","#FEE3E1","#FBB2A7","#EFD7F2","#5CAADA","#37D4F5","#F5779B","#62E186","#FBDA28","#FB8F4A","#A4B9EA","#FAC2C0","#EB6466","#AD87BC","#0B3074","#00517C","#871B2A","#1A6029","#7C4B05","#8A260E","#2E3679","#793F3F","#840F14","#401C56","#003C65","#741A09","#602A2A","#34134A","#114A1B","#27DDD1","#27DD8D","#4ADD27","#D3DD27","#DDA427","#DF2935","#DD27BC","#BA27DD","#3227DD","#2761DD","#27DDD1")

# Loading files
nodes_JEL <- readRDS(paste0(data_path,"JEL_matched_corpus_nodes.rds"))
edges_JEL <- readRDS(paste0(data_path,"JEL_matched_corpus_edges.rds"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################ PART II: NETWORK ANALYSIS #################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################ 1) Building Nodes and Edges #################################-------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

############################### Building Nodes and Edges for co-citation #######################
nodes_cocit_JEL_1990s <- edges_JEL[ID_Art %in% nodes_JEL[between(Annee_Bibliographique,1991,1999),]$ID_Art,]
nodes_cocit_JEL_1990s <- merge(nodes_cocit_JEL_1990s, nodes_JEL[,c("ItemID_Ref","Titre","Code_Revue")], by = "ItemID_Ref", all.x = TRUE)
nodes_cocit_JEL_1990s <- unique(nodes_cocit_JEL_1990s[,nb_cit := .N, by = "New_id2"][nb_cit >= 3,c("New_id2","Annee","Nom","Titre","nb_cit","Code_Revue")])
doublons <- which(duplicated(nodes_cocit_JEL_1990s$New_id2))
nodes_cocit_JEL_1990s <- nodes_cocit_JEL_1990s[-doublons]

edges_JEL_1990s <- edges_JEL[New_id2 %in% nodes_cocit_JEL_1990s$New_id2 
                             & ID_Art %in% nodes_JEL[between(Annee_Bibliographique,1991,1999),]$ID_Art,][, c("ID_Art","New_id2")]

# creation of the edges for the co-citation network

edges_cocit_JEL_1990s <- bibliographic_cocitation(edges_JEL_1990s, source = "ID_Art", ref = "New_id2",weight_threshold = 2)

# creating the tidygraph object  
nodes_cocit_JEL_1990s <- nodes_cocit_JEL_1990s[, New_id2:= as.character(New_id2)]
graph_cocit <- tbl_main_components(edges = edges_cocit_JEL_1990s, nodes = nodes_cocit_JEL_1990s, node_key = "New_id2", threshold_alert = 0.05, directed = FALSE)

# Optionnal: studying the distribution of nodes in each component of the graph
#components <- components_distribution(edges = edges_cocit_JEL_1990s, nodes = nodes_cocit_JEL_1990s, node_key = "New_id2")

############################### Building Nodes and Edges for coupling #######################-----------

# creating the nodes with the number of citation of the nodes in the corpus
nodes_coupling_JEL_1990s <- nodes_JEL[between(Annee_Bibliographique,1991,1999),]
nb_cit<- edges_JEL[,nb_cit := .N,ItemID_Ref]
nodes_coupling_JEL_1990s <- merge(nodes_coupling_JEL_1990s,nb_cit[,c("ItemID_Ref","nb_cit")], by = "ItemID_Ref", all.x = "TRUE")

rm(nb_cit) # not useful anymore
# replacing NA value by 0 in nb_cit (it means that the nodes will be used in Leiden and Force Atlas, but
# not displayed in the graph)

nodes_coupling_JEL_1990s[is.na(nb_cit),]$nb_cit <- 0

# reducing the number of nodes depending of the number of citations
nodes_coupling_JEL_1990s <- unique(nodes_coupling_JEL_1990s[nb_cit >= 2,c("ID_Art","Annee_Bibliographique","Nom","Label","Titre","nb_cit","Code_Revue","ItemID_Ref")])

# building edges
edges_JEL_1990s_bis <- edges_JEL[ID_Art %in% nodes_coupling_JEL_1990s$ID_Art]

# creation of the edges for the co-citation network

edges_coupling_JEL_1990s <- bibliographic_coupling(edges_JEL_1990s_bis, source = "ID_Art", ref = "New_id2", weight_threshold = 2)

# creating the tidygraph object  
nodes_coupling_JEL_1990s <- nodes_coupling_JEL_1990s[, ID_Art:= as.character(ID_Art)]
graph_coupling <- tbl_main_components(edges = edges_coupling_JEL_1990s, nodes = nodes_coupling_JEL_1990s, node_key = "ID_Art", threshold_alert = 0.05, directed = FALSE)

# Optionnal: studying the distribution of nodes in each component of the graph
#components <- components_distribution(edges = edges_coupling_JEL_1990s, nodes = nodes_coupling_JEL_1990s, node_key = "ID_Art")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################ 2) Bibliographic Co-citation #################################-------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

######################### Working on the tidygraph for cocitation and its attributes #################################-------

# Identifying communities with Leiden algorithm                         
graph_cocit <- leiden_improved(graph_cocit, res_1 = 1, res_2 = NULL, res_3 = NULL, n_iterations = 500)       

# Giving colors to communities
graph_cocit <- community_colors(graph_cocit,mypalette)

# Calculating different centrality measures
graph_cocit <- centrality(graph_cocit)

# Integration a size variable for implementing non-overlapping function of Force Atlast
graph_cocit <- graph_cocit %>%
  activate(nodes) %>%
  mutate(size=nb_cit)

# Running Force Atlas layout  
graph_cocit <- force_atlas(graph_cocit,seed = NULL, ew.influence = 1, kgrav = 1, iter_1 = 6000, iter_2 = 2000, barnes.hut = TRUE, size_min = 50, size_max = 200)

# Saving the graph
saveRDS(graph_cocit, paste0(graph_data_path,"graph_cocit.rds"))

############################# Projection of the graph #########################

# loading the graph if necessary
#graph_cocit <- readRDS(paste0(graph_data_path,"graph_cocit.rds"))

# Identifying the labels of the most important authors
important_nodes <- top_citations(graph_cocit)

# Plotting the graph  
ggraph(graph_cocit, "manual", x = x, y = y) + 
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,2)) +
  geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.8, show.legend = FALSE) +
  scale_size_continuous(range = c(0.1,10)) +
  scale_fill_identity() +
  scale_edge_colour_identity() +
  geom_label_repel(data=important_nodes, aes(x=x, y=y, label = paste0(Nom,Annee), fill = color), size = 4, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  #scale_size_continuous(range = c(1,4)) +
  theme_void() +
  ggsave(paste0(picture_path,"graph_cocit.png"), width=30, height=30, units = "cm")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################# 2) Bibliographic Coupling ##################################------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

################## Working on the coupling tidygraph and its attributes ######################----

# Identifying communities with Leiden algorithm                         
graph_coupling <- leiden_improved(graph_coupling, res_1 = 1, res_2 = NULL, res_3 = NULL, n_iterations = 500)       

# Giving colors to communities
graph_coupling <- community_colors(graph_coupling,mypalette)

# Calculating different centrality measures
graph_coupling <- centrality(graph_coupling)

# Integration a size variable for implementing non-overlapping function of Force Atlast
graph_coupling <- graph_coupling %>%
  activate(nodes) %>%
  mutate(size=nb_cit)

# Running Force Atlas layout  
graph_coupling <- force_atlas(graph_coupling,seed = NULL, ew.influence = 1, kgrav = 1, iter_1 = 4000, iter_2 = 1000, barnes.hut = TRUE, size_min = 50, size_max = 200)

# Saving the graph
saveRDS(graph_coupling, paste0(graph_data_path,"graph_coupling.rds"))

############################# Projection of the graph #########################

# loading the graph if necessary
graph_coupling <- readRDS(paste0(graph_data_path,"graph_coupling.rds"))

# Identifying the labels of the most important authors
important_nodes <- top_citations(graph_coupling)

# Plotting the graph  
ggraph(graph_coupling, "manual", x = x, y = y) + 
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,2)) +
  scale_edge_colour_identity() +
  geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.8, show.legend = FALSE) +
  scale_size_continuous(range = c(0.1,10)) +
  scale_fill_identity() +
  geom_label_repel(data=important_nodes, aes(x=x, y=y, label = paste0(Nom,Annee_Bibliographique), fill = color), size = 4, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  #scale_size_continuous(range = c(1,4)) +
  theme_void() +
  ggsave(paste0(picture_path,"graph_coupling.png"), width=30, height=30, units = "cm")

##################################### Basic Statistics of Communities ###############################---

references_extended <- graph_coupling %>%
  activate(nodes)%>%
  as.data.table()
references_extended$ID_Art <- as.integer(references_extended$Id)

references_extended <- merge(references_extended,edges_JEL, by = "ID_Art")
