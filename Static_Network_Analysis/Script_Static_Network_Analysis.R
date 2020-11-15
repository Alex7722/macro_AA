#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################## PART I: LOADING PACKAGES, PATH AND DATA ####################################--------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

##################### Packages ############################################--------------

package_list <-  c("data.table","readr","magrittr","tidyverse", "ggnewscale", "igraph",
                   "tm","quanteda","tidytext","ggraph","tidygraph", "ggrepel", "vite",
                   "reticulate","leiden","reshape2","scales","scico",
                   "ggforce","directlabels","patchwork","DescTools","grid","ggdendro")
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
authors_JEL <- readRDS(paste0(data_path,"JEL_matched_corpus_authors.rds"))
ref_info_JEL <- readRDS(paste0(data_path,"JEL_matched_corpus_references_info.rds"))
institutions_info_JEL <- fread(paste0(data_path,"JEL_matched_corpus_institutions.csv"), quote="") %>% data.table()

# Adding info to references
edges_JEL <- merge(unique(edges_JEL),unique(ref_info_JEL[,c("New_id2","Titre","ESpecialite")]), by = "New_id2", all.x = TRUE)
edges_JEL <- merge(edges_JEL,unique(nodes_JEL[,c("ID_Art","Annee_Bibliographique")]), by = "ID_Art", all.x = TRUE)

# Adding institutions to Articles
institutions_info_JEL$ID_Art <- as.integer(institutions_info_JEL$ID_Art)
institutions_info_JEL$Ordre <- as.integer(institutions_info_JEL$Ordre)
authors_JEL <- merge(authors_JEL,institutions_info_JEL, by = c("ID_Art","Ordre"), all.x = TRUE)

# removing useless files

rm(ref_info_JEL)
rm(institutions_info_JEL)

# passing name column to upper letters
edges_JEL <- edges_JEL[, Nom := toupper(Nom)]
authors_JEL <- authors_JEL[, Nom := toupper(Nom)]
nodes_JEL <- nodes_JEL[, Nom := toupper(Nom)]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################ PART II: NETWORK ANALYSIS #################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################ 1) Building Nodes and Edges #################################-------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

############################### Building Nodes and Edges for co-citation #######################
# fixing threshold
nodes_cocit_threshold = 5
edges_cocit_threshold = 2
graph_filtering_cocit = 5

# creating nodes
nodes_cocit_JEL_1990s <- edges_JEL[between(Annee_Bibliographique,1991,1999),]
#nodes_cocit_JEL_1990s <- merge(nodes_cocit_JEL_1990s, nodes_JEL[,c("ItemID_Ref","Titre","Code_Revue")], by = "ItemID_Ref", all.x = TRUE)
nodes_cocit_JEL_1990s <- unique(nodes_cocit_JEL_1990s[,nb_cit := .N, by = "New_id2"][nb_cit >= nodes_cocit_threshold,c("New_id2","Annee","Nom","Titre","Revue_Abbrege","ESpecialite","nb_cit")])
doublons <- which(duplicated(nodes_cocit_JEL_1990s$New_id2))
nodes_cocit_JEL_1990s <- nodes_cocit_JEL_1990s[-doublons]

# creating a label for nodes
nodes_cocit_JEL_1990s <- nodes_cocit_JEL_1990s[, Label := paste0(Nom, Annee)]

# creating edges
edges_JEL_1990s <- unique(edges_JEL[New_id2 %in% nodes_cocit_JEL_1990s$New_id2 & between(Annee_Bibliographique,1991,1999)][, c("ID_Art","New_id2")])

# creation of the edges for the co-citation network

edges_cocit_JEL_1990s <- bibliographic_cocitation(edges_JEL_1990s, source = "ID_Art", ref = "New_id2",weight_threshold = edges_cocit_threshold)

# creating the tidygraph object  
nodes_cocit_JEL_1990s <- nodes_cocit_JEL_1990s[, New_id2:= as.character(New_id2)]
graph_cocit <- tbl_main_components(edges = edges_cocit_JEL_1990s, nodes = nodes_cocit_JEL_1990s, node_key = "New_id2", threshold_alert = 0.05, directed = FALSE)

# Optionnal: studying the distribution of nodes in each component of the graph
#components <- components_distribution(edges = edges_cocit_JEL_1990s, nodes = nodes_cocit_JEL_1990s, node_key = "New_id2")

############################### Building Nodes and Edges for coupling #######################-----------

# fixing threshold
nodes_coupling_threshold = 1
edges_coupling_threshold = 2
graph_filtering_coupling = 1

# creating the nodes with the number of citation of the nodes in the corpus
nodes_coupling_JEL_1990s <- nodes_JEL[between(Annee_Bibliographique,1991,1999),]
nb_cit<- edges_JEL[between(Annee_Bibliographique,1991,1999),nb_cit := .N,ItemID_Ref]
nodes_coupling_JEL_1990s <- merge(nodes_coupling_JEL_1990s,unique(nb_cit[,c("ItemID_Ref","nb_cit")]), by = "ItemID_Ref", all.x = "TRUE")

rm(nb_cit) # not useful anymore
# replacing NA value by 0 in nb_cit (it means that the nodes will be used in Leiden and Force Atlas, but
# not displayed in the graph)

nodes_coupling_JEL_1990s[is.na(nb_cit),]$nb_cit <- 0

# reducing the number of nodes depending of the number of citations
nodes_coupling_JEL_1990s <- unique(nodes_coupling_JEL_1990s[nb_cit >= nodes_coupling_threshold,c("ID_Art","Annee_Bibliographique","Nom","Label","Titre","Revue","ESpecialite","nb_cit","ItemID_Ref")])

# building edges
edges_JEL_1990s_bis <- unique(edges_JEL[ID_Art %in% nodes_coupling_JEL_1990s$ID_Art])

# creation of the edges for the co-citation network

edges_coupling_JEL_1990s <- bibliographic_coupling(edges_JEL_1990s_bis, source = "ID_Art", ref = "New_id2", weight_threshold = edges_coupling_threshold)

# creating the tidygraph object  
nodes_coupling_JEL_1990s <- nodes_coupling_JEL_1990s[, ID_Art:= as.character(ID_Art)]
graph_coupling <- tbl_main_components(edges = edges_coupling_JEL_1990s, nodes = nodes_coupling_JEL_1990s, node_key = "ID_Art", threshold_alert = 0.05, directed = FALSE)

# optionnal: giving spaces:

#rm(edges_coupling_JEL_1990s)
#rm(nodes_coupling_JEL_1990s)
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

# Naming communities
graph_cocit <-naming_communities(graph_cocit, centrality_measure = "nb_cit", naming = "Label")

# Calculating different centrality measures if necessary
#graph_cocit <- centrality(graph_cocit)

# Integration a size variable for implementing non-overlapping function of Force Atlast
graph_cocit <- graph_cocit %>%
  activate(nodes) %>%
  mutate(size=nb_cit)

# Running Force Atlas layout  
graph_cocit <- force_atlas(graph_cocit,seed = 1, ew.influence = 1, kgrav = 1, iter_1 = 5000, iter_2 = 1000, barnes.hut = TRUE, size_min = 50, size_max = 200)

# Saving the graph
saveRDS(graph_cocit, paste0(graph_data_path,"graph_cocit.rds"))

############################# Projection of the graph #########################

# loading the graph if necessary
#graph_cocit <- readRDS(paste0(graph_data_path,"graph_cocit.rds"))

# Identifying the labels of the most important authors
important_nodes <- top_ordering(graph_cocit, top_n_com = 2, top_n = 10, biggest_community = TRUE)

# Plotting the graph  
graph_cocit_test <- graph_cocit %>%
  activate(nodes) %>%
  filter(nb_cit >= graph_filtering_cocit)

# Plotting the graph 1 - Complete graph with the biggest communities
ggraph(graph_cocit, "manual", x = x, y = y) + 
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,2)) +
  geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.8, show.legend = FALSE) +
  scale_size_continuous(range = c(0.05,16)) +
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

# Calculating different centrality measures if necessary
# graph_coupling <- centrality(graph_coupling)

# Naming communities
graph_coupling <-naming_communities(graph_coupling, centrality_measure = "nb_cit", naming = "Label")

# Integration a size variable for implementing non-overlapping function of Force Atlast
graph_coupling <- graph_coupling %>%
  activate(nodes) %>%
  mutate(size=nb_cit)

# Running Force Atlas layout  
graph_coupling <- force_atlas(graph_coupling,seed = 1, ew.influence = 1, kgrav = 1, iter_1 = 6000, iter_2 = 2000, barnes.hut = TRUE, size_min = 50, size_max = 200)

# Saving the graph
saveRDS(graph_coupling, paste0(graph_data_path,"graph_coupling.rds"))

############################# Projection of the graph #########################

# loading the graph if necessary
#graph_coupling <- readRDS(paste0(graph_data_path,"graph_coupling.rds"))

# Identifying the labels of the most important authors
important_nodes <- top_ordering(graph_coupling, top_n_com = 2, top_n = 25, biggest_community = TRUE)

# Plotting the graph  
graph_filtering_coupling = 1

graph_coupling <- graph_coupling %>%
  filter(nb_cit >= graph_filtering_coupling)

ggraph(graph_coupling, "manual", x = x, y = y) + 
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,2)) +
  scale_edge_colour_identity() +
  geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.8, show.legend = FALSE) +
  scale_size_continuous(range = c(0.1,18)) +
  scale_fill_identity() +
  geom_label_repel(data=important_nodes, aes(x=x, y=y, label = Label, fill = color), size = 4, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  #scale_size_continuous(range = c(1,4)) +
  theme_void() +
  ggsave(paste0(picture_path,"graph_coupling.png"), width=30, height=30, units = "cm")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################# 3) Author Graph from Bibliographic Coupling ##################################------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

################## Building an author bibliographic coupling graph ######################----

nodes_authors_coupling_threshold = 1
edges_authors_coupling_threshold = 2
filtering_graph_authors_coupling = 2

# creating the nodes with the number of citation of the nodes in the corpus
nodes_coupling_JEL_1990s <- nodes_JEL[between(Annee_Bibliographique,1991,1999),]

authors_JEL <- authors_JEL[, citing_author := Nom]
nodes_coupling_JEL_1990s <- merge(nodes_coupling_JEL_1990s, unique(authors_JEL[,c("ID_Art","citing_author")]), by = "ID_Art")
nb_cit<- edges_JEL[between(Annee_Bibliographique,1991,1999) & ItemID_Ref != 0][, nb_cit := .N,ItemID_Ref]
nodes_coupling_JEL_1990s <- merge(unique(nodes_coupling_JEL_1990s),unique(nb_cit[,c("ItemID_Ref","nb_cit")]), by = "ItemID_Ref", all.x = "TRUE")

rm(nb_cit) # not useful anymore
# replacing NA value by 0 in nb_cit (it means that the nodes will be used in Leiden and Force Atlas, but
# not displayed in the graph)

nodes_coupling_JEL_1990s[is.na(nb_cit),]$nb_cit <- 0


# choosing the nodes to keep
nodes_coupling_JEL_1990s <- nodes_coupling_JEL_1990s[nb_cit >= nodes_authors_coupling_threshold]
  
# building edges
edges_JEL_1990s_bis <- unique(edges_JEL[ID_Art %in% nodes_coupling_JEL_1990s$ID_Art])

authors_edges <- merge(edges_JEL_1990s_bis,unique(nodes_coupling_JEL_1990s[,c("ID_Art","citing_author")]), by = "ID_Art", all.x = TRUE, allow.cartesian = TRUE)

authors_edges <- bibliographic_coupling(authors_edges, "citing_author", "New_id2", weight_threshold = edges_authors_coupling_threshold)

# reducing the number of nodes depending of the number of citations

authors_nodes <- unique(nodes_coupling_JEL_1990s[,c("citing_author","nb_cit")])
authors_nodes <- unique(authors_nodes[, nb_cit_author := sum(nb_cit), by = "citing_author"][order(-nb_cit_author), c("citing_author","nb_cit_author")])

# creating the tidygraph object  
graph_authors_coupling <- tbl_main_components(edges = authors_edges, nodes = authors_nodes, node_key = "citing_author", threshold_alert = 0.05, directed = FALSE)

################## Working on the author coupling tidygraph and its attributes ######################----

# Identifying communities with Leiden algorithm                         
graph_authors_coupling <- leiden_improved(graph_authors_coupling, res_1 = 1, res_2 = NULL, res_3 = NULL, n_iterations = 500)       

# Giving colors to communities
graph_authors_coupling <- community_colors(graph_authors_coupling,mypalette)

# Calculating different centrality measures
#graph_authors_coupling <- centrality(graph_authors_coupling)

# Naming communities
graph_authors_coupling <-naming_communities(graph_authors_coupling, centrality_measure = "nb_cit_author", naming = "Id")

# Integration a size variable for implementing non-overlapping function of Force Atlast
graph_authors_coupling <- graph_authors_coupling %>%
  activate(nodes) %>%
  mutate(size=nb_cit_author)

# Running Force Atlas layout  
graph_authors_coupling <- force_atlas(graph_authors_coupling,seed = 1, ew.influence = 1, kgrav = 1, iter_1 = 6000, iter_2 = 2000, barnes.hut = TRUE, size_min = 50, size_max = 200)

# Saving the graph
saveRDS(graph_authors_coupling, paste0(graph_data_path,"graph_authors_coupling.rds"))

############################# Projection of the graph #########################

# loading the graph if necessary
#graph_authors_coupling <- readRDS(paste0(graph_data_path,"graph_authors_coupling.rds"))

# Identifying the labels of the most important authors
important_nodes <- top_ordering(graph_authors_coupling, ordering_column = "nb_cit_author", top_n_com = 2, biggest_community = TRUE)

# Plotting the graph  
graph_authors_coupling_test <- graph_authors_coupling %>%
  filter(nb_cit_author >= filtering_graph_authors_coupling)

ggraph(graph_authors_coupling, "manual", x = x, y = y) + 
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,2)) +
  scale_edge_colour_identity() +
  geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.8, show.legend = FALSE) +
  scale_size_continuous(range = c(0.1,14)) +
  scale_fill_identity() +
  geom_label_repel(data=important_nodes, aes(x=x, y=y, label = Id, fill = color), size = 4, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  #scale_size_continuous(range = c(1,4)) +
  theme_void() +
  ggsave(paste0(picture_path,"graph_authors_coupling.png"), width=30, height=30, units = "cm")

################## Building an institution bibliographic coupling graph ######################----

# creating the nodes with the number of citation of the nodes in the corpus
institutions_coupling_JEL_1990s <- nodes_JEL[between(Annee_Bibliographique,1991,1999),]

authors_JEL <- authors_JEL[, citing_author := Nom]
institutions_coupling_JEL_1990s <- merge(institutions_coupling_JEL_1990s, unique(authors_JEL[,c("ID_Art","citing_author","Institution","Pays")]), by = "ID_Art")

# Calculating the weighted number of articles per institution
institutions_coupling_JEL_1990s <- institutions_coupling_JEL_1990s[, share_authors := .N, by = "ID_Art"][, share_authors := 1/share_authors][, nb_art := sum(share_authors), by = "Institution"]
institutions_coupling_JEL_1990s <- institutions_coupling_JEL_1990s[Institution != "NA" & nb_art > 10]

# building edges
edges_JEL_1990s_bis <- unique(edges_JEL[ID_Art %in% institutions_coupling_JEL_1990s$ID_Art])

institutions_edges <- merge(edges_JEL_1990s_bis,unique(institutions_coupling_JEL_1990s[,c("ID_Art","Institution")]), by = "ID_Art", all.x = TRUE, allow.cartesian = TRUE)

institutions_edges <- bibliographic_coupling(institutions_edges, "Institution", "New_id2", weight_threshold = 10)

# reducing the number of nodes depending of the number of citations

institutions_nodes <- unique(institutions_coupling_JEL_1990s[order(-nb_art), c("Institution","nb_art","Pays")])

# creating the tidygraph object  
graph_institutions_coupling <- tbl_main_components(edges = institutions_edges, nodes = institutions_nodes, node_key = "Institution", threshold_alert = 0.05, directed = FALSE)

################## Working on the Institution coupling tidygraph and its attributes ######################----

# Identifying communities with Leiden algorithm                         
graph_institutions_coupling <- leiden_improved(graph_institutions_coupling, res_1 = 1.05, res_2 = NULL, res_3 = NULL, n_iterations = 500)       

# Giving colors to communities
graph_institutions_coupling <- community_colors(graph_institutions_coupling,mypalette)

# Calculating different centrality measures
#graph_authors_coupling <- centrality(graph_authors_coupling)

# Naming communities
graph_institutions_coupling <-naming_communities(graph_institutions_coupling, centrality_measure = "nb_art", naming = "Id")

# Integration a size variable for implementing non-overlapping function of Force Atlast
graph_institutions_coupling <- graph_institutions_coupling %>%
  activate(nodes) %>%
  mutate(size=nb_art)

# Running Force Atlas layout  
graph_institutions_coupling <- force_atlas(graph_institutions_coupling,seed = 1, ew.influence = 2, kgrav = 1, iter_1 = 6000, iter_2 = 2000, barnes.hut = TRUE, size_min = 50, size_max = 200)

# Saving the graph
saveRDS(graph_institutions_coupling, paste0(graph_data_path,"graph_institutions_coupling.rds"))

############################# Projection of the graph #########################

# loading the graph if necessary
#graph_institutions_coupling <- readRDS(paste0(graph_data_path,"graph_institutions_coupling.rds"))

# Identifying the labels of the most important authors
important_nodes <- top_ordering(graph_institutions_coupling, ordering_column = "nb_art", top_n_com = 3, top_n = 10, biggest_community = TRUE)

# Plotting the graph  

ggraph(graph_institutions_coupling, "manual", x = x, y = y) + 
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,2)) +
  scale_edge_colour_identity() +
  geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.8, show.legend = FALSE) +
  scale_size_continuous(range = c(0.1,20)) +
  scale_fill_identity() +
  geom_label_repel(data=important_nodes, aes(x=x, y=y, label = Id, fill = color), size = 4, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  #scale_size_continuous(range = c(1,4)) +
  theme_void() +
  ggsave(paste0(picture_path,"graph_institutions_coupling.png"), width=25, height=25, units = "cm")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################# 4) Co-Authorship Graph  ##################################------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

################## Building an author bibliographic coupling graph ######################----

# creating the nodes with the number of citation of the nodes in the corpus
coauthorship_nodes_1990s <- nodes_JEL[between(Annee_Bibliographique,1991,1999),]
authors_JEL <- authors_JEL[, citing_author := Nom]

coauthorship_nodes_1990s <- merge(coauthorship_nodes_1990s, unique(authors_JEL[,c("ID_Art","citing_author")]), by = "ID_Art")

# building edges
coauthorship_edges_1990s <- bibliographic_coupling(unique(coauthorship_nodes_1990s[,c("ID_Art","citing_author")]), source = "citing_author", ref = "ID_Art", weight_threshold = 1)

# building nodes
coauthorship_nodes_1990s <- unique(coauthorship_nodes_1990s[,"citing_author"])

# creating the tidygraph object  
graph_coauthorship <- tbl_main_components(edges = coauthorship_edges_1990s, nodes = coauthorship_nodes_1990s, node_key = "citing_author", threshold_alert = 0.05, directed = FALSE)

################## Working on the author coupling tidygraph and its attributes ######################----

# Identifying communities with Leiden algorithm                         
graph_coauthorship <- leiden_improved(graph_coauthorship, res_1 = 0.5, res_2 = NULL, res_3 = NULL, n_iterations = 500)       

# Giving colors to communities
graph_coauthorship <- community_colors(graph_coauthorship,mypalette)

# Calculating different centrality measures
graph_coauthorship <- centrality(graph_coauthorship)

# Naming communities
graph_coauthorship <-naming_communities(graph_coauthorship, centrality_measure = "Strength", naming = "Id")

# Integration a size variable for implementing non-overlapping function of Force Atlas
graph_coauthorship <- graph_coauthorship %>%
  activate(nodes) %>%
  mutate(size=Strength)

# Running Force Atlas layout  
graph_coauthorship <- force_atlas(graph_coauthorship,seed = 1, ew.influence = 1, kgrav = 1, iter_1 = 6000, iter_2 = 2000, barnes.hut = TRUE, size_min = 50, size_max = 200)

# Saving the graph
saveRDS(graph_coauthorship, paste0(graph_data_path,"graph_coauthorship.rds"))

############################# Projection of the graph #########################

# loading the graph if necessary
#graph_coauthorship <- readRDS(paste0(graph_data_path,"graph_coauthorship.rds"))

# Identifying the labels of the most important authors
important_nodes <- top_ordering(graph_coauthorship, top_n_com = 1, top_n =10, biggest_community = TRUE)

# Plotting the graph  

ggraph(graph_coauthorship, "manual", x = x, y = y) + 
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,3)) +
  scale_edge_colour_identity() +
  geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.8, show.legend = FALSE) +
  scale_size_continuous(range = c(0.05,12)) +
  scale_fill_identity() +
  geom_label_repel(data=important_nodes, aes(x=x, y=y, label = Id, fill = color), size = 4, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  #scale_size_continuous(range = c(1,4)) +
  theme_void() +
  ggsave(paste0(picture_path,"graph_coauthorship.png"), width=30, height=30, units = "cm")


############################## institutions network from co-authorship data #########################
# creating the nodes with the number of citation of the nodes in the corpus

institutions_nodes_1990s <- nodes_JEL[between(Annee_Bibliographique,1991,1999),]

nb_cit<- edges_JEL[between(Annee_Bibliographique,1991,1999),nb_cit := .N,ItemID_Ref]
institutions_nodes_1990s <- merge(institutions_nodes_1990s,unique(nb_cit[,c("ItemID_Ref","nb_cit")]), by = "ItemID_Ref", all.x = "TRUE")

rm(nb_cit) # not useful anymore

# replacing NA value by 0 in nb_cit (it means that the nodes will be used in Leiden and Force Atlas, but
# not displayed in the graph)

institutions_nodes_1990s[is.na(nb_cit),]$nb_cit <- 0

# Merging with authors 
authors_JEL <- authors_JEL[, citing_author := Nom]
institutions_nodes_1990s <- merge(institutions_nodes_1990s, unique(authors_JEL), by = "ID_Art")
institutions_nodes_1990s <- institutions_nodes_1990s[!is.na(Institution)]

# calculating the total number of citations per university
nb_cit<- unique(institutions_nodes_1990s[, c("ID_Art","nb_cit","Institution")])
nb_cit<- nb_cit[,nb_cit_univ := sum(nb_cit), Institution]
institutions_nodes_1990s <- merge(institutions_nodes_1990s,unique(nb_cit[,c("Institution","nb_cit_univ")]), by = "Institution", all.x = "TRUE")
institutions_nodes_1990s <- institutions_nodes_1990s[nb_cit_univ > 1]

rm(nb_cit) # not useful anymore

# building edges
institutions_edges_1990s <- bibliographic_coupling(unique(institutions_nodes_1990s[,c("ID_Art","Institution")]), source = "Institution", ref = "ID_Art", weight_threshold = 1)

# building nodes

institutions_nodes_1990s <- unique(institutions_nodes_1990s[,c("Institution","Pays","nb_cit_univ")])

# creating the tidygraph object  
graph_institutions <- tbl_main_components(edges = institutions_edges_1990s, nodes = institutions_nodes_1990s, node_key = "Institution", threshold_alert = 0.05, directed = FALSE)

################## Working on the author coupling tidygraph and its attributes ######################----

# Identifying communities with Leiden algorithm                         
graph_institutions <- leiden_improved(graph_institutions, res_1 = 0.5, res_2 = NULL, res_3 = NULL, n_iterations = 500)       

# Giving colors to communities
graph_institutions <- community_colors(graph_institutions,mypalette)

# Calculating different centrality measures
#graph_institutions <- centrality(graph_institutions)

# Naming communities
graph_institutions <-naming_communities(graph_institutions, centrality_measure = "nb_cit_univ", naming = "Id")

# Integration a size variable for implementing non-overlapping function of Force Atlas
graph_institutions <- graph_institutions %>%
  activate(nodes) %>%
  mutate(size=nb_cit_univ)

# Running Force Atlas layout  
graph_institutions <- force_atlas(graph_institutions,seed = 1, ew.influence = 1, kgrav = 1, iter_1 = 6000, iter_2 = 2000, barnes.hut = TRUE, size_min = 20, size_max = 100)

# Saving the graph
saveRDS(graph_institutions, paste0(graph_data_path,"graph_institutions.rds"))

############################# Projection of the graph #########################

# loading the graph if necessary
#graph_institutions <- readRDS(paste0(graph_data_path,"graph_institutions.rds"))

# Identifying the labels of the most important authors
important_nodes <- top_ordering(graph_institutions, ordering_column ="nb_cit_univ", top_n_com = 2, top_n =15, biggest_community = TRUE)

# Plotting the graph  

ggraph(graph_institutions, "manual", x = x, y = y) + 
  geom_edge_arc(aes(width = weight, color = color_edges), alpha = 0.3, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,3)) +
  scale_edge_colour_identity() +
  geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.8, show.legend = FALSE) +
  scale_size_continuous(range = c(0.1,20)) +
  scale_fill_identity() +
  geom_label_repel(data=important_nodes, aes(x=x, y=y, label = Id, fill = color), size = 4, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  #scale_size_continuous(range = c(1,4)) +
  theme_void() +
  ggsave(paste0(picture_path,"graph_institutions.png"), width=30, height=30, units = "cm")
