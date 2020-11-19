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
picture_path <- "/home/aurelien/macro_AA/Static_Network_Analysis/Pictures/"

source("/home/aurelien/macro_AA/Static_Network_Analysis/functions_for_network_analysis.R")

mypalette <- c("#1969B3","#01A5D8","#DA3E61","#3CB95F","#E0AF0C","#E25920","#6C7FC9","#DE9493","#CD242E","#6F4288","#B2EEF8","#7FF6FD","#FDB8D6","#8BF9A9","#FEF34A","#FEC57D","#DAEFFB","#FEE3E1","#FBB2A7","#EFD7F2","#5CAADA","#37D4F5","#F5779B","#62E186","#FBDA28","#FB8F4A","#A4B9EA","#FAC2C0","#EB6466","#AD87BC","#0B3074","#00517C","#871B2A","#1A6029","#7C4B05","#8A260E","#2E3679","#793F3F","#840F14","#401C56","#003C65","#741A09","#602A2A","#34134A","#114A1B","#27DDD1","#27DD8D","#4ADD27","#D3DD27","#DDA427","#DF2935","#DD27BC","#BA27DD","#3227DD","#2761DD","#27DDD1")

# Loading files
nodes_JEL <- readRDS(paste0(data_path,"JEL_matched_corpus_nodes.rds"))
nodes_old_JEL <- readRDS(paste0(data_path,"Old_JEL_matched_corpus_nodes.rds"))
nodes_JEL <- rbind(nodes_JEL,nodes_old_JEL)

edges_JEL <- readRDS(paste0(data_path,"JEL_matched_corpus_edges.rds"))
edges_old_JEL <- readRDS(paste0(data_path,"Old_JEL_matched_corpus_edges.rds"))
edges_JEL <- rbind(edges_JEL,edges_old_JEL)

authors_JEL <- readRDS(paste0(data_path,"JEL_matched_corpus_authors.rds"))
authors_old_JEL <- readRDS(paste0(data_path,"Old_JEL_matched_corpus_authors.rds"))
authors_JEL <- rbind(authors_JEL,authors_old_JEL)

ref_info_JEL <- readRDS(paste0(data_path,"JEL_matched_corpus_references_info.rds"))
ref_info_old_JEL <- readRDS(paste0(data_path,"Old_JEL_matched_corpus_references_info.rds"))
ref_info_JEL <- rbind(ref_info_JEL,ref_info_old_JEL)

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

######################### Fixing the sub-periods and thresholds ##########################################------------

# The idea is to have a vector of dates, the odd ones (1,3,5...) being the beginning data, the even ones the end date.

dates <- c(1970,1980,1981,1990,1991,2000,2001,2008,2009,2015)

# fixing threshold
nodes_cocit_threshold = 5
nodes_coupling_threshold = 1
edges_threshold = 1
Limit_nodes = 20000
Limit_edges = 300000

############################### Building Nodes and Edges for co-citation #######################

for(i in seq(1,length(dates),2)){
  nodes_cocit_threshold = 5
  edges_threshold = 1

# creating nodes
nodes_cocit_JEL <- edges_JEL[between(Annee_Bibliographique,dates[i],dates[i+1]),]
#nodes_cocit_JEL_1990s <- merge(nodes_cocit_JEL_1990s, nodes_JEL[,c("ItemID_Ref","Titre","Code_Revue")], by = "ItemID_Ref", all.x = TRUE)
nodes_cocit_JEL <- unique(nodes_cocit_JEL[,nb_cit := .N, by = "New_id2"][nb_cit >= nodes_cocit_threshold,c("New_id2","Annee","Nom","Titre","Revue_Abbrege","ESpecialite","nb_cit")])
doublons <- which(duplicated(nodes_cocit_JEL$New_id2))
nodes_cocit_JEL <- nodes_cocit_JEL[-doublons]

# creating a label for nodes
nodes_cocit_JEL <- nodes_cocit_JEL[, Label := paste0(Nom,"-",Annee)]

# creating edges
edges_cocit_JEL<- unique(edges_JEL[New_id2 %in% nodes_cocit_JEL$New_id2 & between(Annee_Bibliographique,dates[i],dates[i+1])][, c("ID_Art","New_id2")])

# creation of the edges for the co-citation network

edges_cocit_JEL <- bibliographic_cocitation(edges_cocit_JEL, source = "ID_Art", ref = "New_id2",weight_threshold = edges_threshold)

# Loop to avoid to large networks - Step 1: reducing nodes
if(length(nodes_cocit_JEL$New_id2) > Limit_nodes){
  for(j in 1:100){
    nodes_cocit_threshold = nodes_cocit_threshold + 1
    # creating nodes
    nodes_cocit_JEL <- edges_JEL[between(Annee_Bibliographique,dates[i],dates[i+1]),]
    #nodes_cocit_JEL_1990s <- merge(nodes_cocit_JEL_1990s, nodes_JEL[,c("ItemID_Ref","Titre","Code_Revue")], by = "ItemID_Ref", all.x = TRUE)
    nodes_cocit_JEL <- unique(nodes_cocit_JEL[,nb_cit := .N, by = "New_id2"][nb_cit >= nodes_cocit_threshold,c("New_id2","Annee","Nom","Titre","Revue_Abbrege","ESpecialite","nb_cit")])
    doublons <- which(duplicated(nodes_cocit_JEL$New_id2))
    nodes_cocit_JEL <- nodes_cocit_JEL[-doublons]
    
    # creating a label for nodes
    nodes_cocit_JEL <- nodes_cocit_JEL[, Label := paste0(Nom,"-",Annee)]
    
    # creating edges
    edges_cocit_JEL<- unique(edges_JEL[New_id2 %in% nodes_cocit_JEL$New_id2 & between(Annee_Bibliographique,dates[i],dates[i+1])][, c("ID_Art","New_id2")])
    
    # creation of the edges for the co-citation network
    
    edges_cocit_JEL <- bibliographic_cocitation(edges_cocit_JEL, source = "ID_Art", ref = "New_id2",weight_threshold = edges_threshold)
    
    if(length(nodes_cocit_JEL$New_id2) < Limit_nodes){
      break
    }
  }
}

# Loop to avoid to large networks - Step 2: reducing edges
if(length(edges_cocit_JEL$from) > Limit_edges){
  for(k in 1:100){
    edges_threshold = edges_threshold + 1
    # creating nodes
    nodes_cocit_JEL <- edges_JEL[between(Annee_Bibliographique,dates[i],dates[i+1]),]
    #nodes_cocit_JEL_1990s <- merge(nodes_cocit_JEL_1990s, nodes_JEL[,c("ItemID_Ref","Titre","Code_Revue")], by = "ItemID_Ref", all.x = TRUE)
    nodes_cocit_JEL <- unique(nodes_cocit_JEL[,nb_cit := .N, by = "New_id2"][nb_cit >= nodes_cocit_threshold,c("New_id2","Annee","Nom","Titre","Revue_Abbrege","ESpecialite","nb_cit")])
    doublons <- which(duplicated(nodes_cocit_JEL$New_id2))
    nodes_cocit_JEL <- nodes_cocit_JEL[-doublons]
    
    # creating a label for nodes
    nodes_cocit_JEL <- nodes_cocit_JEL[, Label := paste0(Nom,"-",Annee)]
    
    # creating edges
    edges_cocit_JEL<- unique(edges_JEL[New_id2 %in% nodes_cocit_JEL$New_id2 & between(Annee_Bibliographique,dates[i],dates[i+1])][, c("ID_Art","New_id2")])
    
    # creation of the edges for the co-citation network
    edges_cocit_JEL <- bibliographic_cocitation(edges_cocit_JEL, source = "ID_Art", ref = "New_id2",weight_threshold = edges_threshold)
    
    if(length(edges_cocit_JEL$from) < Limit_edges){
      break
    }
  }
}

# creating the tidygraph object  
nodes_cocit_JEL <- nodes_cocit_JEL[, New_id2:= as.character(New_id2)]
nodes_cocit_JEL$threshold = nodes_cocit_threshold
edges_cocit_JEL$threshold = edges_threshold

graph_cocit <- tbl_main_components(edges = edges_cocit_JEL, nodes = nodes_cocit_JEL, node_key = "New_id2", threshold_alert = 0.05, directed = FALSE)

saveRDS(graph_cocit, paste0(graph_data_path,"prior_graph_cocit_",dates[i],"-",dates[i+1],".rds"))
}

# Optionnal: studying the distribution of nodes in each component of the graph
#components <- components_distribution(edges = edges_cocit_JEL, nodes = nodes_cocit_JEL, node_key = "New_id2")

############################### Building Nodes and Edges for coupling #######################-----------

for(i in seq(1,length(dates),2)){
nodes_coupling_threshold = 1
edges_threshold = 2

# creating the nodes with the number of citation of the nodes in the corpus
nodes_coupling_JEL <- nodes_JEL[between(Annee_Bibliographique,dates[i],dates[i+1]),]
nb_cit<- edges_JEL[between(Annee_Bibliographique,dates[i],dates[i+1]),nb_cit := .N,ItemID_Ref]
nodes_coupling_JEL <- merge(nodes_coupling_JEL,unique(nb_cit[,c("ItemID_Ref","nb_cit")]), by = "ItemID_Ref", all.x = "TRUE")

rm(nb_cit) # not useful anymore
# replacing NA value by 0 in nb_cit (it means that the nodes will be used in Leiden and Force Atlas, but
# not displayed in the graph)

nodes_coupling_JEL[is.na(nb_cit),]$nb_cit <- 0

# reducing the number of nodes depending of the number of citations
nodes_coupling_JEL <- unique(nodes_coupling_JEL[nb_cit >= nodes_coupling_threshold,c("ID_Art","Annee_Bibliographique","Nom","Label","Titre","Revue","ESpecialite","nb_cit","ItemID_Ref")])

# building edges
edges_coupling_JEL <- unique(edges_JEL[ID_Art %in% nodes_coupling_JEL$ID_Art])

# creation of the edges for the co-citation network

edges_coupling_JEL <- bibliographic_coupling(edges_coupling_JEL, source = "ID_Art", ref = "New_id2", weight_threshold = edges_threshold)

# Loop to avoid to large networks - Step 1: reducing nodes
if(length(nodes_coupling_JEL$ID_Art) > Limit_nodes){
  for(j in 1:100){
    nodes_coupling_threshold = nodes_coupling_threshold + 1

    # creating the nodes with the number of citation of the nodes in the corpus
    nodes_coupling_JEL <- nodes_JEL[between(Annee_Bibliographique,dates[i],dates[i+1]),]
    nb_cit<- edges_JEL[between(Annee_Bibliographique,dates[i],dates[i+1]),nb_cit := .N,ItemID_Ref]
    nodes_coupling_JEL <- merge(nodes_coupling_JEL,unique(nb_cit[,c("ItemID_Ref","nb_cit")]), by = "ItemID_Ref", all.x = "TRUE")
    
    rm(nb_cit) # not useful anymore
    # replacing NA value by 0 in nb_cit (it means that the nodes will be used in Leiden and Force Atlas, but
    # not displayed in the graph)
    
    nodes_coupling_JEL[is.na(nb_cit),]$nb_cit <- 0
    
    # reducing the number of nodes depending of the number of citations
    nodes_coupling_JEL <- unique(nodes_coupling_JEL[nb_cit >= nodes_coupling_threshold,c("ID_Art","Annee_Bibliographique","Nom","Label","Titre","Revue","ESpecialite","nb_cit","ItemID_Ref")])
    
    # building edges
    edges_coupling_JEL <- unique(edges_JEL[ID_Art %in% nodes_coupling_JEL$ID_Art])
    
    # creation of the edges for the co-citation network
    
    edges_coupling_JEL <- bibliographic_coupling(edges_coupling_JEL, source = "ID_Art", ref = "New_id2", weight_threshold = edges_threshold)
    
    if(length(nodes_coupling_JEL$ID_Art) < Limit_nodes){
      break
    }
  }
}

# Loop to avoid to large networks - Step 2: reducing edges
if(length(edges_coupling_JEL$from) > Limit_edges){
  for(k in 1:100){
    edges_threshold = edges_threshold + 1

    # creating the nodes with the number of citation of the nodes in the corpus
    nodes_coupling_JEL <- nodes_JEL[between(Annee_Bibliographique,dates[i],dates[i+1]),]
    nb_cit<- edges_JEL[between(Annee_Bibliographique,dates[i],dates[i+1]),nb_cit := .N,ItemID_Ref]
    nodes_coupling_JEL <- merge(nodes_coupling_JEL,unique(nb_cit[,c("ItemID_Ref","nb_cit")]), by = "ItemID_Ref", all.x = "TRUE")
    
    rm(nb_cit) # not useful anymore
    # replacing NA value by 0 in nb_cit (it means that the nodes will be used in Leiden and Force Atlas, but
    # not displayed in the graph)
    
    nodes_coupling_JEL[is.na(nb_cit),]$nb_cit <- 0
    
    # reducing the number of nodes depending of the number of citations
    nodes_coupling_JEL <- unique(nodes_coupling_JEL[nb_cit >= nodes_coupling_threshold,c("ID_Art","Annee_Bibliographique","Nom","Label","Titre","Revue","ESpecialite","nb_cit","ItemID_Ref")])
    
    # building edges
    edges_coupling_JEL <- unique(edges_JEL[ID_Art %in% nodes_coupling_JEL$ID_Art])
    
    # creation of the edges for the co-citation network
    
    edges_coupling_JEL <- bibliographic_coupling(edges_coupling_JEL, source = "ID_Art", ref = "New_id2", weight_threshold = edges_threshold)
    
    if(length(edges_coupling_JEL$from) < Limit_edges){
      break
    }
  }
}

# creating the tidygraph object  
nodes_coupling_JEL <- nodes_coupling_JEL[, ID_Art:= as.character(ID_Art)]
nodes_coupling_JEL$threshold = nodes_coupling_threshold
edges_coupling_JEL$threshold = edges_threshold

graph_coupling <- tbl_main_components(edges = edges_coupling_JEL, nodes = nodes_coupling_JEL, node_key = "ID_Art", threshold_alert = 0.05, directed = FALSE)

saveRDS(graph_coupling, paste0(graph_data_path,"prior_graph_coupling_",dates[i],"-",dates[i+1],".rds"))

}


# Optionnal: studying the distribution of nodes in each component of the graph
#components <- components_distribution(edges = edges_coupling_JEL_1990s, nodes = nodes_coupling_JEL_1990s, node_key = "ID_Art")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################ 2) Bibliographic Co-citation #################################-------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

######################### Working on the tidygraph for cocitation and its attributes #################################-------

for(i in seq(1,length(dates),2)){

graph_cocit <- readRDS(paste0(graph_data_path,"prior_graph_cocit_",dates[i],"-",dates[i+1],".rds"))

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
saveRDS(graph_cocit, paste0(graph_data_path,"graph_cocit_",dates[i],"-",dates[i+1],".rds"))

}

############################# Projection of the graph #########################

# loading the graph if necessary
graph_cocit <- readRDS(paste0(graph_data_path,"graph_cocit_",dates[i],"-",dates[i+1],".rds"))

# Identifying the labels of the most important authors
important_nodes <- top_ordering(graph_cocit, top_n_com = 2, top_n = 10, biggest_community = TRUE)

# Plotting the graph  
graph_cocit <- graph_cocit %>%
 activate(nodes) %>%
  filter(nb_cit >= 6)

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
  ggsave(paste0(picture_path,"graph_cocit",dates[i],"-",dates[i+1],".png"), width=35, height=35, units = "cm")


#################################### Graph of community as nodes from cocit ######################################

for(i in seq(1,length(dates),2)){
  graph_cocit <- readRDS(paste0(graph_data_path,"graph_cocit_",dates[i],"-",dates[i+1],".rds"))
  
  # Using the function we have built
  graph_cocit_community <- graph_community(graph_cocit)
  
  # Saving the graph
  saveRDS(graph_cocit_community, paste0(graph_data_path,"graph_cocit_community_",dates[i],"-",dates[i+1],".rds"))
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################# 2) Bibliographic Coupling ##################################------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

################## Working on the coupling tidygraph and its attributes ######################----

for(i in seq(1,length(dates),2)){
graph_coupling <- readRDS(paste0(graph_data_path,"graph_coupling_",dates[i],"-",dates[i+1],".rds"))
  
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
graph_coupling <- force_atlas(graph_coupling,seed = 1, ew.influence = 1, kgrav = 1, iter_1 = 5000, iter_2 = 1000, barnes.hut = TRUE, size_min = 50, size_max = 200)

# Saving the graph
saveRDS(graph_coupling, paste0(graph_data_path,"graph_coupling_",dates[i],"-",dates[i+1],".rds"))
}

############################# Projection of the graph #########################
i = 7
# loading the graph if necessary
graph_coupling <- readRDS(paste0(graph_data_path,"graph_coupling_",dates[i],"-",dates[i+1],".rds"))

# Identifying the labels of the most important authors
important_nodes <- top_ordering(graph_coupling, top_n_com = 2, top_n = 10, biggest_community = TRUE)

# Plotting the graph  

#graph_coupling <- graph_coupling %>%
# activate(nodes) %>%
 # filter(nb_cit >= graph_filtering_coupling)

ggraph(graph_coupling, "manual", x = x, y = y) + 
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,2)) +
  scale_edge_colour_identity() +
  geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.8, show.legend = FALSE) +
  scale_size_continuous(range = c(0.05,15)) +
  scale_fill_identity() +
  geom_label_repel(data=important_nodes, aes(x=x, y=y, label = Label, fill = color), size = 4, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  #scale_size_continuous(range = c(1,4)) +
  theme_void() +
  ggsave(paste0(picture_path,"graph_coupling_",dates[i],"-",dates[i+1],".png"), width=35, height=35, units = "cm")

#################################### Graph of community as nodes from coupling ######################################

for(i in seq(1,length(dates),2)){
graph_coupling <- readRDS(paste0(graph_data_path,"graph_coupling_",dates[i],"-",dates[i+1],".rds"))

# Using the function we have built
graph_coupling_community <- graph_community(graph_coupling)

# Saving the graph
saveRDS(graph_coupling_community, paste0(graph_data_path,"graph_coupling_community_",dates[i],"-",dates[i+1],".rds"))
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################# 3) Author Graph from Bibliographic Coupling ##################################------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

################## Building an author bibliographic coupling graph ######################----

for(i in seq(1,length(dates),2)){
  
nodes_coupling_threshold = 1
edges_threshold = 2

# creating the nodes with the number of citation of the nodes in the corpus
nodes_coupling <- nodes_JEL[between(Annee_Bibliographique,dates[i],dates[i+1]),]

authors_JEL <- authors_JEL[, citing_author := Nom]
nodes_coupling <- merge(nodes_coupling, unique(authors_JEL[,c("ID_Art","citing_author")]), by = "ID_Art")
nb_cit<- edges_JEL[between(Annee_Bibliographique,dates[i],dates[i+1]) & ItemID_Ref != 0][, nb_cit := .N,ItemID_Ref]
nodes_coupling <- merge(unique(nodes_coupling),unique(nb_cit[,c("ItemID_Ref","nb_cit")]), by = "ItemID_Ref", all.x = "TRUE")

rm(nb_cit) # not useful anymore
# replacing NA value by 0 in nb_cit (it means that the nodes will be used in Leiden and Force Atlas, but
# not displayed in the graph)

nodes_coupling[is.na(nb_cit),]$nb_cit <- 0


# choosing the nodes to keep
nodes_coupling <- nodes_coupling[nb_cit >= nodes_coupling_threshold]
  
# building edges
edges_coupling <- unique(edges_JEL[ID_Art %in% nodes_coupling$ID_Art])

authors_edges <- merge(edges_coupling,unique(nodes_coupling[,c("ID_Art","citing_author")]), by = "ID_Art", all.x = TRUE, allow.cartesian = TRUE)

authors_edges <- bibliographic_coupling(authors_edges, "citing_author", "New_id2", weight_threshold = edges_threshold)

# reducing the number of nodes depending of the number of citations

authors_nodes <- unique(nodes_coupling[,c("citing_author","nb_cit")])
authors_nodes <- unique(authors_nodes[, nb_cit_author := sum(nb_cit), by = "citing_author"][order(-nb_cit_author), c("citing_author","nb_cit_author")])

# Loop to avoid to large networks - Step 1: reducing nodes
if(length(authors_nodes$citing_author) > Limit_nodes){
  for(j in 1:100){
    nodes_coupling_threshold = nodes_coupling_threshold + 1
  
    # creating the nodes with the number of citation of the nodes in the corpus
    nodes_coupling <- nodes_JEL[between(Annee_Bibliographique,dates[i],dates[i+1]),]
    
    authors_JEL <- authors_JEL[, citing_author := Nom]
    nodes_coupling <- merge(nodes_coupling, unique(authors_JEL[,c("ID_Art","citing_author")]), by = "ID_Art")
    nb_cit<- edges_JEL[between(Annee_Bibliographique,dates[i],dates[i+1]) & ItemID_Ref != 0][, nb_cit := .N,ItemID_Ref]
    nodes_coupling <- merge(unique(nodes_coupling),unique(nb_cit[,c("ItemID_Ref","nb_cit")]), by = "ItemID_Ref", all.x = "TRUE")
    
    rm(nb_cit) # not useful anymore
    # replacing NA value by 0 in nb_cit (it means that the nodes will be used in Leiden and Force Atlas, but
    # not displayed in the graph)
    
    nodes_coupling[is.na(nb_cit),]$nb_cit <- 0
    
    
    # choosing the nodes to keep
    nodes_coupling <- nodes_coupling[nb_cit >= nodes_coupling_threshold]
    
    # building edges
    edges_coupling <- unique(edges_JEL[ID_Art %in% nodes_coupling$ID_Art])
    
    authors_edges <- merge(edges_coupling,unique(nodes_coupling[,c("ID_Art","citing_author")]), by = "ID_Art", all.x = TRUE, allow.cartesian = TRUE)
    
    authors_edges <- bibliographic_coupling(authors_edges, "citing_author", "New_id2", weight_threshold = edges_threshold)
    
    # reducing the number of nodes depending of the number of citations
    
    authors_nodes <- unique(nodes_coupling[,c("citing_author","nb_cit")])
    authors_nodes <- unique(authors_nodes[, nb_cit_author := sum(nb_cit), by = "citing_author"][order(-nb_cit_author), c("citing_author","nb_cit_author")])
    
   
    if(length(authors_nodes$citing_author)  < Limit_nodes){
      break
    }
  }
}

# Loop to avoid to large networks - Step 2: reducing edges
if(length(authors_edges$from) > Limit_edges){
  for(k in 1:100){
    edges_threshold = edges_threshold + 1
    
    # creating the nodes with the number of citation of the nodes in the corpus
    nodes_coupling <- nodes_JEL[between(Annee_Bibliographique,dates[i],dates[i+1]),]
    
    authors_JEL <- authors_JEL[, citing_author := Nom]
    nodes_coupling <- merge(nodes_coupling, unique(authors_JEL[,c("ID_Art","citing_author")]), by = "ID_Art")
    nb_cit<- edges_JEL[between(Annee_Bibliographique,dates[i],dates[i+1]) & ItemID_Ref != 0][, nb_cit := .N,ItemID_Ref]
    nodes_coupling <- merge(unique(nodes_coupling),unique(nb_cit[,c("ItemID_Ref","nb_cit")]), by = "ItemID_Ref", all.x = "TRUE")
    
    rm(nb_cit) # not useful anymore
    # replacing NA value by 0 in nb_cit (it means that the nodes will be used in Leiden and Force Atlas, but
    # not displayed in the graph)
    
    nodes_coupling[is.na(nb_cit),]$nb_cit <- 0
    
    
    # choosing the nodes to keep
    nodes_coupling <- nodes_coupling[nb_cit >= nodes_coupling_threshold]
    
    # building edges
    edges_coupling <- unique(edges_JEL[ID_Art %in% nodes_coupling$ID_Art])
    
    authors_edges <- merge(edges_coupling,unique(nodes_coupling[,c("ID_Art","citing_author")]), by = "ID_Art", all.x = TRUE, allow.cartesian = TRUE)
    
    authors_edges <- bibliographic_coupling(authors_edges, "citing_author", "New_id2", weight_threshold = edges_threshold)
    
    # reducing the number of nodes depending of the number of citations
    
    authors_nodes <- unique(nodes_coupling[,c("citing_author","nb_cit")])
    authors_nodes <- unique(authors_nodes[, nb_cit_author := sum(nb_cit), by = "citing_author"][order(-nb_cit_author), c("citing_author","nb_cit_author")])

    if(length(authors_edges$from) < Limit_edges){
      break
    }
  }
}


authors_nodes$threshold = nodes_coupling_threshold
authors_edges$threshold = edges_threshold

# creating the tidygraph object  
graph_authors_coupling <- tbl_main_components(edges = authors_edges, nodes = authors_nodes, node_key = "citing_author", threshold_alert = 0.05, directed = FALSE)

saveRDS(graph_authors_coupling, paste0(graph_data_path,"prior_graph_authors_coupling_",dates[i],"-",dates[i+1],".rds"))
}

################## Working on the author coupling tidygraph and its attributes ######################----

for(i in seq(1,length(dates),2)){
  
graph_authors_coupling <- readRDS(paste0(graph_data_path,"prior_graph_authors_coupling_",dates[i],"-",dates[i+1],".rds"))

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
graph_authors_coupling <- force_atlas(graph_authors_coupling,seed = 1, ew.influence = 1, kgrav = 1, iter_1 = 5000, iter_2 = 1000, barnes.hut = TRUE, size_min = 50, size_max = 200)

# Saving the graph
saveRDS(graph_authors_coupling, paste0(graph_data_path,"graph_authors_coupling_",dates[i],"-",dates[i+1],".rds"))

}

############################# Projection of the graph #########################

#i = 9
# loading the graph if necessary
graph_authors_coupling <- readRDS(paste0(graph_data_path,"graph_authors_coupling_",dates[i],"-",dates[i+1],".rds"))

# Identifying the labels of the most important authors
important_nodes <- top_ordering(graph_authors_coupling, ordering_column = "nb_cit_author", top_n_com = 2, biggest_community = TRUE)

# Plotting the graph  
graph_authors_coupling <- graph_authors_coupling %>%
filter(nb_cit_author >= 2)

ggraph(graph_authors_coupling, "manual", x = x, y = y) + 
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,2)) +
  scale_edge_colour_identity() +
  geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.8, show.legend = FALSE) +
  scale_size_continuous(range = c(0.05,14)) +
  scale_fill_identity() +
  geom_label_repel(data=important_nodes, aes(x=x, y=y, label = Id, fill = color), size = 4, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  #scale_size_continuous(range = c(1,4)) +
  theme_void() +
  ggsave(paste0(picture_path,"graph_authors_coupling_",dates[i],"-",dates[i+1],".png"), width=35, height=35, units = "cm")


################## Authors coupling graph with community as nodes ######################----

for(i in seq(1,length(dates),2)){

graph_authors_coupling <- readRDS(paste0(graph_data_path,"graph_authors_coupling_",dates[i],"-",dates[i+1],".rds"))

# Using the function we have built
graph_authors_coupling_community <- graph_community(graph_authors_coupling)

# Saving the graph
saveRDS(graph_authors_coupling_community, paste0(graph_data_path,"graph_authors_coupling_community_",dates[i],"-",dates[i+1],".rds"))
}

################## Building an institution bibliographic coupling graph ######################----

for(i in seq(5,length(dates),2)){
nb_art_threshold = 10
edges_threshold = 10

# creating the nodes with the number of citation of the nodes in the corpus
institutions_coupling_JEL_1990s <- nodes_JEL[between(Annee_Bibliographique,dates[i],dates[i+1]),]

authors_JEL <- authors_JEL[, citing_author := Nom]
institutions_coupling_JEL_1990s <- merge(institutions_coupling_JEL_1990s, unique(authors_JEL[,c("ID_Art","citing_author","Institution","Pays")]), by = "ID_Art")

# Calculating the weighted number of articles per institution
institutions_coupling_JEL_1990s <- institutions_coupling_JEL_1990s[, share_authors := .N, by = "ID_Art"][, share_authors := 1/share_authors][, nb_art := sum(share_authors), by = "Institution"]
institutions_coupling_JEL_1990s <- institutions_coupling_JEL_1990s[Institution != "NA" & nb_art > nb_art_threshold]

# building edges
edges_JEL_1990s_bis <- unique(edges_JEL[ID_Art %in% institutions_coupling_JEL_1990s$ID_Art])

institutions_edges <- merge(edges_JEL_1990s_bis,unique(institutions_coupling_JEL_1990s[,c("ID_Art","Institution")]), by = "ID_Art", all.x = TRUE, allow.cartesian = TRUE)

institutions_edges <- bibliographic_coupling(institutions_edges, "Institution", "New_id2", weight_threshold = edges_threshold)

# reducing the number of nodes depending of the number of citations

institutions_nodes <- unique(institutions_coupling_JEL_1990s[order(-nb_art), c("Institution","nb_art","Pays")])

institutions_nodes$threshold = nb_art_threshold
institutions_edges$threshold = edges_threshold

# creating the tidygraph object  
graph_institutions_coupling <- tbl_main_components(edges = institutions_edges, nodes = institutions_nodes, node_key = "Institution", threshold_alert = 0.05, directed = FALSE)

saveRDS(graph_institutions_coupling, paste0(graph_data_path,"prior_graph_institutions_coupling_",dates[i],"-",dates[i+1],".rds"))
}

################## Working on the Institution coupling tidygraph and its attributes ######################----

for(i in seq(5,length(dates),2)){
graph_institutions_coupling <- readRDS(paste0(graph_data_path,"prior_graph_institutions_coupling_",dates[i],"-",dates[i+1],".rds"))

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
saveRDS(graph_institutions_coupling, paste0(graph_data_path,"graph_institutions_coupling_",dates[i],"-",dates[i+1],".rds"))
}

############################# Projection of the graph #########################

#i = 9
# loading the graph if necessary
graph_institutions_coupling <- readRDS(paste0(graph_data_path,"graph_institutions_coupling_",dates[i],"-",dates[i+1],".rds"))

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
  ggsave(paste0(picture_path,"graph_institutions_coupling_",dates[i],"-",dates[i+1],".png"), width=25, height=25, units = "cm")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################# 4) Co-Authorship Graph  ##################################------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

################## Building an author bibliographic coupling graph ######################----

for(i in seq(1,length(dates),2)){
# creating the nodes with the number of citation of the nodes in the corpus
coauthorship_nodes_1990s <- nodes_JEL[between(Annee_Bibliographique,dates[i],dates[i+1]),]
authors_JEL <- authors_JEL[, citing_author := Nom]

coauthorship_nodes_1990s <- merge(coauthorship_nodes_1990s, unique(authors_JEL[,c("ID_Art","citing_author")]), by = "ID_Art")

# building edges
coauthorship_edges_1990s <- bibliographic_coupling(unique(coauthorship_nodes_1990s[,c("ID_Art","citing_author")]), source = "citing_author", ref = "ID_Art", weight_threshold = 1)

# building nodes
coauthorship_nodes_1990s <- unique(coauthorship_nodes_1990s[,"citing_author"])

# creating the tidygraph object  
graph_coauthorship <- tbl_main_components(edges = coauthorship_edges_1990s, nodes = coauthorship_nodes_1990s, node_key = "citing_author", threshold_alert = 0.05, directed = FALSE)

# Saving the graph
saveRDS(graph_coauthorship, paste0(graph_data_path,"prior_graph_coauthorship_",dates[i],"-",dates[i+1],".rds"))
}

################## Working on the author coupling tidygraph and its attributes ######################----

for(i in seq(1,length(dates),2)){
graph_coauthorship <- readRDS(paste0(graph_data_path,"prior_graph_coauthorship_",dates[i],"-",dates[i+1],".rds"))

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
saveRDS(graph_coauthorship, paste0(graph_data_path,"graph_coauthorship_",dates[i],"-",dates[i+1],".rds"))
}

############################# Projection of the graph #########################

i = 9
# loading the graph if necessary
graph_coauthorship <- readRDS(paste0(graph_data_path,"graph_coauthorship_",dates[i],"-",dates[i+1],".rds"))

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
  ggsave(paste0(picture_path,"graph_coauthorship_",dates[i],"-",dates[i+1],".png"), width=30, height=30, units = "cm")


############################## institutions network from co-authorship data #########################
# creating the nodes with the number of citation of the nodes in the corpus

for(i in seq(5,length(dates),2)){
nb_art_threshold = 10
edges_threshold = 1
  
institutions_nodes <- nodes_JEL[between(Annee_Bibliographique,dates[i],dates[i+1]),]

# Merging with authors 
authors_JEL <- authors_JEL[, citing_author := Nom]
institutions_nodes <- merge(institutions_nodes, unique(authors_JEL), by = "ID_Art")
institutions_nodes <- institutions_nodes[!is.na(Institution)]

# Calculating the weighted number of articles per institution
institutions_nodes <- institutions_nodes[, share_authors := .N, by = "ID_Art"][, share_authors := 1/share_authors][, nb_art := sum(share_authors), by = "Institution"]
institutions_nodes <- institutions_nodes[Institution != "NA" & nb_art >= nb_art_threshold]


# building edges
institutions_edges <- bibliographic_coupling(unique(institutions_nodes[,c("ID_Art","Institution")]), source = "Institution", ref = "ID_Art", weight_threshold = edges_threshold)

# building nodes

institutions_nodes <- unique(institutions_nodes[,c("Institution","Pays","nb_art")])

# creating the tidygraph object  
graph_institutions <- tbl_main_components(edges = institutions_edges, nodes = institutions_nodes, node_key = "Institution", threshold_alert = 0.05, directed = FALSE)

saveRDS(graph_institutions, paste0(graph_data_path,"prior_graph_co-authorship_institutions_",dates[i],"-",dates[i+1],".rds"))
}

################## Working on the author coupling tidygraph and its attributes ######################----

for(i in seq(5,length(dates),2)){
  
graph_institutions <-  readRDS(paste0(graph_data_path,"prior_graph_co-authorship_institutions_",dates[i],"-",dates[i+1],".rds"))
  
# Identifying communities with Leiden algorithm                         
graph_institutions <- leiden_improved(graph_institutions, res_1 = 0.5, res_2 = NULL, res_3 = NULL, n_iterations = 500)       

# Giving colors to communities
graph_institutions <- community_colors(graph_institutions,mypalette)

# Calculating different centrality measures
#graph_institutions <- centrality(graph_institutions)

# Naming communities
graph_institutions <-naming_communities(graph_institutions, centrality_measure = "nb_art", naming = "Id")

# Integration a size variable for implementing non-overlapping function of Force Atlas
graph_institutions <- graph_institutions %>%
  activate(nodes) %>%
  mutate(size=nb_art)

# Running Force Atlas layout  
graph_institutions <- force_atlas(graph_institutions,seed = 1, ew.influence = 1, kgrav = 1, iter_1 = 6000, iter_2 = 2000, barnes.hut = TRUE, size_min = 20, size_max = 100)

# Saving the graph
saveRDS(graph_institutions, paste0(graph_data_path,"graph_co-authorship_institutions_",dates[i],"-",dates[i+1],".rds"))
}

############################# Projection of the graph #########################

#i = 9
# loading the graph if necessary
graph_institutions <-  readRDS(paste0(graph_data_path,"graph_co-authorship_institutions_",dates[i],"-",dates[i+1],".rds"))

# Identifying the labels of the most important authors
important_nodes <- top_ordering(graph_institutions, ordering_column ="nb_art", top_n_com = 2, top_n =15, biggest_community = TRUE)

# Plotting the graph  

ggraph(graph_institutions, "manual", x = x, y = y) + 
  geom_edge_arc(aes(width = weight, color = color_edges), alpha = 0.3, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,3)) +
 scale_edge_colour_identity() +
geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.8, show.legend = FALSE) +
  scale_size_continuous(range = c(0.1,20)) +
 scale_fill_identity() +
geom_label_repel(data=important_nodes, aes(x=x, y=y, label = Id, fill = color), size = 4, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  theme_void() +
 ggsave(paste0(picture_path,"graph_co-authorship_institutions_",dates[i],"-",dates[i+1],".png"), width=30, height=30, units = "cm")  


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################# 5) Saving TF-IDF data  ##################################------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

for(i in seq(1,length(dates),2)){ 
graph_coupling <- readRDS(paste0(graph_data_path,"graph_coupling_",dates[i],"-",dates[i+1],".rds"))

TF_IDF <- tf_idf(graph = graph_coupling, n_columns = 5)

saveRDS(TF_IDF, paste0(graph_data_path,"tf-idf_coupling_",dates[i],"-",dates[i+1],".rds"))

# Extracting the nodes of the network
graph_authors_coupling <- readRDS(paste0(graph_data_path,"graph_authors_coupling_",dates[i],"-",dates[i+1],".rds"))

Nodes_authors_coupling <- graph_authors_coupling %>%
  activate(nodes)%>%
  select(Id, nb_cit_author,Com_ID, Size_com, color, Community_name) %>%
  as.data.table()

# Merging with the authors table, to have the list of the authors 
Nodes_authors_coupling <- merge(Nodes_authors_coupling,authors_JEL[,c("ID_Art","Nom","Titre")], by.x = "Id", by.y = "Nom")

Nodes_authors_coupling <- merge(Nodes_authors_coupling,nodes_JEL[between(Annee_Bibliographique,dates[i],dates[i+1]) ,c("ID_Art","Annee_Bibliographique")], by = "ID_Art")

# Calculating the tf-idf for the titles in each community
TF_IDF_authors <- tf_idf(nodes = Nodes_authors_coupling, n_columns = 4)

saveRDS(TF_IDF_authors, paste0(graph_data_path,"tf-idf_authors_coupling_",dates[i],"-",dates[i+1],".rds"))

}
