#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################## PART I: LOADING PACKAGES, PATH AND DATA ####################################--------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

##################### Packages ############################################--------------

package_list <-  c("data.table","readr","magrittr","tidyverse", "ggnewscale", "igraph",
                   "tm","quanteda","tidytext","ggraph","tidygraph", "ggrepel", "vite",
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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################ PART II: NETWORK ANALYSIS #################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################ 1) Building Nodes and Edges #################################-------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

############################### Building Nodes and Edges for co-citation #######################
nodes_cocit_JEL_1990s <- edges_JEL[between(Annee_Bibliographique,1991,1999),]
#nodes_cocit_JEL_1990s <- merge(nodes_cocit_JEL_1990s, nodes_JEL[,c("ItemID_Ref","Titre","Code_Revue")], by = "ItemID_Ref", all.x = TRUE)
nodes_cocit_JEL_1990s <- unique(nodes_cocit_JEL_1990s[,nb_cit := .N, by = "New_id2"][nb_cit >= 2,c("New_id2","Annee","Nom","Titre","Revue_Abbrege","ESpecialite","nb_cit")])
doublons <- which(duplicated(nodes_cocit_JEL_1990s$New_id2))
nodes_cocit_JEL_1990s <- nodes_cocit_JEL_1990s[-doublons]

# creating a label for nodes
nodes_cocit_JEL_1990s <- nodes_cocit_JEL_1990s[, Label := paste0(Nom, Annee)]

# creating edges
edges_JEL_1990s <- unique(edges_JEL[New_id2 %in% nodes_cocit_JEL_1990s$New_id2 & between(Annee_Bibliographique,1991,1999)][, c("ID_Art","New_id2")])

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
nb_cit<- edges_JEL[between(Annee_Bibliographique,1991,1999),nb_cit := .N,ItemID_Ref]
nodes_coupling_JEL_1990s <- merge(nodes_coupling_JEL_1990s,unique(nb_cit[,c("ItemID_Ref","nb_cit")]), by = "ItemID_Ref", all.x = "TRUE")

rm(nb_cit) # not useful anymore
# replacing NA value by 0 in nb_cit (it means that the nodes will be used in Leiden and Force Atlas, but
# not displayed in the graph)

nodes_coupling_JEL_1990s[is.na(nb_cit),]$nb_cit <- 0

# reducing the number of nodes depending of the number of citations
nodes_coupling_JEL_1990s <- unique(nodes_coupling_JEL_1990s[nb_cit >= 1,c("ID_Art","Annee_Bibliographique","Nom","Label","Titre","Revue","ESpecialite","nb_cit","ItemID_Ref")])

# building edges
edges_JEL_1990s_bis <- unique(edges_JEL[ID_Art %in% nodes_coupling_JEL_1990s$ID_Art])

# creation of the edges for the co-citation network

edges_coupling_JEL_1990s <- bibliographic_coupling(edges_JEL_1990s_bis, source = "ID_Art", ref = "New_id2", weight_threshold = 1)

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
graph_cocit <- leiden_improved(graph_cocit, res_1 = 1, res_2 = 0.5, res_3 = NULL, n_iterations = 500)       

# Giving colors to communities
graph_cocit <- community_colors(graph_cocit,mypalette)

# Naming communities
graph_cocit <-naming_communities(graph_cocit, centrality_measure = "nb_cit", naming = "Label")

# Calculating different centrality measures
graph_cocit <- centrality(graph_cocit)

# Integration a size variable for implementing non-overlapping function of Force Atlast
graph_cocit <- graph_cocit %>%
  activate(nodes) %>%
  mutate(size=nb_cit)

# Running Force Atlas layout  
graph_cocit <- force_atlas(graph_cocit,seed = NULL, ew.influence = 1, kgrav = 1, iter_1 = 5000, iter_2 = 1000, barnes.hut = TRUE, size_min = 50, size_max = 200)

# Saving the graph
saveRDS(graph_cocit, paste0(graph_data_path,"graph_cocit.rds"))

############################# Projection of the graph #########################

# loading the graph if necessary
#graph_cocit <- readRDS(paste0(graph_data_path,"graph_cocit.rds"))

# Identifying the labels of the most important authors
important_nodes <- top_ordering(graph_cocit, top_n_com = 2, top_n = 10, biggest_community = TRUE)

# Plotting the graph  
graph_cocit <- graph_cocit %>%
  activate(nodes) %>%
  filter(nb_cit >= 1)

# Plotting the graph  
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
  ggsave(paste0(picture_path,"graph_cocit.png"), width=40, height=40, units = "cm")

# Identifying the labels of the most important authors
#important_nodes <- top_ordering(graph_cocit, top_n_com = 1, top_n = 10)

# Plotting the graph  
ggraph(graph_cocit, "manual", x = x, y = y) + 
  geom_edge_arc(aes(width = weight), color = "gray", alpha = 0.3, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,2)) +
  geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.8, show.legend = FALSE) +
  scale_size_continuous(range = c(0.05,16)) +
  scale_fill_identity() +
  scale_edge_colour_identity() +
  geom_label_repel(data=important_nodes, aes(x=x, y=y, label = paste0(Nom,Annee), fill = color), size = 4, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  #scale_size_continuous(range = c(1,4)) +
  theme_void() +
  ggsave(paste0(picture_path,"graph_cocit_bigcom.png"), width=40, height=40, units = "cm")

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
important_nodes <- top_ordering(graph_coupling, top_n_com = 2, top_n = 25)

# Plotting the graph  
graph_coupling <- graph_coupling %>%
  filter(nb_cit >= 2)

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
  ggsave(paste0(picture_path,"graph_coupling_test.png"), width=30, height=30, units = "cm")

##################################### Basic Statistics of Communities ###############################------

# Extracting the nodes of the network
Nodes_coupling <- graph_coupling %>%
  activate(nodes)%>%
  select(Id, Annee_Bibliographique,Label,nb_cit,Com_ID,Revue,ESpecialite) %>%
  as.data.table()

# Changing format of ID_Art to merge with other data table
Nodes_coupling$ID_Art <- as.integer(Nodes_coupling$Id)

# Calculating the size of communities and mean of citation per community (total number of citations of all the nodes on the number of nodes)
Nodes_coupling <- Nodes_coupling[, size_com := .N, by = "Com_ID"][, mean_cit_com := sum(nb_cit), by = "Com_ID"][, mean_cit_com := mean_cit_com/size_com]

# keeping only communities with at least x% of the nodes
Nodes_coupling <- Nodes_coupling[, share_com := size_com/length(Nodes_coupling$ID_Art)][share_com >= 0.01,]

# Merging with the authors table, to have the complete list
references_extended <- merge(Nodes_coupling,authors_JEL[,c("ID_Art","Nom","Titre","Ordre","Institution","Pays")], by = "ID_Art")

# Merging with the reference table
# We now have the nodes, with community information, plus the references cited by these nodes
references_extended <- merge(references_extended,edges_JEL[,c("ID_Art","New_id2","Annee","Nom")], by = "ID_Art")

# Renaming columns to avoid confusion
setnames(references_extended,c("Nom.x","Nom.y"), c("citing_author","cited_author"))

# Strategy: keeping the five highest values for different variables, per community
# fixing n
n = 5

# Keeping the n nodes with the highest number of citations in our corpus, per community
most_cited_nodes <- unique(references_extended[, c("Com_ID","nb_cit","Label","Titre")])
most_cited_nodes <- most_cited_nodes %>%
  group_by(Com_ID) %>%
  arrange(desc(nb_cit)) %>%
  slice(1:n)

# Keeping the n references the most cited per community
most_cited_ref <- references_extended[, c("Com_ID","New_id2","cited_author","Annee")]
most_cited_ref <- most_cited_ref[, share_cit_ref := .N, by = c("Com_ID","New_id2")][, nb_cit_per_com := .N, by = "Com_ID"][, share_cit_ref := share_cit_ref/nb_cit_per_com]
most_cited_ref <- most_cited_ref %>%
  select(Com_ID,New_id2,cited_author,Annee,share_cit_ref) %>%
  unique() %>%
  group_by(Com_ID) %>%
  arrange(desc(share_cit_ref)) %>%
  slice(1:n)

# keeping the n authors the most present in coupling communities
main_author <- unique(references_extended[, c("ID_Art","Com_ID","citing_author")])
main_author <- main_author[, main_author := .N, by = c("Com_ID","citing_author")]
main_author <- main_author %>%
  select(Com_ID,citing_author,main_author) %>%
  unique() %>%
  group_by(Com_ID) %>%
  arrange(desc(main_author)) %>%
  slice(1:n)

# keeping the n journals where most nodes were published in coupling communities
main_journal <- unique(references_extended[, c("ID_Art","Com_ID","Revue","size_com")])
main_journal <- main_journal[, main_journal := .N, by = c("Com_ID","Revue")][, main_journal:= main_journal/size_com]
main_journal <- main_journal %>%
  select(Com_ID,Revue,main_journal) %>%
  unique() %>%
  group_by(Com_ID) %>%
  arrange(desc(main_journal)) %>%
  slice(1:n)

# keeping the n top disciplines per community
main_discipline <- unique(references_extended[, c("ID_Art","Com_ID","ESpecialite","size_com")])
main_discipline <- main_discipline[, main_discipline := .N, by = c("Com_ID","ESpecialite")][, main_discipline := main_discipline/size_com]
main_discipline <- main_discipline %>%
  select(Com_ID,ESpecialite,main_discipline) %>%
  unique() %>%
  group_by(Com_ID) %>%
  arrange(desc(main_discipline)) %>%
  slice(1:n) %>%
  as.data.table()

main_discipline <- main_discipline[, rank := 1:.N, by = list(Com_ID)]

# keeping the n institutions the most present in coupling communities
main_institution <- unique(references_extended[, c("ID_Art","Com_ID","citing_author","Institution")])
main_institution <- main_institution[, Total := .N, by = "Com_ID"]
main_institution <- main_institution[Institution != "NA", main_institution := .N, by = c("Com_ID","Institution")][, main_institution := main_institution/Total]
main_institution <- main_institution %>%
  select(Com_ID,Institution,main_institution) %>%
  unique() %>%
  group_by(Com_ID) %>%
  arrange(desc(main_institution)) %>%
  slice(1:n)

# keeping the n institutions the most present in coupling communities
main_country <- unique(references_extended[, c("ID_Art","Com_ID","citing_author","Pays")])
main_country <- main_country[, Total := .N, by = "Com_ID"]
main_country <- main_country[Pays != "NA", main_country := .N, by = c("Com_ID","Pays")][, main_country := main_country/Total]
main_country <- main_country %>%
  select(Com_ID,Pays,main_country) %>%
  unique() %>%
  group_by(Com_ID) %>%
  arrange(desc(main_country)) %>%
  slice(1:n)

# creating a "rank" column to be merged with the data
rank <- data.table(rank=1:n)
rank <- merge(unique(references_extended$Com_ID), rank)
colnames(rank)[1] = "Com_ID"
rank <- rank %>% arrange(Com_ID)

# Merging all the data on coupling communities
coupling_com <- merge(unique(references_extended[,c("Com_ID","size_com","share_com","mean_cit_com")]), rank, by = "Com_ID")
coupling_com <- cbind(coupling_com,most_cited_nodes[, c("Label","Titre")],most_cited_ref[, c("cited_author","Annee","share_cit_ref")], 
                      main_author[, c("citing_author","main_author")], main_journal[, c("Revue","main_journal")], 
                      main_institution[,c("Institution","main_institution")], main_country[,c("Pays","main_country")])
coupling_com <- merge(coupling_com, main_discipline, by = c("Com_ID","rank"), all.x = TRUE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################# 3) Author Graph from Bibliographic Coupling ##################################------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

################## Building an author bibliographic coupling graph ######################----

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
nodes_coupling_JEL_1990s <- nodes_coupling_JEL_1990s[nb_cit >= 1]
  
# building edges
edges_JEL_1990s_bis <- unique(edges_JEL[ID_Art %in% nodes_coupling_JEL_1990s$ID_Art])

authors_edges <- merge(edges_JEL_1990s_bis,unique(nodes_coupling_JEL_1990s[,c("ID_Art","citing_author")]), by = "ID_Art", all.x = TRUE, allow.cartesian = TRUE)

authors_edges <- bibliographic_coupling(authors_edges, "citing_author", "New_id2", weight_threshold = 2)

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
graph_authors_coupling <- readRDS(paste0(graph_data_path,"graph_authors_coupling.rds"))

# Identifying the labels of the most important authors
important_nodes <- top_ordering(graph_authors_coupling, top_n_com = 2, biggest_community = TRUE)

# Plotting the graph  
graph_authors_coupling <- graph_authors_coupling %>%
  filter(nb_cit_author >= 2)

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
