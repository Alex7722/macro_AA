#################################################################################
############################## Loading Packages and paths ####################################--------------
#########################################################################################

##################### Packages #########################################--------------

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

######################### Paths and data ##########################################

data_path <- "/projects/data/alexandre/Macro/Networks/"
picture_path <- "/home/aurelien/macro_AA/Static_Network_Analysis/"

source("/home/aurelien/macro_AA/Static_Network_Analysis/functions_for_network_analysis.R")

mypalette <- c("#1969B3","#01A5D8","#DA3E61","#3CB95F","#E0AF0C","#E25920","#6C7FC9","#DE9493","#CD242E","#6F4288","#B2EEF8","#7FF6FD","#FDB8D6","#8BF9A9","#FEF34A","#FEC57D","#DAEFFB","#FEE3E1","#FBB2A7","#EFD7F2","#5CAADA","#37D4F5","#F5779B","#62E186","#FBDA28","#FB8F4A","#A4B9EA","#FAC2C0","#EB6466","#AD87BC","#0B3074","#00517C","#871B2A","#1A6029","#7C4B05","#8A260E","#2E3679","#793F3F","#840F14","#401C56","#003C65","#741A09","#602A2A","#34134A","#114A1B","#27DDD1","#27DD8D","#4ADD27","#D3DD27","#DDA427","#DF2935","#DD27BC","#BA27DD","#3227DD","#2761DD","#27DDD1")


######################################################################################
#######################################################################################
################################ Network Analysis #################################
###################################################################################
###################################################################################"

nodes_cocit_1980s <- fread(paste0(data_path, "nodes_BE_extended_cocit_00.csv"))
edges_cocit_1980s <- fread(paste0(data_path, "edges_BE_extended_cocit_00.csv"))

# Temporary: transforming identifying columns in character to avoid problems with tidygraph
nodes_cocit_1980s$Id <- as.character(nodes_cocit_1980s$Id)
edges_cocit_1980s$from <- as.character(edges_cocit_1980s$Source)
edges_cocit_1980s$to <- as.character(edges_cocit_1980s$Target)
colnames(edges_cocit_1980s)[colnames(edges_cocit_1980s)=="Weight"] <- "weight" 


# creating the tidygraph object  
graph_cocit <- tbl_main_components(edges = edges_cocit_1980s, nodes = nodes_cocit_1980s, node_key = "Id", threshold_alert = 0.05)

# Optionnal: studying the distribution of nodes in each component of the graph
  #components <- components_distribution(edges = edges_cocit_1980s, nodes = nodes_cocit_1980s, node_key = "Id")

# Identifying communities with Leiden algorithm                         
graph_cocit <- leiden_improved(graph_cocit, res_1 = 1, res_2 = NULL, res_3 = NULL, n_iterations = 500)       

# Giving colors to communities
graph_cocit <- community_colors(graph_cocit,mypalette)

# Calculating different centrality measures
graph_cocit <- centrality(graph_cocit)

# Integration a size variable for implementing non-overlapping function of Force Atlast
graph_cocit <- graph_cocit %>%
  activate(nodes) %>%
  mutate(size=N_citations)

# Running Force Atlas layout  
graph_cocit <- force_atlas(graph_cocit,seed = NULL, ew.influence = 1, kgrav = 1, iter_1 = 4000, iter_2 = 200, barnes.hut = TRUE, size_min = 50, size_max = 200)

# Identifying the labels of the most important authors
important_nodes <- top_centrality_com(graph_cocit, top_n = 3)

# Plotting the graph  
ggraph(graph_cocit, "manual", x = x, y = y) + 
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,2)) +
 # scale_edge_colour_manual(values = c(mypalette[1:as.numeric(max(V(graph_cocit)$Com_ID))], alpha("grey",0.2)))+
  geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.8, show.legend = FALSE) +
  scale_size_continuous(range = c(0.1,8)) +
  new_scale("size") +
  scale_fill_identity() +
  scale_edge_colour_identity() +
  geom_text_repel(data=important_nodes, aes(x=x, y=y, label = Label, size=size), fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  scale_size_continuous(range = c(1,4)) +
  theme_void() +
  ggsave(paste0(picture_path,"graph_cocit.png"), width=50, height=35, units = "cm")

