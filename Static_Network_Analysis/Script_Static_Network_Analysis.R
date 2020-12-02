#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################## PART I: LOADING PACKAGES, PATH AND DATA ####################################--------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

##################### Packages, paths, etc. ############################################--------------

source("/home/aurelien/macro_AA/Static_Network_Analysis/functions_for_network_analysis.R")
source("/home/aurelien/macro_AA/Static_Network_Analysis/Script_paths_and_basic_objects.R")

##################### Loading Data ############################################--------------

# Loading the title of commmunities and associating them colors
names_coupling <- fread(paste0(graph_data_path,"names_coupling.csv")) %>% data.table()
names_coupling <- names_coupling[, Com_ID := gsub("\"\"","",Com_ID)]
names_authors_coupling <- fread(paste0(graph_data_path,"names_authors_coupling.csv")) %>% data.table()
names_authors_coupling <- names_authors_coupling[, Com_ID := gsub("\"\"","",Com_ID)]
names_cocit <- fread(paste0(graph_data_path,"names_cocit.csv")) %>% data.table()
names_cocit <- names_cocit[, Com_ID := gsub("\"\"","",Com_ID)]


names_com <- rbind(names_coupling[,-"Comments"],names_authors_coupling,names_cocit)
names_com <- unique(names_com[order(Titre_provisoire),"Titre_provisoire"])
names_com <- cbind(names_com,mypalette[1:length(names_com$Titre_provisoire)])

names_authors_coupling <- merge(names_authors_coupling,names_com, by = "Titre_provisoire")
names_coupling <- merge(names_coupling,names_com, by = "Titre_provisoire")
names_cocit <- merge(names_cocit,names_com, by = "Titre_provisoire")


#setnames(names_com,c("Titre_provisoire","V2"),c("Community_name","color"))

######################### Fixing the sub-periods and thresholds ##########################################------------

start_date <- c(1970,1977,1984,1991,1997,2003)
end_date <- c(1976,1983,1990,1996,2002,2008)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################ PART II: NETWORK ANALYSIS #################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################ 1) Bibliographic Co-citation #################################-------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

######################### Working on the tidygraph for cocitation and its attributes #################################-------
for(i in 1:length(start_date)){
graph_cocit <- readRDS(paste0(graph_data_path,"prior_graph_cocit_",start_date[i],"-",end_date[i],".rds"))

# Identifying communities with Leiden algorithm                         
graph_cocit <- leiden_improved(graph_cocit, res_1 = 1, res_2 = NULL, res_3 = NULL, n_iterations = -1)       

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
graph_cocit <- force_atlas(graph_cocit,seed = 1, ew.influence = 1, kgrav = 1, iter_1 = 8000, iter_2 = 800, barnes.hut = TRUE, size_min = 50, size_max = 200)

# Saving the graph
saveRDS(graph_cocit, paste0(graph_data_path,"graph_cocit_",start_date[i],"-",end_date[i],".rds"))

}

################################### Recoloring graphs depending on the new titles ##########################

for(i in 1:length(start_date)){
  # loading the graph if necessary
  graph_cocit <- readRDS(paste0(graph_data_path,"graph_cocit_",start_date[i],"-",end_date[i],".rds"))
  
  # Adding new titles and new colors
  graph_cocit <- graph_cocit %>%
    left_join(names_cocit[start_date == start_date[i],c("Com_ID","Titre_provisoire","V2")]) %>%
    mutate(Community_name = Titre_provisoire, 
           color = V2)
  #Mix color for edges of different color
  graph_cocit <- graph_cocit %>% #mix color
    activate(edges) %>%
    mutate(color_com_ID_to = .N()$color[to], color_com_ID_from = .N()$color[from]) %>%
    mutate(color_edges = MixColor(color_com_ID_to, color_com_ID_from, amount1 = 0.5)) 
  
  # Saving the graph
  saveRDS(graph_cocit, paste0(graph_data_path,"graph_cocit_",start_date[i],"-",end_date[i],".rds"))
  
}

############################# Projection of the graph #########################-----------

graph_cocit <- readRDS(paste0(graph_data_path,"graph_cocit_",start_date[i],"-",end_date[i],".rds"))

# Identifying the labels of the most important authors
important_nodes <- top_ordering(graph_cocit, top_n_com = 3, top_n = 20, biggest_community = TRUE)
com_label <- label_com(graph_cocit,biggest_community = TRUE)

# Plotting the graph  
graph_cocit <- graph_cocit %>%
 activate(nodes) %>%
  filter(nb_cit >= 8)


# Plotting the graph 1 - Complete graph with the biggest communities
ggraph(graph_cocit, "manual", x = x, y = y) + 
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,2)) +
  geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.8, show.legend = FALSE) +
  scale_size_continuous(range = c(0.1,14)) +
  scale_fill_identity() +
  scale_edge_colour_identity() +
  geom_text_repel(data=important_nodes, aes(x=x, y=y, label = Label), size = 2, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  geom_label_repel(data=com_label, aes(x=x, y=y, label = Community_name, fill = color), size = 5, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  theme_void() +
  ggsave(paste0(picture_path,"graph_cocit",start_date[i],"-",end_date[i],".png"), width=35, height=35, units = "cm")

#################################### Graph of community as nodes from cocit ######################################

for(i in 1:length(start_date)){
  graph_cocit <- readRDS(paste0(graph_data_path,"graph_cocit_",start_date[i],"-",end_date[i],".rds"))
  
  # Using the function we have built
  graph_cocit_community <- graph_community(graph_cocit)
  
  # Saving the graph
  saveRDS(graph_cocit_community, paste0(graph_data_path,"graph_cocit_community_",start_date[i],"-",end_date[i],".rds"))
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################# 2) Bibliographic Coupling ##################################------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

################## Working on the coupling tidygraph and its attributes ######################----
for(i in 1:length(start_date)){
graph_coupling <- readRDS(paste0(graph_data_path,"prior_graph_coupling_",start_date[i],"-",end_date[i],".rds"))
  
# Identifying communities with Leiden algorithm                         
graph_coupling <- leiden_improved(graph_coupling, res_1 = 1, res_2 = NULL, res_3 = NULL, n_iterations = -1)       

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

# Saving the graph
saveRDS(graph_coupling, paste0(graph_data_path,"graph_coupling_",start_date[i],"-",end_date[i],".rds"))

# Running Force Atlas layout  
graph_coupling <- force_atlas(graph_coupling,seed = 1, ew.influence = 1, kgrav = 1, iter_1 = 10000, iter_2 = 800, barnes.hut = TRUE, size_min = 50, size_max = 200)

# Saving the graph
saveRDS(graph_coupling, paste0(graph_data_path,"graph_coupling_",start_date[i],"-",end_date[i],".rds"))
}

############################### Recoloring Graph depending on the new titles ###############################
for(i in 1:length(start_date)){
  # loading the graph if necessary
  graph_coupling <- readRDS(paste0(graph_data_path,"graph_coupling_",start_date[i],"-",end_date[i],".rds"))
  
  # Adding new titles and new colors
  graph_coupling <- graph_coupling %>%
    left_join(names_coupling[start_date == start_date[i],c("Com_ID","Titre_provisoire","V2")]) %>%
    mutate(Community_name = Titre_provisoire, 
           color = V2)
  #Mix color for edges of different color
  graph_coupling <- graph_coupling %>% #mix color
    activate(edges) %>%
    mutate(color_com_ID_to = .N()$color[to], color_com_ID_from = .N()$color[from]) %>%
    mutate(color_edges = MixColor(color_com_ID_to, color_com_ID_from, amount1 = 0.5)) 
  
  # Saving the graph
  saveRDS(graph_coupling, paste0(graph_data_path,"graph_coupling_",start_date[i],"-",end_date[i],".rds"))
  
}
i = 1
############################# Projection of the graph #########################

# loading the graph if necessary
graph_coupling <- readRDS(paste0(graph_data_path,"graph_coupling_",start_date[i],"-",end_date[i],".rds"))

# Identifying the labels of the most important authors
important_nodes <- top_ordering(graph_coupling, top_n_com = 2, top_n = 15, biggest_community = TRUE, community_threshold = 0.02)
com_label <- label_com(graph_coupling,biggest_community = TRUE, community_threshold = 0.02)

# Plotting the graph  

graph_coupling <- graph_coupling %>%
activate(nodes) %>%
 filter(nb_cit >= 1)

ggraph(graph_coupling, "manual", x = x, y = y) + 
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,2)) +
  scale_edge_colour_identity() +
  geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.8, show.legend = FALSE) +
  scale_size_continuous(range = c(0.2,13)) +
  scale_fill_identity() +
  geom_text_repel(data=important_nodes, aes(x=x, y=y, label = Label), size = 2, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  geom_label_repel(data=com_label, aes(x=x, y=y, label = Community_name, fill = color), size = 5, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  theme_void() +
  ggsave(paste0(picture_path,"graph_coupling_",start_date[i],"-",end_date[i],".png"), width=35, height=35, units = "cm")

#################################### Graph of community as nodes from coupling ######################################

for(i in 1:length(start_date)){
graph_coupling <- readRDS(paste0(graph_data_path,"graph_coupling_",start_date[i],"-",end_date[i],".rds"))

# Using the function we have built
graph_coupling_community <- graph_community(graph_coupling)

# Saving the graph
saveRDS(graph_coupling_community, paste0(graph_data_path,"graph_coupling_community_",start_date[i],"-",end_date[i],".rds"))
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################# 3) Author Graph from Bibliographic Coupling ##################################------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

################## Working on the author coupling tidygraph and its attributes ######################----

for(i in 1:length(start_date)){
graph_authors_coupling <- readRDS(paste0(graph_data_path,"prior_graph_authors_coupling_",start_date[i],"-",end_date[i],".rds"))

# Identifying communities with Leiden algorithm                         
graph_authors_coupling <- leiden_improved(graph_authors_coupling, res_1 = 1, res_2 = NULL, res_3 = NULL, n_iterations = -1)       

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

# Saving the graph
saveRDS(graph_authors_coupling, paste0(graph_data_path,"graph_authors_coupling_",start_date[i],"-",end_date[i],".rds"))

# Running Force Atlas layout  
graph_authors_coupling <- force_atlas(graph_authors_coupling,seed = 1, ew.influence = 1, kgrav = 1, iter_1 = 8000, iter_2 = 800, barnes.hut = TRUE, size_min = 50, size_max = 200)

# Saving the graph
saveRDS(graph_authors_coupling, paste0(graph_data_path,"graph_authors_coupling_",start_date[i],"-",end_date[i],".rds"))

}

############################### Recoloring Graph depending on the new titles ###############################
for(i in 1:length(start_date)){
  # loading the graph if necessary
  graph_authors_coupling <- readRDS(paste0(graph_data_path,"graph_authors_coupling_",start_date[i],"-",end_date[i],".rds"))
  
  # Adding new titles and new colors
  graph_authors_coupling <- graph_authors_coupling %>%
    left_join(names_authors_coupling[start_date == start_date[i],c("Com_ID","Titre_provisoire","V2")]) %>%
    mutate(Community_name = Titre_provisoire, 
           color = V2)
  #Mix color for edges of different color
  graph_authors_coupling <- graph_authors_coupling %>% #mix color
    activate(edges) %>%
    mutate(color_com_ID_to = .N()$color[to], color_com_ID_from = .N()$color[from]) %>%
    mutate(color_edges = MixColor(color_com_ID_to, color_com_ID_from, amount1 = 0.5)) 
  
  # Saving the graph
  saveRDS(graph_authors_coupling, paste0(graph_data_path,"graph_authors_coupling_",start_date[i],"-",end_date[i],".rds"))
  
}
############################# Projection of the graph #########################

i = 6
# loading the graph if necessary
graph_authors_coupling <- readRDS(paste0(graph_data_path,"graph_authors_coupling_",start_date[i],"-",end_date[i],".rds"))

# Identifying the labels of the most important authors
important_nodes <- top_ordering(graph_authors_coupling, ordering_column = "nb_cit_author", top_n_com = 2, biggest_community = TRUE)
com_label <-  label_com(graph_authors_coupling,biggest_community = TRUE)

# Plotting the graph  
graph_authors_coupling <- graph_authors_coupling %>%
filter(nb_cit_author >= 1)

ggraph(graph_authors_coupling, "manual", x = x, y = y) + 
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,2)) +
  scale_edge_colour_identity() +
  geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.8, show.legend = FALSE) +
  scale_size_continuous(range = c(0.4,13)) +
  scale_fill_identity() +
  geom_text_repel(data=important_nodes, aes(x=x, y=y, label = Id), size = 2, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  geom_label_repel(data=com_label, aes(x=x, y=y, label = Community_name, fill = color), size = 5, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  theme_void() +
  ggsave(paste0(picture_path,"graph_authors_coupling_",start_date[i],"-",end_date[i],".png"), width=30, height=30, units = "cm")


################## Authors coupling graph with community as nodes ######################----

for(i in 1:length(start_date)){

graph_authors_coupling <- readRDS(paste0(graph_data_path,"graph_authors_coupling_",start_date[i],"-",end_date[i],".rds"))

# Using the function we have built
graph_authors_coupling_community <- graph_community(graph_authors_coupling)

# Saving the graph
saveRDS(graph_authors_coupling_community, paste0(graph_data_path,"graph_authors_coupling_community_",start_date[i],"-",end_date[i],".rds"))
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################ 4) Institutions network from Coupling #################################-------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

################## Working on the Institution coupling tidygraph and its attributes ######################----

for(i in 4:length(start_date)){
graph_institutions_coupling <- readRDS(paste0(graph_data_path,"prior_graph_institutions_coupling_",start_date[i],"-",end_date[i],".rds"))

# Identifying communities with Leiden algorithm                         
graph_institutions_coupling <- leiden_improved(graph_institutions_coupling, res_1 = 1.05, res_2 = NULL, res_3 = NULL, n_iterations = -1)       

# Giving colors to communities
graph_institutions_coupling <- community_colors(graph_institutions_coupling,mypalette)

# Calculating different centrality measures
#graph_authors_coupling <- centrality(graph_authors_coupling)

# Naming communities
graph_institutions_coupling <-naming_communities(graph_institutions_coupling, centrality_measure = "nb_art", naming = "Id")

# Integration a size variable for implementing non-overlapping function of Force Atlas
graph_institutions_coupling <- graph_institutions_coupling %>%
  activate(nodes) %>%
  mutate(size=nb_art)

# Running Force Atlas layout  
graph_institutions_coupling <- force_atlas(graph_institutions_coupling,seed = 1, ew.influence = 2, kgrav = 1, iter_1 = 6000, iter_2 = 2000, barnes.hut = TRUE, size_min = 50, size_max = 200)

# Saving the graph
saveRDS(graph_institutions_coupling, paste0(graph_data_path,"graph_institutions_coupling_",start_date[i],"-",end_date[i],".rds"))
}

############################# Projection of the graph #########################

i = 6
# loading the graph if necessary
graph_institutions_coupling <- readRDS(paste0(graph_data_path,"graph_institutions_coupling_",start_date[i],"-",end_date[i],".rds"))

# Identifying the labels of the most important authors
important_nodes <- top_ordering(graph_institutions_coupling, ordering_column = "nb_art", top_n_com = 3, top_n = 10, biggest_community = TRUE)
com_label <-  label_com(graph_institutions_coupling,biggest_community = TRUE)


# Plotting the graph  

ggraph(graph_institutions_coupling, "manual", x = x, y = y) + 
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,2)) +
  scale_edge_colour_identity() +
  geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.8, show.legend = FALSE) +
  scale_size_continuous(range = c(1,20)) +
  scale_fill_identity() +
  geom_text_repel(data=important_nodes, aes(x=x, y=y, label = Id), size = 2, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  geom_label_repel(data=com_label, aes(x=x, y=y, label = Community_name, fill = color), size = 5, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  theme_void() +
  ggsave(paste0(picture_path,"graph_institutions_coupling_",start_date[i],"-",end_date[i],".png"), width=30, height=30, units = "cm")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################################# 5) Co-Authorship Graph  ##################################------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

################## Working on the author coupling tidygraph and its attributes ######################----

for(i in 1:length(start_date)){
graph_coauthorship <- readRDS(paste0(graph_data_path,"prior_graph_coauthorship_",start_date[i],"-",end_date[i],".rds"))

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
saveRDS(graph_coauthorship, paste0(graph_data_path,"graph_coauthorship_",start_date[i],"-",end_date[i],".rds"))
}

############################# Projection of the graph #########################

i = 6
# loading the graph if necessary
graph_coauthorship <- readRDS(paste0(graph_data_path,"graph_coauthorship_",start_date[i],"-",end_date[i],".rds"))

# Identifying the labels of the most important authors
important_nodes <- top_ordering(graph_coauthorship, top_n_com = 1, top_n =10, biggest_community = TRUE)
com_label <-  label_com(graph_coauthorship,biggest_community = TRUE)


# Plotting the graph  

ggraph(graph_coauthorship, "manual", x = x, y = y) + 
  geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,3)) +
  scale_edge_colour_identity() +
  geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.8, show.legend = FALSE) +
  scale_size_continuous(range = c(0.1,9)) +
  scale_fill_identity() +
  geom_text_repel(data=important_nodes, aes(x=x, y=y, label = Id), size = 2, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  geom_label_repel(data=com_label, aes(x=x, y=y, label = Community_name, fill = color), size = 4, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  theme_void() +
  ggsave(paste0(picture_path,"graph_coauthorship_",start_date[i],"-",end_date[i],".png"), width=30, height=30, units = "cm")

################## Working on the institution co-authorship tidygraph and its attributes ######################----

for(i in 4:length(start_date)){
  
graph_institutions <-  readRDS(paste0(graph_data_path,"prior_graph_co-authorship_institutions_",start_date[i],"-",end_date[i],".rds"))
  
# Identifying communities with Leiden algorithm                         
graph_institutions <- leiden_improved(graph_institutions, res_1 = 0.5, res_2 = NULL, res_3 = NULL, n_iterations = 500)       
com_label <-  label_com(graph_authors_coupling,biggest_community = TRUE)

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
saveRDS(graph_institutions, paste0(graph_data_path,"graph_co-authorship_institutions_",start_date[i],"-",end_date[i],".rds"))
}

############################# Projection of the graph #########################

i = 6
# loading the graph if necessary
graph_institutions <-  readRDS(paste0(graph_data_path,"graph_co-authorship_institutions_",start_date[i],"-",end_date[i],".rds"))

# Identifying the labels of the most important authors
important_nodes <- top_ordering(graph_institutions, ordering_column ="nb_art", top_n_com = 2, top_n =15, biggest_community = TRUE)
com_label <-  label_com(graph_institutions,biggest_community = TRUE)

# Plotting the graph  

ggraph(graph_institutions, "manual", x = x, y = y) + 
  geom_edge_arc(aes(width = weight, color = color_edges), alpha = 0.3, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,3)) +
 scale_edge_colour_identity() +
geom_node_point(aes(x=x, y=y, size = size, fill = color), pch = 21, alpha = 0.8, show.legend = FALSE) +
  scale_size_continuous(range = c(0.1,20)) +
 scale_fill_identity() +
geom_text_repel(data=important_nodes, aes(x=x, y=y, label = Id), size = 2, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
  geom_label_repel(data=com_label, aes(x=x, y=y, label = Community_name, fill = color), size = 4, fontface="bold", alpha = 1, point.padding=NA, show.legend = FALSE) +
   theme_void() +
 ggsave(paste0(picture_path,"graph_co-authorship_institutions_",start_date[i],"-",end_date[i],".png"), width=20, height=20, units = "cm")  


