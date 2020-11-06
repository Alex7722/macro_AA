# Loading a package to be able to write documentation to function in a way similar to Python
# For any function with comments delineated by "#'", you can run docstring(name_of_function) to get the standard help page.
if ("docstring" %in% installed.packages()==FALSE)
{
  install.packages('docstring',dependencies = TRUE)
}
library(docstring)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################## PART I: GENERAL FUNCTIONS FOR NETWORK ANALYSIS #######################-----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################## 1) Building Nodes and Edges ########################------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

bibliographic_coupling <- function(dt, source, ref, normalized_weight_only=TRUE, weight_threshold = 1, output_in_character = TRUE)
{
  #' function for edges of bibliographic coupling
  #' 
  #' This function creates the cosine normalized edges of the bibliographic coupling network, from a direct
  #' citation data frame.
  #' 
  #' @param dt
  #' The table with citing and cited documents.
  #' 
  #' @param source
  #' the column name of the source identifiers, that is the documents that are citing. 
  #'
  #' @param ref
  #'the column name of the references that are cited.
  #'
  #' @normalized_weight_only
  #' if set to FALSE, the function returns the weights normalized by the cosine measure, 
  #' but also simply the number of shared references.
  #' 
  #' @param weight_threshold
  #' Correspond to the value of the non-normalized weights of edges. The function just keeps the edges 
  #' that have a non-normalized weight superior to the `weight_threshold`.
  #' 
  #' @param output_in_character
  #' If TRUE, the function ends by transforming the `from` and `to` columns in character, to make the
  #' creation of a tidygraph object easier.

    # Making sure the table is a datatable
    dt <- data.table(dt)
  
  # Renaming and simplifying 
  setnames(dt, c(source,ref), c("id_art", "id_ref"))
  dt <- dt[,list(id_art,id_ref)]
  setkey(dt,id_ref,id_art)
  
  # removing duplicated citations with exactly the same source and target
  dt <- unique(dt)
  
  # remove loop
  dt <- dt[source!=ref]
  
  # Removing references cited only once:
  dt <- dt[,N := .N, by = id_ref][N > 1][, list(id_art,id_ref)]
  
  # Computing how many items each citing document has (necessary for normalization later)
  id_nb_cit <-  dt[,list(nb_cit = .N),by=id_art]
  
  #Creating every combinaison of articles per references
  bib_coup <- dt[,list(Target = rep(id_art[1:(length(id_art)-1)],(length(id_art)-1):1),
                       Source = rev(id_art)[sequence((length(id_art)-1):1)]),
                 by= id_ref]
  
  # remove loop
  bib_coup <- bib_coup[Source!=Target]
  
  #Calculating the weight
  bib_coup <- bib_coup[,.N,by=list(Target,Source)] # This is the number of go references
  
  # keeping edges over threshold
  bib_coup <- bib_coup[N>=weight_threshold]
  
  # We than do manipulations to normalize this number with the cosine measure
  bib_coup <-  merge(bib_coup, id_nb_cit, by.x = "Target",by.y = "id_art" )
  setnames(bib_coup,"nb_cit", "nb_cit_Target")
  bib_coup <-  merge(bib_coup, id_nb_cit, by.x = "Source",by.y = "id_art" )
  setnames(bib_coup,"nb_cit", "nb_cit_Source")
  bib_coup[,weight := N/sqrt(nb_cit_Target*nb_cit_Source)] # cosine measure
  
  # Renaming columns
  setnames(bib_coup, c("N"), 
           c("nb_shared_references"))
  
  # Transforming the Source and Target columns in character (and keeping the Source and Target in copy)
  # Then selection which columns to return
  if(output_in_character == TRUE){
    bib_coup$from <- as.character(bib_coup$Source)
    bib_coup$to <- as.character(bib_coup$Target)
    if(normalized_weight_only==TRUE){
      return (bib_coup[, c("from","to","weight","Source","Target")])  
    } else { 
      return (bib_coup[, c("from","to","weight","nb_shared_references","Source","Target")]) 
    }
  }
  else{
    if(normalized_weight_only==TRUE){
      return (bib_coup[, c("Source","Target","weight")])  
    } else { 
      return (bib_coup[, c("Source","Target","weight","nb_shared_references")]) 
    }
  }

}

bibliographic_cocitation <- function(dt, source, ref, normalized_weight_only=TRUE, weight_threshold = 1, output_in_character = TRUE)
{
  #' function for creating the edges of bibliographic cocitation
  #' 
  #' This function creates the cosine normalized edges of the bibliographic cocitation network, from a direct
  #' citation data frame.
  #' 
  #' @param dt
  #' The table with citing and cited documents.
  #' 
  #' @param source
  #' the column name of the source identifiers, that is the documents that are citing. 
  #'
  #' @param ref
  #'the column name of the references that are cited.
  #'
  #' @param normalized_weight_only
  #' if set to FALSE, the function returns the weights normalized by the cosine measure, 
  #' but also simply the number of shared references.
  #' 
  #' @param weight_threshold
  #' Correspond to the value of the non-normalized weights of edges. The function just keeps the edges 
  #' that have a non-normalized weight superior to the `weight_threshold`.
  #' 
  #' @param output_in_character
  #' If TRUE, the function ends by transforming the `from` and `to` columns in character, to make the
  #' creation of a tidygraph object easier.
  
  # Making sure the table is a datatable
  dt <- data.table(dt)
  
  # Renaming and simplifying 
  setnames(dt, c(source,ref), c("id_art", "id_ref"))
  dt <- dt[,list(id_art,id_ref)]
  setkey(dt,id_ref,id_art)
  
  # removing duplicated citations with exactly the same source and target
  dt <- unique(dt)
  
  # remove loop
  dt <- dt[source!=ref]
  
  # Removing documents with only one reference:
  dt <- dt[, N := .N, by = id_art][N > 1][, list(id_art,id_ref)]
  
  # Computing how many items each cited document has (necessary for normalization later)
  id_nb_cit <-  dt[,list(nb_cit = .N),by=id_ref]
  
  #Creating every combinaison of articles per references
  bib_cocit <- dt[,list(Target = rep(id_ref[1:(length(id_ref)-1)],(length(id_ref)-1):1),
                       Source = rev(id_ref)[sequence((length(id_ref)-1):1)]),
                 by= id_art]
  
  
  # remove loop
  bib_cocit <- bib_cocit[Source!=Target]
  
  # counting the number of identical links across citing articles
  bib_cocit <- bib_cocit[,.N, .(Source,Target)]
  # keeping edges over threshold
  bib_cocit <- bib_cocit[N>=weight_threshold]
  
  # We then do manipulations to normalize this number with the cosine measure
  bib_cocit <-  merge(bib_cocit, id_nb_cit, by.x = "Target",by.y = "id_ref" )
  setnames(bib_cocit,"nb_cit", "nb_cit_Target")
  bib_cocit <-  merge(bib_cocit, id_nb_cit, by.x = "Source",by.y = "id_ref" )
  setnames(bib_cocit,"nb_cit", "nb_cit_Source")
  bib_cocit[,weight := N/sqrt(nb_cit_Target*nb_cit_Source)] # cosine measure
  
  # Renaming columns
  setnames(bib_cocit, c("N"), 
           c("nb_shared_references"))
  
  # Transforming the Source and Target columns in character (and keeping the Source and Target in copy)
  if(output_in_character == TRUE){
    bib_cocit$from <- as.character(bib_cocit$Source)
    bib_cocit$to <- as.character(bib_cocit$Target)
    if(normalized_weight_only==TRUE){
      return (bib_cocit[, c("from","to","weight","Source","Target")])  
    } else { 
      return (bib_cocit[, c("from","to","weight","nb_shared_references","Source","Target")]) 
    }
  }
  else{
    if(normalized_weight_only==TRUE){
      return (bib_cocit[, c("Source","Target","weight")])  
    } else { 
      return (bib_cocit[, c("Source","Target","weight","nb_shared_references")]) 
    }
  }
  
  # Selecting which columns to return
 # if(normalized_weight_only==TRUE){
#    return (bib_cocit[, c("from","to","weight","Source","Target")])  
#  } else { 
#    return (bib_cocit[, c("from","to","weight","nb_shared_references")]) 
#  }
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################# 2) Building graphs - Basics ############################---------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

tbl_main_components  <- function(edges, nodes, directed = FALSE, node_key = NULL, nb_components = 1, threshold_alert = 0.05){
  #' Main component tidygraph from edges and nodes
  #' 
  #' A function which i) creates a tidygraph graph; ii) keeps the main components of the graph; and iii) warns
  #' the user if the first biggest component removed is too large.
  #' 
  #' @param edges
  #' A dataframe with a list of links between nodes, under the columns "from" and "to". The two columns should
  #' be characters.
  #' 
  #' @param nodes
  #' A dataframe with a list of nodes. The first column will be used as the identifying column. 
  #' Be careful to avoid doublons in the first column. The first column should be characters.
  #' 
  #' @param directed
  #' By default, the graph is computed as a non-directed graph.
  #' 
  #' @param node_key
  #' The name of the identifying column in the nodes dataframe.
  #' 
  #' @param nb_components
  #' By default, the function takes the main component of the graph (nb_components = 1). However it is possible to take as 
  #' many components as you wish Component 1 is the largest one, component 2 is the second one, etc.
  #' 
  #' @param threshold_alert
  #' If the biggest component after the last one selection (by default, nb_component = 1) gathers more than x% (by default, 5%) of the total number of nodes,
  #' the function triggers a warning to inform the user that he has removed a big component of the network.
  #' 
  #' @details 
  #' The function will automatically rename the first column of nodes as "Id".
 
  # creating the tidygraph object
   graph <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE, node_key = node_key)
  
  # attributing a number to the different components (1 is the biggest components)
  graph <- graph %>% 
    activate(nodes) %>% 
    mutate(components_att = group_components(type = "weak")) %>% 
    rename_at( 1, ~"Id" ) # renamed the first column to a standard format
  
# looking at the biggest component just after the last one we have kept
  threshold <- graph %>% 
    filter(components_att== nb_components + 1) %>% 
    rename_at( 1, ~"Id" ) 

# looking at the percentage of nodes in the biggest component just after the last one we have kept
  # trigger a warning if superior to the threshold_alert
  if(length(V(threshold)$Id)/length(V(graph)$Id) > threshold_alert)warning(paste0("Warning: you have removed a component gathering more than ",threshold_alert,"% of the nodes"))

# keeping only the number of components we want
  graph <- graph %>% 
    filter(components_att== nb_components) %>%
    select(-components_att) # we remove the column as it won't be useful after that
}

components_distribution  <- function(edges, nodes, directed = FALSE, node_key = NULL){
  #' Distribution of nodes in components
  #' 
  #' A function which creates the graph from nodes and edges and gives a table with the percentage of nodes in each component.
  #' 
  #' @param edges
  #' A dataframe with a list of links between nodes, under the columns "from" and "to". The two columns should
  #' be characters.
  #' 
  #' @param nodes
  #' A dataframe with a list of nodes. The first column will be used as the identifying column. 
  #' Be careful to avoid doublons in the first column. The first column should be characters.
  #' 
  #' @param directed
  #' By default, the graph is computed as a non-directed graph.
  #' 
  #' @param node_key
  #' The name of the identifying column in the nodes dataframe.
  #' 
  #' @section Future improvements
  #' Integrate the function directly in the `tbl_main_components` function, as a second object. Imply
  #' to find how to return two objects with a function.
  
  # creating the tidygraph object
  graph <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE, node_key = node_key)
  
  # attributing a number to the different components (1 is the biggest components)
  Components_data <- graph %>% 
    activate(nodes) %>% 
    mutate(components_att = group_components(type = "weak")) %>% 
    rename_at( 1, ~"Id" )  %>% 
    as.data.table() 
  
  Components_data <- Components_data[, N_nodes_components := .N, by = "components_att"][, N_nodes_total := .N][, percentage_nodes_components := N_nodes_components/N_nodes_total*100]
  Components_data <- unique(Components_data[order(components_att),c("components_att","N_nodes_components","percentage_nodes_components")])
}

# Running Leiden for two different resolutions and associating edges to communities
leiden_improved <- function(graph, res_1 = 1, res_2 = NULL, res_3 = NULL, n_iterations = 500){
  #' Add Leiden Communities to graph
  #' 
  #' @description This function takes as input a tidygraph object. It then runs the Leiden detection community algorithm which
  #' creates a partition. The function then associates each node to its corresponding community number. It also
  #' creates a community attribute for edges: to each edge is associated a corresponding community number, if the two nodes connected
  #' by the edge belong to the same community. If nodes have a different community, the edge takes as attribute the total 
  #' number of communities plus 1.  
  #' @description The function could be run for 1, 2 or 3 different resolution values of the Leiden algorithm. It enables
  #' communities partitions comparison when we vary the resolution. A low resolution means fewer communities. For instance, if 
  #' the second resolution is smaller than the first one, we can observe how decreasing the resolution led some communities to disappear
  #' and to be merged with other communities. Sankey diagrams enable interesting analysis of the different partitions.
  #' 
  #' @param graph A tidygraph object.
  #' @param res_1 The first resolution used for running the Leiden algorithm. 1 by default.
  #' @param res_2 The second resolution used for running the Leiden algorithm a second time. 
  #' It adds a second community attribute to nodes and edges. By default, res_2 is null and 
  #' the function just run the Leiden algorithm once (with a resolution equals to res_1).
  #' @param res_3 The third resolution used for running the Leiden algorithm a third time. 
  #' @param n_iterations Number of iterations to run the Leiden algorithm, in order to optimise
  #' the resulting partition. By default, n_iterations equals 500 which warrants a quasi-optimal
  #' partitionning. Decrease n_iterations for exploratory work, in order to decrease computation
  #' time.
  #' 
  #' @details To make plotting easier later, a zero is put before each one-digit community number (community 5 becomes 05).
  #' @details The community attribute of nodes and edges for the first resolution is called `Com_ID`. 
  #' For the second and third resolution, it is called respectively `Com_ID_2` and `Com_ID_3`.
  #' @details Attributing a community number to edges enable to give edges the same color of the nodes
  #' they are connecting, if the two nodes have the same color, or a different color than any node, if 
  #' the nodes belong to a different community.
  #' 
  #' @section Future improvements:
  #' Thinking about the attribute to give to edges for which the connected nodes belong to different communites.
  #' For instance, the attribute could equal the average of node 1 community number, and node 2 community number
  #' (for the moment, it is problematic as community number are not numeric, because of the str_pad function we use).
  #' It would enable the edge to take a colour that would be a mix of the color of Node 1 and the color of Node 2 (as 
  #' in GEPHI).
  
  # run the leiden algorithm for the first resolution
  leiden <- leiden(graph, resolution_parameter = res_1, n_iterations = n_iterations)
  
  # Add the resulting partition as an attribute of nodes
  # (to make plotting easier, put a 0 before one digit community)
  graph <- graph %>% mutate(Com_ID = sprintf("%02d", leiden)) %>% 
    mutate(Com_ID = as.character(Com_ID))
  
  # Add an attribute to edges, depending on the community of their nodes
  # (If communities are different between the two nodes, edges takes the total number of communities plus 1 as attribute)
  # (Another possibility in the future would be for edges with nodes from different communities to be the average of the 
  # two communities number. It would allow the edges to take as color the mix of the two communities color)
  graph <- graph %>% 
    activate(edges) %>%
    mutate(com_ID_to = .N()$Com_ID[to], com_ID_from = .N()$Com_ID[from], Com_ID = ifelse(com_ID_from == com_ID_to, com_ID_from,(max(leiden)+1))) # .N() makes the node data available while manipulating edges
  
  # Doing the same for the second resolution
  if(is.null(res_2)){
    return(graph)
  }
  if(!is.null(res_2)){
  leiden_2 <- leiden(graph, resolution_parameter = res_2, n_iterations = n_iterations)
  graph <- graph %>%
    activate(nodes) %>%
    mutate(Com_ID_2 = sprintf("%02d", leiden)) %>% 
    mutate(Com_ID_2 = as.character(Com_ID))
  
  graph <- graph %>% 
    activate(edges) %>%
    mutate(Com_ID_2_to = .N()$Com_ID_2[to], Com_ID_2_from = .N()$Com_ID_2[from], Com_ID_2 = ifelse(Com_ID_2_from == Com_ID_2_to, Com_ID_2_from,(max(leiden_2)+1)))
  }
  
  # Doing the same for the third resolution
  if(is.null(res_3)){
    return(graph)
  }
  if(!is.null(res_3)){
    leiden_3 <- leiden(graph, resolution_parameter = res_3, n_iterations = n_iterations)
    graph <- graph %>%
      activate(nodes) %>%
      mutate(Com_ID_3 = sprintf("%02d", leiden)) %>% 
      mutate(Com_ID_3 = as.character(Com_ID))
    
    graph <- graph %>% 
      activate(edges) %>%
      mutate(Com_ID_3_to = .N()$Com_ID_3[to], Com_ID_3_from = .N()$Com_ID_3[from], Com_ID_3 = ifelse(Com_ID_3_from == Com_ID_3_to, Com_ID_3_from,(max(leiden_3)+1)))
  }
}

# function for integrating the color to the communities
community_colors <- function(graph, palette){
  #' Creates color attribute depending on communities
  #' 
  #' @description This function takes as an input a graph, with a column for nodes and edges called `Com_ID`
  #' and attribute to each community a color. If the two nodes connected by an edge have a different community,
  #' the function mixes the color of the two communities.
  #' 
  #' @param graph A tidygraph object.
  #' 
  #' @param palette The palette to be used for attributing colors to communities.
  #' 
  #' @section Future improvements
  #' Adding a feature to use any name for the community column, rather than uniquely "Com_ID". 
  #' Adding a feature to give color to different resolution communities (if we have used two or three
  #' resolution, for having a different number of communities).

#Setup Colors
color = data.table(
  Com_ID = 1:500,
  color = palette)
color<-color %>%  mutate(Com_ID = sprintf("%02d", Com_ID)) %>% mutate(Com_ID = as.character(Com_ID))

#Add color to nodes
graph <- graph %>% 
  activate(nodes) %>% 
  left_join(color)
#Mix color for edges of different color
graph <- graph %>% #mix color
  activate(edges) %>%
  mutate(color_com_ID_to = .N()$color[to], color_com_ID_from = .N()$color[from]) %>%
  mutate(color_edges = MixColor(color_com_ID_to, color_com_ID_from, amount1 = 0.5)) 
}

# Computing different centrality stats
centrality <- function(graph){
  #' Centrality statistics
  #' 
  #' This functions compute for each node different centrality statistics: Degree, Strength,
  #' Closeness, Eigen centrality and Betweeness.
  #' 
  #' @param graph A tidygraph object.
  #' 
  #' @details The weights attribute in the edges part of the tidygraph object has to be called `weight`.
  #' 
  #' @section Future improvements:
  #' Fixing a list of the centrality statistics we want to compute. For instance, among a list
  #' of 10 centrality statistics, we choose the three we are interested in.
  graph <- graph %>% 
    activate(nodes) %>% 
    mutate(Degree = centrality_degree(),
           Strength = centrality_degree(weights = weight),
           Closeness = centrality_closeness(weights = weight, normalized = TRUE),
           Eigen_centrality = centrality_eigen(weights = weight),
           Betweeness = centrality_betweenness())
  }

naming_communities <- function(graph, centrality_measure = "Strength",naming = "label"){
  #'Naming communities
  #'
  #' @description A function to give to a community the name of its node with the highest chosen measure.
  #' It also gives the edges the name of their community. If the edge connects nodes from
  #' different community, name will be NA.
  #' 
  #' @description The function takes into account the parameters chosen for the `leiden_improved()` function. 
  #' If you have chosen 2 or 3 levels of resolution, it repeats the same process for the second and third resolution.
  #' In other words, for 3 levels of resolution, you will have the names of the communities for the first value of the 
  #' Leiden resolution, but also the names for the second and third values.
  #' 
  #' 
  #' @param graph A tidygraph object.
  #' @param centrality_measure Enter the name of the column (between quotation marks) you want to be used to choose 
  #' the community name. For instance, if you choose `Degree`, the function takes the name of the node with the highest 
  #' degree in the community to name the community. By default, takes the column named `Strength`. You can use other measure
  #' than network centrality measures: for instance, if nodes are articles, you can use the number of citations of articles.
  #' @param naming Enter the name of the column you want to be used for naming the community. The function takes the node
  #' with the highest `centrality_measure` chosen, and use the node value in the `naming` column  to title the community.
  #' For instance, if nodes are individuals and if you have a column called `surname`, you can use this column.
  #' 
  #' @details The nodes side of the tidygraph object has to have a column called `Strength`, and 
  #' `label`, the latter being the way we want to call the nodes (for instance, the surname of the 
  #' individual, if nodes are individuals).
  #' 
  #' @details The attribute of nodes and edges with the names of the communities is called
  #' `Community_name`. If you have entered a second and a third resolutions values in the 
  #' `leiden_improved()` function, you will have two supplementary columns: `Community_2_name`
  #' and `Community_3_name`. 
  #' 

  # Finding the nodes with the highest strength per community and building a df with community numbers
  # and the label of the node with the highest Strength.
   Community_names <- graph %>%
     activate(nodes)  %>%
     rename(Centrality = centrality_measure,
            Label = naming) %>%
     as_tibble()  %>%
    arrange(Com_ID, desc(Centrality)) %>%
    mutate(Community_name = Label) %>%
    select(Community_name, Com_ID) %>%
    group_by(Com_ID) %>%
    slice(1)
  
  # adding the name as an attribute to the nodes.
  graph <- graph %>%
    activate(nodes) %>%
    inner_join(Community_names, by = "Com_ID")
  
  # adding the name as an attribute to the edges
  graph <- graph %>%
    activate(edges) %>%
    left_join(Community_names, by = "Com_ID")
  
  # Reproducing the same operation for Com_ID_2 if it exists
  if(!is.null(V(graph)$Com_ID_2)){ 
  Community_names <- graph %>%
    activate(nodes) %>%
    rename(Centrality = centrality_measure,
           Label = naming) %>%
    as_tibble()  %>%
    arrange(Com_ID, desc(Centrality)) %>%
    mutate(Community_2_name = Label) %>%
    select(Community_2_name, Com_ID_2) %>%
    group_by(Com_ID_2) %>%
    slice(1)
  
  graph <- graph %>%
    activate(nodes) %>%
    inner_join(Community_names, by = "Com_ID_2")
  
  graph <- graph %>%
    activate(edges) %>%
    left_join(Community_names, by = "Com_ID_2")
}
  
  # Reproducing the same operation for Com_ID_3 if it exists
if(!is.null(V(graph)$Com_ID_3)){ 
  Community_names <- graph %>%
    activate(nodes) %>%
    rename(Centrality = centrality_measure,
           Label = naming) %>%
    as_tibble()  %>%
    arrange(Com_ID, desc(Centrality)) %>%
    mutate(Community_3_name = Label) %>%
    select(Community_3_name, Com_ID_3) %>%
    group_by(Com_ID_3) %>%
    slice(1)

graph <- graph %>%
  activate(nodes) %>%
  inner_join(Community_names, by = "Com_ID_3")

graph <- graph %>%
  activate(edges) %>%
  left_join(Community_names, by = "Com_ID_3")
}
}


# Force Atlas Function
force_atlas <- function(graph,seed = NULL, ew.influence = 1, kgrav = 1, iter_1 = 5000, iter_2 = 200, barnes.hut = FALSE, size_min = 10, size_max = 50){
  #' Force Atlas 2 algorithm for the graph (without overlap)
  #' 
  #' This function automatized the use of Force Atlas 2 from package `vite`: the FA algorithm is run a first time,
  #' without taking account of overlapping nodes. Nodes coordinates are integrated in the graph 
  #' object as attributes. FA is run a second time, this time taking account of overlapping nodes.
  #' New coordinates are integrated to the graph object. 
  #' 
  #' @param seed Set a seed to have the same coordinates if we run the function several times. 
  #' Parameters of the layout_forceatlas2(). NULL by default.
  #' @param ew.influence Package `vite` argument: Edge weight influence. The edge weights are set to 
  #' edge.weight ^ ew.influence before the calculation (see original ForceAtlas2 publication)
  #' @param kgrav Package `vite` argument: The gravity constant. Higher values will result in more compact graphs (see original ForceAtlas2 publication)
  #' @param iter_1 Maximum number of iterations for the first run of Force Atlast (without taking care of 
  #' overlapping). The algorithm will stop after this many iterations.
  #' @param iter_2  Maximum number of iterations for the second run of Force Atlast (taking care of 
  #' overlapping). The algorithm will stop after this many iterations.
  #' @param barnes.hut Package `vite` argument: Whether to use the Barnes-Hut approximation for speeding up the calculations when dealing with large graphs. 
  #' This option is automatically set to true when the graph has more than 2000 nodes.
  #' @param size_min Minimum value for normalization of the size of nodes to enter in the FA algorithm to avoid overlapping. 10 by default, as in 
  #' the original algorithm (see `details`). 
  #' @param size_max Maximum value for normalization of the size of nodes to enter in the FA algorithm to avoid overlapping.
  #' 
  #' 
  #' @details To avoid overlapping, the FA algorithm takes into account the size of the nodes. Without any 
  #' `size` attribute, the algorithm takes 10 as default value. If one displays the network with small and big nodes,
  #' there are chances that medium and big nodes are still overlapped. Thus, nodes should have an attribute called
  #' `size`. Then, sizes are normalized according to a chosen minimum value (`size_min`) and a chosen maximum 
  #' value `size_max`. The values chosen should depend on the scaling of nodes in the final plotting of the network.
  #' 
  #' @details The highest the number of iterations, the longest the computation. For the second run of FA, one 
  #' can choose a smaller number of iterations, as changes of coordinates are minor (just removing overlapping).
  #' 
  #' @section Future improvements:
  #' Modify the function for choosing the content of the column `size` in the function rather than 
  #' before (Problems with renaming column in tidygraph at this point).
  #' 
  #' @section Future improvements:
  #' The size_min/size_max works with many tatonnements for the moment. It leads to very different results
  #' depending on the number of nodes in the network. Need to think about ways to make it smoother and more 
  #' efficient.
  #' 
  #' @references 
  #' The code for the original force atlas function in package `vite`: https://rdrr.io/github/ParkerICI/vite/src/R/forceatlas2.R
  #' 
  #' @references 
  #' The original reference of the algorithm: Jacomy M1, Venturini T, Heymann S, Bastian M. ForceAtlas2, a continuous graph layout algorithm for handy network visualization designed for the Gephi software. PLoS One. 2014 Jun 10;9(6):e98679. doi: 10.1371/journal.pone.0098679 

  # Adding a size variable for the avoid.overlapping with force atlas

  graph <- graph %>%
    activate(nodes) %>%
    mutate(size = ((size - min(size))/(max(size) - min(size)))*(size_max - size_min) + size_min)
  
  # running FA for the first time (without prevent.overlap)
  set.seed(seed)
  fa  <- layout_forceatlas2(graph, ew.influence = ew.influence, kgrav = kgrav, iter = iter_1,
                                          prevent.overlap = FALSE, fixed = NULL, stopping.tolerance = 0.001,
                            barnes.hut = barnes.hut)
  
  fa  <- as.data.table(fa$lay)
  graph  <- graph %>% 
    activate(nodes) %>% 
    mutate(x = fa$x, y = fa$y)
  
  # Force Atlas 2 algorithm for the graph (with prevent.overlap)
  set.seed(seed)
  fa  <- layout_forceatlas2(graph, ew.influence = ew.influence, kgrav = kgrav, iter = iter_2,
                            prevent.overlap = TRUE, fixed = NULL, stopping.tolerance = 0.001,
                            barnes.hut = barnes.hut)
  
  fa  <- as.data.table(fa$lay)
  graph  <- graph %>% 
    activate(nodes) %>% 
    mutate(x = fa$x, y = fa$y)
}

# Simple functions for keeping a number n of nodes with highest citations measures per communities
top_citations <- function(graph, top_n = 20, top_n_com = 1){
  #' Displaying the highest cited nodes
  #' 
  #' A simple functions for keeping a number n of nodes with highest citations 
  #' per communities and a number m of nodes with highest citations in general.
  #' 
  #' @param graph
  #' A tidygraph object.
  #' 
  #' @param top_n
  #' The number of highest cited nodes in general to display.
  #' 
  #' @param top_n_com
  #' The number of highest cited nodes per community to display.
  #' 
  #' @details 
  #' Works only if you have a column called `nb_cit`
  #' 
  #' @section Future improvements
  #' Finding a way to name the column in the arguments of the function. The problem here
  #' is that you need to put the column name between quotation marks in the function, but
  #' you have to use it without quotation mark in the function.
  
  # Most citing nodes per community
  top_citations_com <- graph %>%
    activate(nodes) %>%
    as_tibble() %>%
    arrange(desc(nb_cit)) %>%
    group_by(Com_ID) %>% 
    slice(1:top_n_com) %>%
    as.data.table()
  
  # Most citing nodes in general 
  top_citations_general <- graph %>%
    activate(nodes) %>%
    as_tibble() %>%
    arrange(desc(nb_cit)) %>%
    slice(1:top_n)%>%
    as.data.table()
  
  # adding the two and removing the doublons
  top_centrality_sum <- unique(rbind(top_citations_general, top_citations_com))
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################### 3) Building Graphs - Secondary and to be improved ################-------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Simple functions for keeping a number n of nodes with highest centrality measures per communities
top_centrality_com <- function(graph, centrality, top_n = 3){
  top_centrality_com <- graph %>%
    activate(nodes) %>%
    as_tibble() %>%
    arrange(desc(centrality)) %>%
    group_by(Com_ID) %>% 
    slice(1:top_n)
  
  top_centrality_com <- graph %>%
    activate(nodes) %>%
    as_tibble() %>%
    arrange(desc(centrality)) %>%
    group_by(Com_ID) %>% 
    slice(1:top_n)
  
  top_centrality
}
# Ideally, a parameter to choose which centrality measure to apply

# A function to build a list of nodes with highest values for different centrality measures
important_nodes <- function(graph, top_n = 3){
    strength_range <- as_tibble(graph) %>%
    arrange(desc(Strength)) %>%
      group_by(Com_ID) %>% 
      slice(1:top_n) 

    betweeness_range <-  as_tibble(graph) %>%
      arrange(desc(Betweeness)) %>% 
      group_by(Com_ID) %>% 
      slice(1:top_n)

    centrality_range <- as_tibble(graph) %>%
      arrange(desc(Eigen_centrality)) %>% 
      group_by(Com_ID) %>% 
      slice(1:top_n)

    closeness_range <- as_tibble(graph) %>%
      arrange(desc(Closeness)) %>% 
       group_by(Com_ID) %>% 
       slice(1:top_n)

# merging centrality/degree/betweeness
Important_nodes <- rbind(strength_range, betweeness_range)
Important_nodes <- rbind(Important_nodes, centrality_range)
Important_nodes <- rbind(Important_nodes, closeness_range)
Important_nodes <- unique(Important_nodes)
}
# ideally an aggregation of the top_centrality_com function for different centrality measure

# A fonction to concentrate nodes of the same community in one singular node and build the corresponding graph
graph_community <- function(graph, nb_components = 1){
  # switching the names of nodes by the name of their community
  # calculating the number of nodes in the community
  # keeping only relevant attributes  
  Nodes_Com <- graph %>% 
  activate(nodes) %>%
  mutate(name = Community_name) %>% 
  select(name) %>% 
  group_by(name) %>%
  mutate(Size_com = n()) %>%
  as_tibble() %>%
  unique()

  # associating the two nodes of each edge with their respective community name
  # summing the weights of the similar edges
Edges_Com <- graph %>% 
  activate(edges) %>% 
  mutate(Com_name_from = .N()$Community_name[from], Com_name_to = .N()$Community_name[to])  %>%
  as_tibble() %>% 
  select(Com_name_from,Com_name_to,weight) %>%
  group_by(Com_name_from,Com_name_to) %>%
  mutate(weight=sum(weight)) %>%
  unique() %>%
  as.data.table()

# calculating the sum of weigths for each node (i.e. each community), that is the sum
# of the weights of all the node edges
Weight_com1 <- Edges_Com[Com_name_from != Com_name_to,c("Com_name_from","weight")]
Weight_com2 <- Edges_Com[Com_name_from != Com_name_to,c("Com_name_to","weight")]
Weight_com3 <- Edges_Com[Com_name_from == Com_name_to,c("Com_name_from","weight")]
Weight_com <- rbind(Weight_com1,Weight_com2,Weight_com3, use.names = FALSE)
Weight_com <- Weight_com[, Com_weight := sum(weight), by = "Com_name_from"]
colnames(Weight_com)[1] <- "Com_name"
Weight_com <- unique(Weight_com[,c("Com_name","Com_weight")])

# merging the two nodes of each edge with their respective total weight
Edges_Com <- merge(Edges_Com,Weight_com,by.x="Com_name_from",by.y="Com_name")
Edges_Com <- merge(Edges_Com,Weight_com,by.x="Com_name_to",by.y="Com_name")
Edges_Com <- Edges_Com[, c("Com_name_from","Com_name_to","weight","Com_weight.x","Com_weight.y")]
colnames(Edges_Com) <- c("from","to","weight","Com_weight_from","Com_weight_to")

# Cosine normalization of the edges weights
Edges_Com <- Edges_Com[, weight := weight/sqrt(Com_weight_from*Com_weight_to), by = c("from","to")]

# using our tbl_main_components function to build the tidygraph with a main component
graph_BoE_communities <- tbl_main_components(Edges_Com,Nodes_Com,nb_components = nb_components)
}
# The links between nodes(ie communities) are normalized such as not being overestimated for bigger 
# communities with more edges.


# Automatisation of a function which caculates the tf_idf values of words in titles of each community
tf_idf_per_community <- function(titles_df){
tible_tf_idf <- titles_df[,paste(title, collapse = ", "), by="Com_ID"]
tible_tf_idf[,V1 := stripWhitespace(V1)]
tible_tf_idf <- VCorpus(VectorSource(tible_tf_idf$V1))
tible_tf_idf <- tm_map(tible_tf_idf, removePunctuation)
tible_tf_idf <- tm_map(tible_tf_idf, removeNumbers)
tible_tf_idf <- tm_map(tible_tf_idf, content_transformer(tolower))
tible_tf_idf <- tm_map(tible_tf_idf, removeWords, stopwords("english"))
tible_tf_idf <- tm_map(tible_tf_idf, stemDocument)
tible_tf_idf <- DocumentTermMatrix(tible_tf_idf)
tible_tf_idf <- tidy(tible_tf_idf)

# Computing tf-idf
tf_idf_table  <-  bind_tf_idf(tible_tf_idf, term, document, count)

# uniformizing identifier of documents to merge with Com_ID
tf_idf_table$document  <- str_pad(tf_idf_table$document,2, pad = "0")

# merging with the table with the number of the community and its name (Com_ID and Community_name)
tf_idf_table  <- merge(tf_idf_table,unique(titles_df[,c("Com_ID","Community_name")]), by.x="document",by.y="Com_ID")
}
# In this function, you should enter your data frame with the titles of articles, their Com_ID, and the
# name of this community. The ouptut is a tidy object with tf-idf values that you can plot (output should 
# not be the same as input)