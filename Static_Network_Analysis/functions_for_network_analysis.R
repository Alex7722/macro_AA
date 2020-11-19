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
  #' @description The function also automatically calculates the percentage of total nodes that are gathered in each 
  #' community, in the column `Size_com`. This calculation is only done for `Com_ID`, that is for the resolution equals to
  #' 1.
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
  graph <- graph %>%  activate (nodes) %>% 
    mutate(Com_ID = sprintf("%02d", leiden)) %>% 
    mutate(Com_ID = as.character(Com_ID))
  
  # calculate the size of the community
  graph <- graph %>% group_by(Com_ID) %>%
    mutate(Size_com = n()) %>%
    mutate(Size_com = Size_com / length(V(graph)$Com_ID)) %>%
    ungroup()
    
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

naming_communities <- function(graph, centrality_measure = "Strength", naming = "Label", Community = "Com_ID"){
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
  #' 
  #' @param naming Enter the name of the column you want to be used for naming the community. The function takes the node
  #' with the highest `centrality_measure` chosen, and use the node value in the `naming` column  to title the community.
  #' For instance, if nodes are individuals and if you have a column called `surname`, you can use this column.
  #' 
  #' @param Community
  #' The name of your community column.
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
    as_tibble()
  
  # Changing the name of the variable chosen
  colnames(Community_names)[colnames(Community_names) == naming] = "Label"
  colnames(Community_names)[colnames(Community_names) == Community] = "Com_ID"
  colnames(Community_names)[colnames(Community_names) == centrality_measure] = "Centrality"
  
  Community_names <- Community_names %>%
    arrange(Com_ID, desc(Centrality)) %>%
    mutate(Community_name = Label) %>%
    select(Community_name, Com_ID) %>%
    group_by(Com_ID) %>%
    slice(1) %>%
    mutate(Community_name = paste0(Com_ID,"-",Community_name))
  
  # adding the name as an attribute to the nodes.
  graph <- graph %>%
    activate(nodes) %>%
    inner_join(Community_names, by = "Com_ID")
  
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
  
  return(graph)
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
top_ordering <- function(graph, ordering_column ="nb_cit" , top_n = 20, top_n_com = 1, biggest_community = FALSE, community_threshold = 0.01){
  #' Displaying the highest cited nodes
  #' 
  #' A simple functions for keeping a number n of nodes with highest citations 
  #' per communities and a number m of nodes with highest citations in general.
  #' 
  #' @param graph
  #' A tidygraph object.
  #' 
  #' @param ordering_column
  #' The name of the column you want to use to select the most important nodes of your network.
  #' By default, "nb_cit", which usually corresponds to the number of citations of nodes.
  #' 
  #' @param top_n
  #' The number of highest cited nodes in general to display.
  #' 
  #' @param top_n_com
  #' The number of highest cited nodes per community to display.
  #' 
  #' @param biggest_community
  #' If true, you have the possibility to remove the smallest community, depending of the `community_threshold`
  #' you have set.
  #' 
  #' @param community_threshold
  #' If `biggest_community` is true, the function selects the nodes that belong to communities which represent
  #' at least x% of the total number of nodes. By default, the parameter is set to 1%.
  #' 
  #' @details 
  #' Works only if you have a column called `nb_cit`
  #' 
  #' @section Future improvements
  #' Finding a way to name the column in the arguments of the function. The problem here
  #' is that you need to put the column name between quotation marks in the function, but
  #' you have to use it without quotation mark in the function.
  
  # Top nodes per community for the variable chosen
  top_variable_com <- graph %>%
    activate(nodes) %>%
    as_tibble()
  
  # Changing the name of the variable chosen
  colnames(top_variable_com)[colnames(top_variable_com) == ordering_column] = "ordering_column"
  
  # Keeping only the biggest communites if the parameter is TRUE
  if(biggest_community == TRUE){
    top_variable_com <- top_variable_com %>%
      filter(Size_com > community_threshold)
  }
  
  # Keeping the n top nodes per community
  top_variable_com <- top_variable_com %>%
    arrange(desc(ordering_column)) %>%
    group_by(Com_ID) %>% 
    slice(1:top_n_com) %>%
    as.data.table()
  
  # Top nodes in general for the chosen variable
  top_variable_general <- graph %>%
    activate(nodes) %>%
    as_tibble() 
  
  colnames(top_variable_general)[colnames(top_variable_general) == ordering_column] = "ordering_column"
  
  top_variable_general <- top_variable_general %>%
    arrange(desc(ordering_column)) %>%
    slice(1:top_n)%>%
    as.data.table()
  
  # adding the two and removing the doublons
  top_variable_sum <- unique(rbind(top_variable_general, top_variable_com))
  
  colnames(top_variable_sum)[colnames(top_variable_general) == "ordering_column"] = ordering_column
  
  return(top_variable_sum)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################### 3) Building Graphs - Secondary and to be improved ################-------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# A function to concentrate nodes of the same community in one singular node and build the corresponding graph
graph_community <- function(graph, Community_name = "Community_name", nb_components = 1, preparing_graph = TRUE){
  #'Function for building of graph with community as nodes
  #'
  #'This function takes as input a tidygraph object, it switches the names of individual nodes 
  #'by the name of their community, calculate the number of nodes in the community, and 
  #'transforms the whole community as a unique node.
  #'
  #'@param graph
  #'A tidygraph object.
  #'
  #'@param Community_name
  #'Select the column that you want to be used to name the community. By default, the function
  #'considers that you have a column called `Community_name`. Be careful to choose a variable that
  #'identifies all members of a community share. It could be the number of the community, or the
  #'name of the node with the highest degree, number of citations, etc...
  #'
  #'@param nb_components
  #'The number of components you want to keep in the network. Usually, as nodes are community, it is
  #'likely that no node is isolated from the rest of the network.
  #'
  #'@param preparing_graph 
  #'If `TRUE`, the function prepares the graph to be plotted with ggraph. It attributes color to the name
  #'of the nodes, depending of the color attributed in the original graph (the tidygraph use in the parameter
  #'`graph`). The `graph` which serves as input thus need to already have a `color` column. The function
  #'then run the Force Atlas algorithm to calculate the position of nodes.
  #'
  #'@details
  #'For running the Force Atlas, we have standard parameters that are not modifiable via the parameters of the
  #'function, as in general one doesn't have more than 40/50 communities and that you don't need
  #'to adjust the parameters. If one has many communities and one wants to find the communities of these 
  #'communities as node, it is possible to do out from the output of the function.
  #'
  #'@details
  #'The links between nodes(ie communities) are normalized such as not being overestimated for bigger 
  # communities with more edges.
  
  
  Nodes_Com <- graph %>% 
    activate(nodes) %>%
    as_tibble() 
  
  colnames(Nodes_Com)[colnames(Nodes_Com) == Community_name] <- "Community_name"
  
  Nodes_Com <- Nodes_Com %>% 
    mutate(name = Community_name) %>% 
    select(name) %>% 
    group_by(name) %>%
    mutate(nb_nodes = n()) %>%
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
  graph_community <- tbl_main_components(Edges_Com,Nodes_Com,node_key = "name",nb_components = nb_components)
  
  if(preparing_graph == TRUE){
    # merging with the colors attributed to community before
    color_community <- graph %>%
      activate(nodes) %>%
      as_tibble() %>%
      select(Community_name,color) %>%
      unique()
    
    graph_community <- graph_community %>%
      activate(nodes) %>%
      left_join(color_community, by=c("Id"="Community_name"))
    
    
    # Integration a size variable for implementing non-overlapping function of Force Atlas
    graph_community <- graph_community %>%
      activate(nodes) %>%
      mutate(size=nb_nodes)
    
    # Running Force Atlas layout  
    graph_community <- force_atlas(graph_community,seed = NULL, ew.influence = 1, kgrav = 1, iter_1 = 6000, iter_2 = 2000, barnes.hut = FALSE, size_min = 50, size_max = 200)
  }
}


# A function to concentrate nodes with a similar attribute in one singular node and build the corresponding graph
graph_from_attribute <- function(nodes, edges, palette, Attribute_name, nb_components = 1, preparing_graph = TRUE, size_min = 50, size_max = 200){
  #'Function for building of graph with an attribute as nodes
  #'
  #'This function takes as input a tidygraph object, it switches the names of individual nodes 
  #'by one of their attribute, calculate the number of nodes having this attribute, and 
  #'transforms all the nodes with this attribute as a unique node.
  #'
  #'@param nodes
  #'The list of the nodes of your network.
  #'
  #'@param edges
  #'The list of the edges of your network.
  #'
  #'@param palette
  #'A color palette that will be used to attribute color to the communities calculated
  #'by the Leiden algorithm
  #'
  #'@param Attribute_name
  #'Select the column that you want to be used for aggregating the nodes.
  #'
  #'@param nb_components
  #'The number of components you want to keep in the network. Usually, as nodes are community, it is
  #'likely that no node is isolated from the rest of the network.
  #'
  #'@param preparing_graph 
  #'If `TRUE`, the function prepares the graph to be plotted with ggraph. It will run Leiden, attribute
  #'color to the community, calculate some centrality measures and then run the Force Atlast
  #'algorithm to calculate the position of nodes.
  #'
  #'@param size_min
  #'A parameter used in the Force Atlas algorithm to avoid the overlappping of nodes. See the 
  #'`force_atlas` function for documentation.
  #'
  #'@param size_max
  #'See `size_min`.
  #'

  colnames(nodes)[colnames(nodes) == Attribute_name] <- "Attribute_name"
  graph <- tbl_main_components(edges = edges, nodes = nodes, node_key = "ID_Art", threshold_alert = 0.05, directed = FALSE)
  
  Nodes_Com <- graph %>% 
    activate(nodes) %>%
    as_tibble()   %>%
    mutate(name = Attribute_name) %>% 
    select(name) %>% 
    group_by(name) %>%
    mutate(Size_att = n()) %>%
    arrange(desc(Size_att)) %>%
    unique()
  
  # associating the two nodes of each edge with their respective community name
  # summing the weights of the similar edges
  Edges_Com <- graph %>% 
    activate(edges) %>% 
    mutate(Att_name_from = .N()$Attribute_name[from], Att_name_to = .N()$Attribute_name[to])  %>%
    as_tibble() %>% 
    select(Att_name_from,Att_name_to,weight) %>%
    group_by(Att_name_from,Att_name_to) %>%
    mutate(weight=sum(weight)) %>%
    unique() %>%
    as.data.table()
  
  # calculating the sum of weigths for each node (i.e. each community), that is the sum
  # of the weights of all the node edges
  Weight_com1 <- Edges_Com[Att_name_from != Att_name_to,c("Att_name_from","weight")]
  Weight_com2 <- Edges_Com[Att_name_from != Att_name_to,c("Att_name_to","weight")]
  Weight_com3 <- Edges_Com[Att_name_from == Att_name_to,c("Att_name_from","weight")]
  Weight_com <- rbind(Weight_com1,Weight_com2,Weight_com3, use.names = FALSE)
  Weight_com <- Weight_com[, Com_weight := sum(weight), by = "Att_name_from"]
  colnames(Weight_com)[1] <- "Att_name"
  Weight_com <- unique(Weight_com[,c("Att_name","Com_weight")])
  
  # merging the two nodes of each edge with their respective total weight
  Edges_Com <- merge(Edges_Com,Weight_com,by.x="Att_name_from",by.y="Att_name")
  Edges_Com <- merge(Edges_Com,Weight_com,by.x="Att_name_to",by.y="Att_name")
  Edges_Com <- Edges_Com[, c("Att_name_from","Att_name_to","weight","Com_weight.x","Com_weight.y")]
  colnames(Edges_Com) <- c("from","to","weight","Att_name_from","Att_name_to")
  
  # Cosine normalization of the edges weights
  Edges_Com <- Edges_Com[, weight := weight/sqrt(Att_name_from*Att_name_to), by = c("from","to")]
  
  # using our tbl_main_components function to build the tidygraph with a main component
  graph_community <- tbl_main_components(Edges_Com,Nodes_Com,node_key = "name",nb_components = nb_components)
  
  if(preparing_graph == TRUE){
    # Identifying communities with Leiden algorithm                         
    graph_community <- leiden_improved(graph_community, res_1 = 1, res_2 = NULL, res_3 = NULL, n_iterations = 500)       
    
    # Giving colors to communities
    graph_community <- community_colors(graph_community,palette)
    
    # Calculating different centrality measures
    graph_community <- centrality(graph_community)
    
    # Integration a size variable for implementing non-overlapping function of Force Atlas
    graph_community <- graph_community %>%
      activate(nodes) %>%
      mutate(size=Size_att)
    
    # Running Force Atlas layout  
    graph_community <- force_atlas(graph_community,seed = NULL, ew.influence = 1, kgrav = 1, iter_1 = 6000, iter_2 = 2000, barnes.hut = TRUE, size_min = size_min, size_max = size_max)
  }
  
}

clustering_communities <- function(graph, label_size = 6, threshold_com = 0.01){
  #'Function for building a heatmap of the communities
  #'
  #'This function takes as input a tidygraph object with communities as nodes and produce a heatmap of the links
  #'between communities, and a dendrogram of these communities. 
  #'
  #'@param graph
  #'A tidygraph object with nodes being communities and a column `Size_com` which represent the percentage of 
  #'total nodes in the community
  #'
  #'@param label_size
  #'The size of the labels displayed in the heatmap plot 
  #'
  #'#' @param threshold_com
  #' The minimun percentage of nodes in the community for the community to be displayed on the plot.
  #'
  #'@section Future improvements:
  #'Find a way to plot only the biggest communities, but by removing the communities before the plotting, not to
  #'biased the values.
  #'
  #'@section Future improvements:
  #'Using a different method for plotting, by mixing ggplot and ggraph for the dendrogram, to have more options.

  # Extracting edges with only the biggest communities, and integrating the name of communities for source and target of edges
  edges <- graph_coupling_community %>%
    activate(edges) %>%
    mutate(com_name_to = .N()$Id[to], com_name_from = .N()$Id[from]) %>%
    as.data.table()
  
  # making the matrix from the edges
  matrix <- as.matrix(get.adjacency(graph.data.frame(edges[,c("com_name_to","com_name_from","weight")], directed=FALSE), type = "both", attr = "weight"))
  
  # clustering and creation of a dendrogram from the Matrix.
  dendro <- as.dendrogram(hclust(dist(matrix)))
  plot_dendro <- ggdendrogram(dendro, rotate = TRUE) # saving the plot of the dendrogram
  order_dendro <- order.dendrogram(dendro) # extracting the order of nodes 
  
  # keeping only biggest communities
  nodes <- graph_coupling_community %>%
    activate(nodes) %>%
    as.data.table()
  
  nodes <- nodes[,total := sum(nb_nodes)][,share_com := nb_nodes/total][share_com > threshold_com,"Id"]
  
  # cleaning the matrix for plotting the heatmap  
  matrix <- scale(matrix, center = FALSE, scale = colSums(matrix))
  matrix <- melt(matrix) %>% as.data.table()
  matrix$value <- matrix$value*100
  matrix$value <-round(matrix$value, digits = 1)
  matrix  <- matrix[matrix$Var1 %in% nodes$Id & matrix$Var2 %in% nodes$Id]
  matrix$Var1 = str_wrap(matrix$Var1, width = 10)
  matrix$Var2 = str_wrap(matrix$Var2, width = 200)
  
  # ordering the nodes depending of the order of the dendrogram
  matrix$Var1 <- factor(x = matrix$Var1,
                        levels = unique(matrix$Var1)[order_dendro], 
                        ordered = TRUE)
  matrix$Var2 <- factor(x = matrix$Var2,
                        levels = unique(matrix$Var2)[order_dendro], 
                        ordered = TRUE)
  
  # saving the heat map
  plot_heatmap <- ggplot(matrix, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile() +
    theme(text = element_text(size=14)) +
    geom_text(aes(x=Var1, y=Var2, label = value), color = "black", size = label_size) +
    scale_fill_viridis(discrete=FALSE)+
    ylab("In the cluster...") + xlab("...X% of links goes to") 
  
  list_return <- list("heatmap" = plot_heatmap, "dendrogram" = plot_dendro)
  return (list_return)
}

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



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################### 4) Fuctions for word analysis (titles) of networks ################-------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

tf_idf <- function(graph = NULL, nodes = NULL, title_column = "Titre", com_column = "Com_ID", color_column = "color",
                   com_name_column = "Community_name", com_size_column = "Size_com", treshold_com = 0.01, number_of_words = 10, n_columns = 4, 
                   palette = NULL, size_title_wrap = 8)
{
  #' Creating a TF-IDF analysis of the titles of WoS corpus
  #' 
  #' This function takes as input a tidygraph object or a data frame with nodes, both with a community attribute, and analyzes
  #' the words use in the title of the articles to calculate the words with the highest TF-IDF
  #' value for each community.
  #'  
  #' @param graph
  #' A tidygraph object. By default `NULL` in case you prefer to enter a data frame with nodes.
  #' 
  #' @param nodes
  #' A data frame with the nodes of the network, and community and title attributes.
  #' 
  #' @param title_column
  #' The name of the column with the titles of the articles. The function renames the column
  #' "Titre", as in the OST WoS database ("Titre" is the default value).
  #' 
  #' @param com_column
  #' The name of the column with the id of the communities. The function renames the column
  #' "Com_ID" (default value).
  #'
  #' @param color_column
  #' The name of the column with the color attribute of the communities. The function renames the column
  #' "color" (default value).
  #' 
  #' @param com_name_column
  #' The name of the column with the name of the communities.
  #' 
  #' @param com_size_column
  #' The name of the column with the share of total nodes in each community.
  #' 
  #' @param threshold_com
  #' The minimun percentage of nodes in the community for the community to be displayed on the plot.
  #' 
  #' @param number_of_words
  #' How many words you want to display on the final graph.
  #' 
  #' @param n_columns
  #' The number of columns you want to have in your graph (that is the argument of the 
  #' `facet_wrap` function of ggplot2).
  #' 
  #' @param palette
  #' If you don't already have a color attribute for your communities in your tidygraph object,
  #' the function will generate one from a palette that you can add in the paramaters (NULL by default).
  #' 
  #' @param size_title_wrap
  #' The size of the community title in the plot.


# extracting the nodes
  if(! is.null(graph)){
    tf_idf_save <- graph %>% activate(nodes) %>% as.data.table()
  }
  else{
    tf_idf_save <- nodes %>% as.data.table()
  }
  
  # changing the names of the column for titles and communities
  colnames(tf_idf_save)[colnames(tf_idf_save)==com_column] = "Com_ID"
  colnames(tf_idf_save)[colnames(tf_idf_save)==title_column] = "Titre"
  colnames(tf_idf_save)[colnames(tf_idf_save)==color_column] = "color"
  colnames(tf_idf_save)[colnames(tf_idf_save)==com_name_column] = "Community_name"
  colnames(tf_idf_save)[colnames(tf_idf_save)==com_size_column] = "Size_com"
  
  
  
  # adding a color column attribute in case it doesn't exist
  if(colnames(tf_idf_save)[colnames(tf_idf_save)=="color"] != "color"){
    color = data.table(
      Com_ID = 1:500,
      color = mypalette)
    color<-color %>%  mutate(Com_ID = sprintf("%02d", Com_ID)) %>% mutate(Com_ID = as.character(Com_ID))
    
    tf_idf <- merge(tf_idf_save,color, by = "Com_ID", all.x = TRUE)
  }
  
  tf_idf <- tf_idf_save # we will need tf_idf_save later
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Unigram ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Cleaning the titles
  
  tf_idf <- tf_idf[Titre!="NULL"]
  tf_idf[,Titre := removeWords(Titre, stopwords("english"))]
  tf_idf[,Titre := stripWhitespace(Titre)]
  tf_idf[,Titre := removePunctuation(Titre)]
  tf_idf[,Titre := removeNumbers(Titre)]
  tf_idf[,Titre := tolower(Titre)]
  tf_idf[,Titre := removeWords(Titre, stopwords("english"))]
  tf_idf[,Titre := as.character(Titre)]
  tible_tf_idf <- tf_idf[,paste(Titre, collapse = " "), by="Com_ID"]
  tible_tf_idf[,V1 := stripWhitespace(V1)]
  #Dictionnary to find the root of stem word before stemming
  dictionary <- tible_tf_idf
  dictionary <- dictionary %>% unnest_tokens(word, V1)
  tible_tf_idf[,V1 := stemDocument(V1)]
  # tf-idf using quanteda
  tible_tf_idf <- corpus(tible_tf_idf, text_field = "V1")
  tible_tf_idf <- dfm(tible_tf_idf)
  tible_tf_idf <- dfm_tfidf(tible_tf_idf)
  # Keep column of names
  documents_names <- cbind(docvars(tible_tf_idf), quanteda::convert(tible_tf_idf, to = "data.frame")) %>% as.data.table()
  tible_tf_idf <- tidy(tible_tf_idf) %>% as.data.table()
  tf_idf_table <- merge(tible_tf_idf, documents_names[,.(doc_id, Com_ID)], by.x = "document", by.y = "doc_id")
  tf_idf_table[,unstemmed_word:=stemCompletion(tf_idf_table$term, dictionary$word, type = "prevalent")] # unstem with most common word
  tf_idf_table[unstemmed_word=="",unstemmed_word:=term] # unstem with most common word
  tf_idf_table[,term := unstemmed_word]
  tf_idf_table_uni <- tf_idf_table
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Bigram ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #cleaning
  
  tf_idf <- tf_idf_save[Titre!="NULL"]
  tf_idf[,Titre := removeWords(Titre, stopwords("english"))]
  tf_idf[,Titre := stripWhitespace(Titre)]
  tf_idf[,Titre := removePunctuation(Titre)]
  tf_idf[,Titre := removeNumbers(Titre)]
  tf_idf[,Titre := tolower(Titre)]
  tf_idf[,Titre := removeWords(Titre, stopwords("english"))]
  tf_idf[,Titre := as.character(Titre)]
  # ngraming
  tf_idf[,Titre := stemDocument(Titre)]
  tf_idf$Titre <- tokens(tf_idf$Titre, remove_punct = TRUE)
  tf_idf$Titre <- tokens_ngrams(tf_idf$Titre, n = 2)
  tible_tf_idf <- tf_idf[,paste(Titre, collapse = " "), by="Com_ID"]
  tible_tf_idf[,V1 := stripWhitespace(V1)]
  # tf-idf using quanteda
  tible_tf_idf <- corpus(tible_tf_idf, text_field = "V1")
  tible_tf_idf <- dfm(tible_tf_idf)
  tible_tf_idf <- dfm_tfidf(tible_tf_idf)
  # Keep column of names
  documents_names <- cbind(docvars(tible_tf_idf), quanteda::convert(tible_tf_idf, to = "data.frame")) %>% as.data.table()
  tible_tf_idf <- tidy(tible_tf_idf) %>% as.data.table()
  tf_idf_table <- merge(tible_tf_idf, documents_names[,.(doc_id, Com_ID)], by.x = "document", by.y = "doc_id")
  # Unstemming bigram: first term, then second term, them bringing them together
  tf_idf_table$term <- gsub("_", " ", tf_idf_table$term)
  tf_idf_table[,term1:=str_extract(tf_idf_table$term, '\\S+')]
  tf_idf_table[,term2:=str_extract(tf_idf_table$term, '\\S+$')]
  tf_idf_table[,unstemmed_word1:=stemCompletion(tf_idf_table$term1, dictionary$word, type = "prevalent")] # unstem with most common word
  tf_idf_table[unstemmed_word1=="",unstemmed_word:=term1]
  tf_idf_table[,unstemmed_word2:=stemCompletion(tf_idf_table$term2, dictionary$word, type = "prevalent")] # unstem with most common word
  tf_idf_table[unstemmed_word2=="",unstemmed_word:=term2]
  tf_idf_table[,term := paste(unstemmed_word1, unstemmed_word2)]
  tf_idf_table_bi <- tf_idf_table
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Plot ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  tf_idf_table <- rbind(tf_idf_table_uni, tf_idf_table_bi, fill = TRUE)
  tf_idf_table <- tf_idf_table[order(-count)][, head(.SD, number_of_words), Com_ID]
  
  # Get info about size of communities
  Size_com <- unique(tf_idf_save[,.(Com_ID, Community_name, Size_com,color)])
  tf_idf_table <- merge(tf_idf_table, Size_com, by = "Com_ID", all.x = TRUE) # merge
  
  # Wrap Name and Reorder according to share_leiden
  tf_idf_table$Com_wrap <- str_wrap(tf_idf_table$Community_name, width = 10)
  tf_idf_table <- tf_idf_table[order(-Size_com)] # order by variable
  tf_idf_table <- tf_idf_table[order(-Size_com)] # order by variable
  tf_idf_table$Com_wrap <- factor(tf_idf_table$Com_wrap) # make a factor
  tf_idf_table$Com_wrap <- fct_inorder(tf_idf_table$Com_wrap) # by order of appearance
  
  
  tf_idf_plot <- ggplot(tf_idf_table[Size_com>=treshold_com], aes(reorder_within(term, count, color), count, fill = color)) +
    geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
    labs(title = "Highest tf-idf",
         x = "Words", y = "tf-idf") +
    facet_wrap(~Com_wrap, ncol = n_columns, scales = "free") +
    scale_x_reordered() +
    scale_fill_identity() +
    theme(strip.text = element_text(size = size_title_wrap)) +
    coord_flip() 
  list_return <- list("plot" = tf_idf_plot, "list_words" = tf_idf_table)
  return (list_return)

}


############################# Functions built by Olivier Santerre ####################

BigramTokenizer <- function(document)
{
  #This function takes a document and returns every bigram in it
  #[in] document : a full text document
  #[out] a list of bigrams
  unlist(lapply(ngrams(NLP::words(document), 2), paste, collapse = " "), use.names = FALSE)
}
find_most_significant_bigram <- function(corpus, bigram_probability_thrhld = 0.25)
{
  #This function takes a corpus (list of documents) and returns the most probable bigrams in that corpus. Stop words     
  #need to be removed in order to have good   results. The probability of a bigram is a conditionnal probability. It      
  #answers the question of what is the probability of the second word given the probability of the first one. We found    
  #that a probability of 0.25 (every bigram that has a probability of 0.25 or higher) has good results. The corpus must   
  #be a full text that went through a stemming process.
  # [in] corpus, a data.table containing the list of document
  # [in] bigram_probability_thrhld, the threshold over which we keep the bigram (default is 0.25)
  # [out] imp_bigram, a data.table with the most important bigrams. It contains 3 columns, word1, word2 and bigram.
  dt_bigram <- data.table()
  #getting every bigram in the corpus
  for (i in c(1:nrow(corpus)))
  {
    doc_bigram <- data.table(BigramTokenizer(corpus[i]$x), document = i)
    dt_bigram <- rbind(dt_bigram, doc_bigram, fill = TRUE)
  }
  #First we count the frequency of every bigram
  dt_bigram[,bigram_freq := .N, by = "V1"]
  dt_bigram[,bigram := V1]
  #We need to know what are the two words of the bigram
  dt_bigram <- dt_bigram %>% separate(V1, c("word1", "word2"), sep = " ") %>% unique()
  #Getting rid of bigram that appears only once, since they are obviously not the most significant
  dt_bigram <- dt_bigram[bigram_freq > 1]
  #To calculate the probability of a bigram, we need the frequency of every unigram
  dt_unigram <- data.table(word = words(corpus$x))
  dt_unigram <- dt_unigram[,word_freq :=  .N, by = "word"] %>% unique()
  #Merging that frequency on the bigram table
  dt_bigram <- merge(dt_bigram, dt_unigram, by.x = "word1", by.y = "word", all.x = TRUE, all.y = FALSE)
  #calculting the bigram probability
  dt_bigram[,bigram_prob := bigram_freq/word_freq]
  setorder(dt_bigram, -bigram_prob, -bigram_freq)
  #Getting the bigrams with a probability over our threshold
  imp_bigrams <- dt_bigram[bigram_prob > bigram_probability_thrhld, list(word1, word2, bigram)] %>% unique()
  imp_bigrams
}
take_off_unigram_in_bigram <- function(document, imp_bigrams)
{
  #This function takes a list of unigram and takes off the unigram that are part of bigrams present in
  #the imp_bigrams table. The unigrams have to be in the same order as they appear in the document.
  # [in] document, a data.table with a single column named 'unigram' (one word per row)
  # [in] imp_bigrams, a data.table containing the important bigrams
  # [out] document_new without the unigrams that are in important bigrams
  list_of_first_word_in_bigram <- c()
  for (i in 1:(nrow(document)-1))
  {
    #if the first word is in a bigram
    suspect_bigram <- imp_bigrams[word1 == document[i]$unigram]
    if (nrow(suspect_bigram) != 0)
    {
      #and the second word is the next word after in the document
      if (document[i + 1]$unigram %in% suspect_bigram$word2)
      {
        #I keep the index in memory
        list_of_first_word_in_bigram <- c(i, list_of_first_word_in_bigram)
      }
    }
  }
  #Same thing with the second word
  list_of_second_word_in_bigram <- c()
  for (i in 2:(nrow(document)))
  {
    #if a word is the second word of a bigram
    suspect_bigram <- imp_bigrams[word2 == document[i]$unigram]
    if (nrow(suspect_bigram) != 0)
    {
      #and the word before is the document is the first word of that bigram
      if (document[i - 1]$unigram %in% suspect_bigram$word1)
      {
        #I keep the index in memory
        list_of_second_word_in_bigram <- c(i, list_of_second_word_in_bigram)
      }
    }
  }
  #then I bind the two lists together
  list_of_words_in_bigram <- c(list_of_first_word_in_bigram, list_of_second_word_in_bigram) %>% unique()
  #and I return the document without the words that we don't want.
  if(is.null(list_of_words_in_bigram))
  {
    return(document)
  }
  else
  {
    return(document[-list_of_words_in_bigram])
  }
}
cleaning_corpus <- function(table, col_name)
{
  #This function uses tm functions and cleans a corpus. It removes punctuation, numbers, put everything to lower case
  #gets rid of stopwords and also stems words.
  # [in] : table, a table containing a list of document you want to clean
  # [in] : col_name, the name of the column in which the documents are
  # [out] : corpus, a data.table of the cleaned documents
  if (!"text" %in% colnames(table))
  {
    setnames(table, col_name, "text")
  }
  setorder(table, modularity_class)
  #tf-idf for the whole period
  tible_tf_idf <- table[,paste(text, collapse = ", "), by="modularity_class"]
  tible_tf_idf[,V1 := stripWhitespace(V1)]
  tible_tf_idf <- VCorpus(VectorSource(tible_tf_idf$V1))
  tible_tf_idf <- tm_map(tible_tf_idf, removePunctuation)
  tible_tf_idf <- tm_map(tible_tf_idf, removeNumbers)
  tible_tf_idf <- tm_map(tible_tf_idf, content_transformer(tolower))
  tible_tf_idf <- tm_map(tible_tf_idf, removeWords, stopwords("english"))
  tible_tf_idf <- tm_map(tible_tf_idf, stemDocument)
  corpus <- data.table()
  for (i in 1:length(tible_tf_idf))
  {
    corpus <- rbind(corpus, tible_tf_idf[[i]]$content)
  }
  # returning to original name of table (because it's changed outside the function!)
  setnames(table, "text", col_name)
  corpus
}
make_unigram_and_bigram_occurence_table <- function(corpus, imp_bigrams, unstem_dictionary_path)
{
  #This function takes a corpus of documents and produces a table of occurence of unigrams and bigrams.
  #The bigrams we want to count must have been previously identified and passed in the function.
  #This function makes sure not to put unigrams twice (as they may be present in the bigram we want to count).
  #This function also unstem unigram and bigram.
  #[in] corpus, a data.table with one document per row
  #[in] imp_bigrams, a data.table with one bigram per row
  #[in] unstem_dictionary_path, the path to a data.table named "dictionary" with two columns, stem and unstem
  #[out] dt_ngram_occurences, a data.table with every occurences of every unigram and bigram in the corpus
  #Generating the unigram table (all unigram for each document that aren't part of the significant bigram we found)
  dt_ngram_occurences <- data.table()
  for (i in 1:nrow(corpus))
  {
    #generating all the unigrams.
    doc <- str_split(corpus[i], pattern = " ")[[1]]
    doc <- data.table(unigram = doc)
    #self explanatory.
    doc <- take_off_unigram_in_bigram(doc, imp_bigrams)
    #adding the cluster number.
    doc$document <- i
    #binding to the big table.
    dt_ngram_occurences <- rbind(dt_ngram_occurences, doc, fill = TRUE)
  }
  setnames(dt_ngram_occurences, "unigram", "n_gram")
  #Then generating the bigrams table
  dt_bigram <- data.table()
  for (i in c(1:nrow(corpus)))
  {
    doc_bigram <- data.table(BigramTokenizer(corpus[i]$x), document = i)
    dt_bigram <- rbind(dt_bigram, doc_bigram, fill = TRUE)
  }
  setnames(dt_bigram, "V1", "n_gram")
  #only keeping the important bigram
  dt_bigram <- dt_bigram[n_gram %in% imp_bigrams$bigram]
  #Unstemming
  load(unstem_dictionary_path)
  dt_bigram <- dt_bigram %>% separate(n_gram, c("word1", "word2"), sep = " ")
  dt_bigram <- merge(dt_bigram, dictionary, by.x = "word1", by.y = "stem")
  setnames(dt_bigram, "unstem", "unstem_word1")
  dt_bigram <- merge(dt_bigram, dictionary, by.x = "word2", by.y = "stem")
  setnames(dt_bigram, "unstem", "unstem_word2")
  dt_bigram[,n_gram := paste(unstem_word1, unstem_word2)]
  dt_ngram_occurences <- merge(dt_ngram_occurences, dictionary, by.x="n_gram", by.y = "stem")
  setnames(dt_ngram_occurences, c("n_gram", "unstem"), c("junk", "n_gram"))
  #binding the unigram and the bigram
  dt_ngram_occurences <- rbind(dt_ngram_occurences[,list(document,n_gram)],dt_bigram[,list(document, n_gram)])
  dt_ngram_occurences #returning table
}
plot_tfidf <- function(dt_ngram_occurences,title_graph= "TF-IDF for the whole period", colors, order_disc)
{
  #This function takes a table with every occurences of n_gram tokens and its corresponding topic and plots the tf-idf
  #[in] dt_ngram_occurences, a data.table with at least 2 columns, n_gram and Topic. Every row represent one occurence 
  #     of a n_gram token in the corpus.
  #[in] title_graph, the title of the graph
  #[in] colors, a dictionary topic:color. Exemple (c("Topic1" = "Color1", "Topic2" = "Color2", ...))
  #[in] order_disc, a vector containing every Topic in the order we want it to appear in the graph.
  #[out]The function doesn't return the graph, it plots it automatically.
  #Counting the frequencies of every n_gram per document
  dt_tfidf <- dt_ngram_occurences[,list(count=.N), by = c("n_gram", "Topic")]
  #ploting the tf-idf
  dt_plot <- bind_tf_idf(dt_tfidf, n_gram, Topic, count) %>%
    arrange(desc(tf_idf)) %>%
    mutate(n_gram = factor(n_gram, levels = rev(unique(n_gram))),
           chapter = factor(Topic, levels = 1:5)) %>%
    group_by(Topic) %>%
    data.table() %>% 
    setorder(-tf_idf, -count)
  dt_plot$Topic <- factor(dt_plot$Topic, levels = order_disc)
  ggplot(dt_plot[,head(.SD, 10), by = Topic],aes(tidytext::reorder_within(n_gram,tf_idf,Topic), tf_idf, fill = Topic)) +
    geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
    labs(y = "tf-idf", title = title_graph) +
    facet_wrap(~Topic, scales = "free_y") +
    scale_x_reordered() +
    scale_y_continuous(expand=c(0,0)) +
    scale_fill_manual(name = "Topic", values = colors) +
    theme_bw() + coord_flip() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.title.y=element_blank()) + 
    theme(strip.text = element_text(size=9),
          strip.background = element_rect(fill="white", colour="black",size=1))
}
