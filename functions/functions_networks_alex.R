library(docstring)
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART I : Coupling ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

bibliographic_coupling <- function(dt, source = from, ref = to, normalized_weight_only=TRUE, weight_threshold = 1, output_in_character = TRUE)
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
  
  # Computing how many items each citing document has (necessary for normalization later)
  id_nb_cit <-  dt[,list(nb_cit = .N),by=id_art]
  
  # Removing references cited only once:
  dt <- dt[,N := .N, by = id_ref][N > 1][, list(id_art,id_ref)]
  
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

coupling_strength_bibliographic_coupling <- function(dt, source = from, ref = to, normalized_weight_only=TRUE, weight_threshold = 1, output_in_character = TRUE)
{
  #' function for edges of bibliographic coupling
  #' 
  #' This function creates the cosine normalized edges of the bibliographic coupling network, from a direct
  #' citation data frame. It is refined by comparison to our standard function, by taking into account the 
  #' frequency of citations of a references. In other words, most cited references are less important in the links
  #' between two articles.
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
  dt <- dt[id_art!=id_ref]
  # Computing how many items each citing document has (necessary for normalization later)
  id_nb_ref <-  dt[,list(nb_ref = .N),by=id_art]
  # Computing how many times each cited document
  ref_nb_cit <-  dt[,list(nb_cit = .N),by=id_ref]
  # Computing how many times each cited document
  nb_doc <-  dt[unique(id_art)][,list(n_document = .N)]
  # Removing references cited only once:
  dt <- dt[,N := .N, by = id_ref][N > 1][, list(id_art,id_ref)]
  # Creating every combinaison of articles per references
  bib_coup <- dt[,list(Target = rep(id_art[1:(length(id_art)-1)],(length(id_art)-1):1),
                       Source = rev(id_art)[sequence((length(id_art)-1):1)]),
                 by= id_ref]
  # remove loop
  bib_coup <- bib_coup[Source!=Target]
  # Inverse Source and Target so that couple of Source/Target are always on the same side
  bib_coup <- bib_coup[Source > Target, c("Target", "Source") := list(Source, Target)] # exchanging
  ###### Add columns with info for weighting
  #Calculating the number of references in common and deleting the links between articles that share less than weight_threshold
  bib_coup <- bib_coup[,N:= .N,by=list(Target,Source)][N>=weight_threshold] 
  # nb_doc
  bib_coup[,nb_doc:=nb_doc]
  # merge the number of occurence of a ref in a document
  bib_coup <-  merge(bib_coup, ref_nb_cit, by = "id_ref")
  # merge the lenght of reference list
  bib_coup <-  merge(bib_coup, id_nb_ref, by.x = "Target",by.y = "id_art" )
  setnames(bib_coup,"nb_ref", "nb_ref_Target")
  bib_coup <-  merge(bib_coup, id_nb_ref, by.x = "Source",by.y = "id_art" )
  setnames(bib_coup,"nb_ref", "nb_ref_Source")
  # CS
  bib_coup[,weight := (sum(log(nb_doc/nb_cit))) / (nb_ref_Target*nb_ref_Source), .(Source,Target)]
  # Keep only unique couple
  #bib_coup <- bib_coup[, head(.SD, 1), .(Source,Target)]
  bib_coup$from <- as.character(bib_coup$Source)
  bib_coup$to <- as.character(bib_coup$Target)
  bib_coup <- unique(bib_coup[, c("from","to","weight","Source","Target")])

  # # Renaming columns
  # setnames(bib_coup, c("N"), 
  #          c("nb_shared_references"))
  # 
  # # Transforming the Source and Target columns in character (and keeping the Source and Target in copy)
  # # Then selection which columns to return
  # if(output_in_character == TRUE){
  #   bib_coup$from <- as.character(bib_coup$Source)
  #   bib_coup$to <- as.character(bib_coup$Target)
  #   if(normalized_weight_only==TRUE){
  #     return (bib_coup[, c("from","to","weight","Source","Target")])  
  #   } else { 
  #     return (bib_coup[, c("from","to","weight","nb_shared_references","Source","Target")]) 
  #   }
  # }
  # else{
  #   if(normalized_weight_only==TRUE){
  #     return (bib_coup[, c("Source","Target","weight")])  
  #   } else { 
  #     return (bib_coup[, c("Source","Target","weight","nb_shared_references")]) 
  #   }
  # }
  return (bib_coup)
}

cocitation_strength <- function(dt, source = from, ref = to, normalized_weight_only=TRUE, weight_threshold = 1, output_in_character = TRUE)
{
  #' function for edges of bibliographic coupling
  #' 
  #' This function creates the cosine normalized edges of the bibliographic coupling network, from a direct
  #' citation data frame. It is refined by comparison to our standard function, by taking into account the 
  #' frequency of citations of a references. In other words, most cited references are less important in the links
  #' between two articles.
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
  dt <- dt[id_art!=id_ref]
  # Computing how many items each citing document has (necessary for normalization later)
  id_nb_ref <-  dt[,list(nb_ref = .N),by=id_art]
  # Computing how many times each cited document
  ref_nb_cit <-  dt[,list(nb_cit = .N),by=id_ref]
  # Computing how many documents citing
  nb_doc <-  dt[unique(id_art)][,list(n_document = .N)]
  # Removing references cited only once:
  dt <- dt[,N := .N, by = id_art][N > 1][, list(id_art,id_ref)]
  # Creating every combinaison of articles per references
  bib_coup <- dt[,list(Target = rep(id_ref[1:(length(id_ref)-1)],(length(id_ref)-1):1),
                       Source = rev(id_ref)[sequence((length(id_ref)-1):1)]),
                 by= id_art]
  # remove loop
  bib_coup <- bib_coup[Source!=Target]
  # Inverse Source and Target so that couple of Source/Target are always on the same side
  bib_coup <- bib_coup[Source > Target, c("Target", "Source") := list(Source, Target)] # exchanging
  ###### Add columns with info for weighting
  #Calculating the number of references in common and deleting the links between articles that share less than weight_threshold
  bib_coup <- bib_coup[,N:= .N,by=list(Target,Source)][N>=weight_threshold] 
  # nb_doc
  bib_coup[,nb_doc:=nb_doc]
  # merge the number of occurence of a ref in a document
  bib_coup <-  merge(bib_coup, id_nb_ref, by = "id_art")
  # merge the lenght of reference list
  bib_coup <-  merge(bib_coup, ref_nb_cit, by.x = "Target",by.y = "id_ref" )
  setnames(bib_coup,"nb_cit", "nb_cit_Target")
  bib_coup <-  merge(bib_coup, ref_nb_cit, by.x = "Source",by.y = "id_ref" )
  setnames(bib_coup,"nb_cit", "nb_cit_Source")
  # CS
  bib_coup[,weight := (sum(log(nb_doc/nb_ref))) / (nb_cit_Target*nb_cit_Source), .(Source,Target)]
  # Keep only unique couple
  #bib_coup <- bib_coup[, head(.SD, 1), .(Source,Target)]
  bib_coup$from <- as.character(bib_coup$Source)
  bib_coup$to <- as.character(bib_coup$Target)
  bib_coup <- unique(bib_coup[, c("from","to","weight","Source","Target")])
  
  # # Renaming columns
  # setnames(bib_coup, c("N"), 
  #          c("nb_shared_references"))
  # 
  # # Transforming the Source and Target columns in character (and keeping the Source and Target in copy)
  # # Then selection which columns to return
  # if(output_in_character == TRUE){
  #   bib_coup$from <- as.character(bib_coup$Source)
  #   bib_coup$to <- as.character(bib_coup$Target)
  #   if(normalized_weight_only==TRUE){
  #     return (bib_coup[, c("from","to","weight","Source","Target")])  
  #   } else { 
  #     return (bib_coup[, c("from","to","weight","nb_shared_references","Source","Target")]) 
  #   }
  # }
  # else{
  #   if(normalized_weight_only==TRUE){
  #     return (bib_coup[, c("Source","Target","weight")])  
  #   } else { 
  #     return (bib_coup[, c("Source","Target","weight","nb_shared_references")]) 
  #   }
  # }
  return (bib_coup)
}

bibliographic_cocitation <- function(dt, source = from, ref = to, normalized_weight_only=TRUE, weight_threshold = 1, output_in_character = TRUE)
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
  
  # Computing how many items each cited document has (necessary for normalization later)
  id_nb_cit <-  dt[,list(nb_cit = .N),by=id_ref]
  
  # Removing references cited only once:
  dt <- dt[, N := .N, by = id_art][N > 1][, list(id_art,id_ref)]
  
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
      return (bib_cocit[, c("from","to","weight","Source","Target", "nb_cit_Source", "nb_cit_Target")])  
    } else { 
      return (bib_cocit[, c("from","to","weight","nb_shared_references","Source","Target", "nb_cit_Source", "nb_cit_Target")]) 
    }
  }
  else{
    if(normalized_weight_only==TRUE){
      return (bib_cocit[, c("Source","Target","weight", "nb_cit_Source", "nb_cit_Target")])  
    } else { 
      return (bib_cocit[, c("Source","Target","weight","nb_shared_references", "nb_cit_Source", "nb_cit_Target")]) 
    }
  }
  
  # Selecting which columns to return
  # if(normalized_weight_only==TRUE){
  #    return (bib_cocit[, c("from","to","weight","Source","Target")])  
  #  } else { 
  #    return (bib_cocit[, c("from","to","weight","nb_shared_references")]) 
  #  }
  
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART II : From tbl to graph ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

make_a_graph <- function(tbl, 
                         color_table = color, 
                         resolution = 1, 
                         detection_method = "RBConfigurationVertexPartition", 
                         layout_graph = "TRUE",
                         treshold_color = 0.2, treshold_ignore = 0.05, 
                         iter_1 = 10000, iter_2 = 2000, 
                         name_leiden = NULL)
{
  #' function for creating a graph
  #' 
  #' @tbl
  #' A tidygraph
  #' 
  #' @color_table
  #' A table with two columns: Leiden1 with numbers or the name of the community and color the desired color
  #' 
  #' @resolution
  #' Resolution. 1 for Leiden, or 0.001-5 for CPM
  #' 
  #' @detection_method
  #' RBConfigurationVertexPartition or CPMVertexPartition
  #' 
  #' @treshold_color
  #' Treshold when you start to have grey nodes
  #'
  #' @iter_1
  #' Number of iterations for FA2 10 000
  #'
  #' @iter_2
  #' Number of iterations for FA2 without overlap. 1000-2000 is nice
  #'
  #' @name_leiden
  #' DF with the new name
  #'   
  #' @new_name
  #' Column with the new names
  #' 
  
  #' 
  
  #Inspect the size of the components
  components <- tbl %>% 
    activate(nodes) %>% 
    mutate(components_att = group_components(type = "weak")) %>% 
    as.data.table()
  components[,.N,components_att][order(-N)]
  
  
  #Keep Main Component
  tbl <- tbl %>% 
    activate(nodes) %>% 
    mutate(components_att = group_components(type = "weak")) %>% 
    filter(components_att == 1)
  
  # Some metrics
  tbl <- tbl %>%
    activate(nodes) %>%
    mutate(betweenness = centrality_betweenness(
      weights = weight,
      directed = FALSE,
      normalized = TRUE)) %>%
    mutate(closeness = centrality_closeness(
      weights = weight,
      normalized = TRUE)) %>%
    mutate(degree = centrality_degree(
      weights = weight,
      loops = FALSE,
      normalized = FALSE))
  
  #Leiden
  leiden <- leiden(tbl, resolution_parameter = resolution, n_iterations = -1, weights = E(tbl)$weight, seed = 1, partition_type = detection_method)
  tbl <- tbl %>% activate(nodes) %>% mutate(Leiden_num = leiden) %>% mutate(Leiden1 = leiden) %>% mutate(Leiden1 = sprintf("%02d", Leiden1)) %>% mutate(Leiden1 = as.character(Leiden1))
  
  #Share Leiden
  share_l <- tbl %>% activate(nodes) %>% as.data.table()
  share_l <- share_l[,share_leiden:= .N/share_l[,.N], Leiden1]
  tbl <- tbl %>% 
    activate(nodes) %>% 
    left_join(share_l)
  
  #New Names for Leiden
  name <- tbl  %>% activate(nodes)  %>% as.data.table()
  
  if(is.null(name_leiden)){
    name <- name
  }
  if(!is.null(name_leiden)){
    name <- merge(name[, c("Leiden1"):=NULL], name_leiden, by = "Leiden_num")
    name[Leiden1=="NA", Leiden1:= sprintf("%02d", Leiden_num)]  
  }
  
  tbl <- tbl %>% activate(nodes) %>% select(-c(Leiden1)) %>% left_join(name) 

  #### Color #### 
  
  #Add color to nodes
  tbl <- tbl %>% 
    activate(nodes) %>% 
    left_join(color)
  
  # Grey if less than X
  color_tresh <- tbl %>% 
    activate(nodes) %>% 
    as.data.table()
  color_tresh[share_leiden < treshold_color, color:="#c0c0c0"]
  
  # Join
  tbl <- tbl %>% 
    activate(nodes) %>% 
    select (-c(color)) %>% 
    left_join(color_tresh[,.(Id, color)])
  
    
  #Mix color for edges of different color
  tbl <- tbl %>% #mix color
    activate(edges) %>%
    mutate(com_ID_to = .N()$color[to], com_ID_from = .N()$color[from]) %>%
    mutate(color_edges = MixColor(com_ID_to, com_ID_from, amount1 = 0.5))  # .N() makes the node data available while manipulating edges
  
  #### Layout #### 
  if(layout_graph == "TRUE"){
 
  #Force Atlas 2 (without overlap)
  fa_lay <- layout_forceatlas2(tbl, ew.influence = 1, kgrav = 1, iter = iter_1,
                               prevent.overlap = FALSE, fixed = NULL, stopping.tolerance = 0.001,
                               barnes.hut = TRUE)

  fa_lay <- fa_lay$lay %>% as.data.table
  tbl <- tbl %>% activate(nodes) %>% mutate(x = fa_lay$x, y = fa_lay$y)  
  
  #Force Atlas 2 (with overlap)
  fa_lay <- layout_forceatlas2(tbl, ew.influence = 1, kgrav = 1, iter = iter_2,
                               prevent.overlap = TRUE, fixed = NULL, stopping.tolerance = 0.001,
                               barnes.hut = TRUE)

  fa_lay <- fa_lay$lay %>% as.data.table
  tbl <- tbl %>% activate(nodes) %>%  mutate(x = fa_lay$x, y = fa_lay$y)
  }

  #### Secondary network #### 

  #Add color to nodes
  tbl_sec <- tbl %>% 
    activate(nodes) %>% 
    select (-c(color)) %>% 
    left_join(color)
  
  # Grey if less than X
  color_tresh <- tbl_sec %>% 
    activate(nodes) %>% 
    as.data.table()
  color_tresh[share_leiden >= treshold_color, color:="#c0c0c0"]
  color_tresh[share_leiden < treshold_ignore, color:="#c0c0c0"]
  
  # Join
  tbl_sec <- tbl_sec %>% 
    activate(nodes) %>% 
    select (-c(color)) %>% 
    left_join(color_tresh[,.(Id, color)])
  
  
  #Mix color for edges of different color
  tbl_sec <- tbl_sec %>% #mix color
    activate(edges) %>%
    mutate(com_ID_to = .N()$color[to], com_ID_from = .N()$color[from]) %>%
    mutate(color_edges = MixColor(com_ID_to, com_ID_from, amount1 = 0.5))  # .N() makes the node data available while manipulating edges
  
  nodes <- tbl %>% activate(nodes) %>% as.data.table()
  
  list_return <- list("tbl" = tbl, "components" = components[,.N,components_att][order(-N)], "tbl_sec" = tbl_sec, "nodes" = nodes)
  
  return (list_return)
}


main_components <- function(tbl, minimum_component_size = 5)
{  
  #Inspect the size of the components
  components <- tbl %>% 
    activate(nodes) %>% 
    mutate(components_att = group_components(type = "weak")) %>% 
    as.data.table()
  components[,.N,components_att][order(-N)]
  
  components_to_keep <- components[,.N,components_att][order(-N)][N>=minimum_component_size]$components_att
  
  #Keep Main Component
  tbl <- tbl %>% 
    activate(nodes) %>% 
    mutate(components_att = group_components(type = "weak")) %>% 
    filter(components_att %in% components_to_keep)
  
  list_return <- list("tbl" = tbl, "components" = components[,.N,components_att][order(-N)])
  if (components[,.N,components_att][order(-N)][,.N]>1){
    print(paste0("Second Biggest components is of size:", components[,.N,components_att][order(-N)]$N[[2]]))
  }
  
  return (tbl)
}



detect_leiden <- function(tbl, 
                          resolution = 1, 
                          detection_method = "RBConfigurationVertexPartition", 
                          name_leiden = NULL)
{
  #Leiden
  leiden <- leiden(tbl, resolution_parameter = resolution, n_iterations = -1, weights = E(tbl)$weight, seed = 1, partition_type = detection_method)
  tbl <- tbl %>% activate(nodes) %>% mutate(Leiden_num = leiden) %>% mutate(Leiden1 = leiden) %>% mutate(Leiden1 = sprintf("%02d", Leiden1)) %>% mutate(Leiden1 = as.character(Leiden1))
  
  #Share Leiden
  share_l <- tbl %>% activate(nodes) %>% as.data.table()
  share_l <- share_l[,share_leiden:= .N/share_l[,.N], Leiden1]
  tbl <- tbl %>% 
    activate(nodes) %>% 
    left_join(share_l)
  
  #New Names for Leiden
  name <- tbl  %>% activate(nodes)  %>% as.data.table()
  
  if(is.null(name_leiden)){
    name <- name
  }
  if(!is.null(name_leiden)){
    name <- merge(name[, c("Leiden1"):=NULL], name_leiden, by = "Leiden_num")
    name[Leiden1=="NA", Leiden1:= sprintf("%02d", Leiden_num)]  
  }
  
  tbl <- tbl %>% activate(nodes) %>% select(-c(Leiden1)) %>% left_join(name)
  
  return (tbl)
}



detect_leidenalg <- function(tbl, 
                          resolution = 1, 
                          name_leiden = NULL,
                          niter = 5000)
{
  #Leiden
  leiden <- leidenAlg::leiden.community(tbl, resolution = resolution, n.iterations = niter)
  # leiden <- leidenAlg::find_partition(tbl, edge_weights = igraph::E(tbl)$weight, resolution = resolution, niter = niter) OLD FUNCTION NOT SURE ITER WORK
  
  tbl <- tbl %>% activate(nodes) %>% mutate(Leiden_num = leiden$membership) %>% mutate(Leiden1 = leiden$membership) %>% mutate(Leiden1 = sprintf("%02d", Leiden1)) %>% mutate(Leiden1 = as.character(Leiden1))
  
  #Share Leiden
  share_l <- tbl %>% activate(nodes) %>% as.data.table()
  share_l <- share_l[,share_leiden:= .N/share_l[,.N], Leiden1]
  tbl <- tbl %>% 
    activate(nodes) %>% 
    left_join(share_l)
  
  #New Names for Leiden
  name <- tbl  %>% activate(nodes)  %>% as.data.table()
  
  if(is.null(name_leiden)){
    name <- name
  }
  if(!is.null(name_leiden)){
    name <- merge(name[, c("Leiden1"):=NULL], name_leiden, by = "Leiden_num")
    name[Leiden1=="NA", Leiden1:= sprintf("%02d", Leiden_num)]  
  }
  
  tbl <- tbl %>% activate(nodes) %>% select(-c(Leiden1)) %>% left_join(name)
  
  return (tbl)
}






layout_fa2 <- function(tbl,
                       iter_1 = 10000, 
                       iter_2 = 2000,
                       kgrav_value = 1)
{
  #Layout
  #Force Atlas 2 (without overlap)
  fa_lay <- layout_forceatlas2(tbl, ew.influence = 1, kgrav = kgrav_value, iter = iter_1,
                               prevent.overlap = FALSE, fixed = NULL, stopping.tolerance = 0.001,
                               barnes.hut = TRUE)
  
  fa_lay <- fa_lay$lay %>% as.data.table
  tbl <- tbl %>% activate(nodes) %>% mutate(x = fa_lay$x, y = fa_lay$y)  
  
  #Force Atlas 2 (with overlap)
  fa_lay <- layout_forceatlas2(tbl, ew.influence = 1, kgrav = 1, iter = iter_2,
                               prevent.overlap = TRUE, fixed = NULL, stopping.tolerance = 0.001,
                               barnes.hut = TRUE)
  
  fa_lay <- fa_lay$lay %>% as.data.table
  tbl <- tbl %>% activate(nodes) %>%  mutate(x = fa_lay$x, y = fa_lay$y)
  return (tbl)
}


layout_fa2_java <- function(tbl=tbl,niter=3000)
{
  #Layout
  graph_before <- tbl %>% activate(nodes) %>% mutate(id=as.character(Id))
  nodes_before <- tbl %>% activate(nodes) %>% as.data.table()
  
  if("x" %in% colnames(nodes_before)){
    graph_before <- graph_before %>% activate(nodes) %>% mutate(x = ifelse(is.na(x)==TRUE,sample(1:100),x))
    graph_before <- graph_before %>% activate(nodes) %>% mutate(y = ifelse(is.na(y)==TRUE,sample(1:100),y))
    graph_before <- graph_before %>% activate(nodes) %>% select(c(id,ID_Art,x,y,size))
  }
  else{
    graph_before <- graph_before %>% activate(nodes) %>% select(c(id,ID_Art,size))
  }
  
  write.graph(graph = graph_before, file = 'tidy.graphml', format = 'graphml')
  system(paste0('java -jar "D://Dropbox/8-Projets Quanti/1-R_Projects/Home/2-External_tools/GephiLayouts-1.0.jar" forceatlas2 -i ./tidy.graphml -o  ./forceatlas2.graphml -threads 16 -maxiters ',niter,' -barneshut true -adjustsizes true -gravity 1'))
  
  
  gml <- read.graph("forceatlas2.graphml", format="graphml")
  graph_after <- as_tbl_graph(gml)
  graph_after <- graph_after %>% activate(nodes) %>% as.data.table() %>% .[,.(x,y,ID_Art)]
  
  if("x" %in% colnames(nodes_before)){
    tbl <- tbl %>% activate(nodes) %>% select(-c(x,y)) %>% left_join(graph_after)
  }
  else{
    tbl <- tbl %>% activate(nodes) %>% left_join(graph_after)
  }
  
  return (tbl)
}


color_tbl <- function(tbl=tbl,
                      color_table=color,
                      treshold_grey = 0)
{
  #### Color #### 
  color <- color_table
  #Add color to nodes
  tbl <- tbl %>% 
    activate(nodes) %>% 
    left_join(color)
  
  #Share color
  share_color <- tbl %>% activate(nodes) %>% as.data.table()
  n_tot <- share_color[,.N]
  share_color <- share_color[,share_color:= .N/n_tot, color]
  tbl <- tbl %>% 
    activate(nodes) %>% 
    left_join(share_color)
  
  # Grey if less than X
  color_tresh <- tbl %>% 
    activate(nodes) %>% 
    as.data.table()
  color_tresh[share_color < treshold_grey, color:="#c0c0c0"]
  
  # Join
  tbl <- tbl %>% 
    activate(nodes) %>% 
    select (-c(color)) %>% 
    left_join(color_tresh)
  
  tbl <- tbl %>% activate(nodes) %>% mutate(color=ifelse(is.na(color)==TRUE,"grey",color))
  
  #Mix color for edges of different color
  tbl <- tbl %>% #mix color
    activate(edges) %>%
    mutate(com_ID_to = .N()$color[to], com_ID_from = .N()$color[from]) %>%
    mutate(color_edges = MixColor(com_ID_to, com_ID_from, amount1 = 0.5))  # .N() makes the node data available while manipulating edges
  
  return (tbl)
}














tf_idf <- function(tbl = tbl, color_table = color, number_of_words = 10, n_columns = 4, treshold_com=0, size_title_wrap=6, unstemming = TRUE)
{
  #' function for creating a graph
  #' 
  #' @tbl
  #' A tidygraph, you need a Leiden1 column for community, and a Titre column for Titles, a share_leiden for importance of communities
  #' @color_table
  #' A table with a Leiden 1 column, and a color column for the color of communities
  #' @number_of_words
  #' How many words on the graph
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Unigram ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Cleaning the titles
  tf_idf <- tbl %>% activate(nodes) %>% as.data.table()
  tf_idf <- tf_idf[Titre!="NULL"]
  tf_idf[,Titre := removeWords(Titre, stopwords("english"))]
  tf_idf[,Titre := stripWhitespace(Titre)]
  tf_idf[,Titre := removePunctuation(Titre)]
  tf_idf[,Titre := removeNumbers(Titre)]
  tf_idf[,Titre := tolower(Titre)]
  tf_idf[,Titre := removeWords(Titre, stopwords("english"))]
  tf_idf[,Titre := as.character(Titre)]
  tible_tf_idf <- tf_idf[,paste(Titre, collapse = " "), by="Leiden1"]
  tible_tf_idf[,V1 := stripWhitespace(V1)]
  
  #Dictionnary to find the root of stem word before stemming
  dictionary <- tible_tf_idf
  dictionary <- dictionary %>% unnest_tokens(word, V1) %>% as.data.table()
  
  tible_tf_idf[,V1 := stemDocument(V1)]
  
  # tf-idf using quanteda
  tible_tf_idf <- corpus(tible_tf_idf, text_field = "V1")
  
  tible_tf_idf <- dfm(tible_tf_idf)
  
  tible_tf_idf <- dfm_tfidf(tible_tf_idf)
  
  # Keep column of names
  documents_names <- cbind(docvars(tible_tf_idf), convert(tible_tf_idf, to = "data.frame")) %>% as.data.table()
  tible_tf_idf <- tidy(tible_tf_idf) %>% as.data.table()
  tf_idf_table <- merge(tible_tf_idf, documents_names[,.(doc_id, Leiden1)], by.x = "document", by.y = "doc_id")
  
  if(unstemming==TRUE){
  unstemming_table <- tf_idf_table[order(-count)][, head(.SD, number_of_words), .(Leiden1)]
  unstemming_table[,unstemmed_word:=stemCompletion(unstemming_table$term, dictionary$word, type = "prevalent")] # unstem with most common word
  
  tf_idf_table <- tf_idf_table %>% left_join(unstemming_table)
  tf_idf_table[unstemmed_word=="" | is.na(unstemmed_word)==TRUE, unstemmed_word:=term] # unstem with most common word
  }
  if(unstemming==FALSE){
    tf_idf_table[,unstemmed_word:=term]
  }
  

  tf_idf_table[,term := unstemmed_word]
  
  tf_idf_table_uni <- tf_idf_table
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Bigram ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  #cleaning
  tf_idf <- tbl %>% activate(nodes) %>% as.data.table()
  tf_idf <- tf_idf[Titre!="NULL"]
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
  tible_tf_idf <- tf_idf[,paste(Titre, collapse = " "), by="Leiden1"]
  tible_tf_idf[,V1 := stripWhitespace(V1)]
  
  # tf-idf using quanteda
  tible_tf_idf <- corpus(tible_tf_idf, text_field = "V1")
  tible_tf_idf <- dfm(tible_tf_idf)
  tible_tf_idf <- dfm_tfidf(tible_tf_idf)
  
  # Keep column of names
  documents_names <- cbind(docvars(tible_tf_idf), convert(tible_tf_idf, to = "data.frame")) %>% as.data.table()
  tible_tf_idf <- tidy(tible_tf_idf) %>% as.data.table()
  tf_idf_table <- merge(tible_tf_idf, documents_names[,.(doc_id, Leiden1)], by.x = "document", by.y = "doc_id")
  
  # Unstemming bigram: first term, then second term, them bringing them together
  tf_idf_table$term <- gsub("_", " ", tf_idf_table$term)
  
  tf_idf_table[,term1:=str_extract(tf_idf_table$term, '\\S+')]
  tf_idf_table[,term2:=str_extract(tf_idf_table$term, '\\S+$')]
  
  if(unstemming==TRUE){
  unstemming_table <- tf_idf_table[order(-count)][, head(.SD, number_of_words), .(Leiden1)]
  unstemming_table[,unstemmed_word1:=stemCompletion(unstemming_table$term1, dictionary$word, type = "prevalent")] # unstem with most common word
  tf_idf_table <- tf_idf_table %>% left_join(unstemming_table)
  tf_idf_table[unstemmed_word1=="" | is.na(unstemmed_word1)==TRUE,unstemmed_word1:=term1]# unstem with most common word
  
  unstemming_table <- tf_idf_table[order(-count)][, head(.SD, number_of_words), .(Leiden1)]
  unstemming_table[,unstemmed_word2:=stemCompletion(unstemming_table$term2, dictionary$word, type = "prevalent")] # unstem with most common word
  tf_idf_table <- tf_idf_table %>% left_join(unstemming_table)
  tf_idf_table[unstemmed_word2=="" | is.na(unstemmed_word2)==TRUE,unstemmed_word2:=term2]
  }
  if(unstemming==FALSE){
    tf_idf_table[,unstemmed_word1:=term1]
    tf_idf_table[,unstemmed_word2:=term2]
  }
  
  tf_idf_table[,term := paste(unstemmed_word1, unstemmed_word2)]
  
  tf_idf_table_bi <- tf_idf_table
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Plot ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  tf_idf_table <- rbind(tf_idf_table_uni, tf_idf_table_bi, fill = TRUE)
  
  # Colors
  tf_idf_table <- tf_idf_table[order(-count)][, head(.SD, number_of_words), Leiden1]
  tf_idf_table <- merge(tf_idf_table, color_table, by = "Leiden1", all.x = TRUE)
  
  # Get info about size of communities
  tf_idf <- tbl %>% activate(nodes) %>% as.data.table() # get the number
  share_leiden <- tf_idf[,.(Leiden1, share_leiden)]
  tf_idf_table <- merge(tf_idf_table, share_leiden[,.N,.(Leiden1, share_leiden)], by = "Leiden1", all.x = TRUE) # merge
  
  # Wrap Name and Reorder according to share_leiden
  tf_idf_table$Leiden_wrap <- str_wrap(tf_idf_table$Leiden1, width = 10)
  
  tf_idf_table <- tf_idf_table[order(-share_leiden)] # order by variable
  tf_idf_table <- tf_idf_table[order(-share_leiden)] # order by variable
  tf_idf_table$Leiden_wrap <- factor(tf_idf_table$Leiden_wrap) # make a factor
  tf_idf_table$Leiden_wrap <- fct_inorder(tf_idf_table$Leiden_wrap) # by order of appearance
  
  plot <- ggplot(tf_idf_table[share_leiden>=treshold_com], aes(reorder_within(term, count, color), count, fill = color)) +
    geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
    labs(title = "Highest tf-idf",
         x = "Words", y = "tf-idf") +
    facet_wrap(~Leiden_wrap, ncol = n_columns, scales = "free") +
    scale_x_reordered() +
    scale_fill_identity() +
    theme(strip.text = element_text(size = size_title_wrap)) +
    coord_flip() 
  
  list_return <- list("plot" = plot, "list_words" = tf_idf_table[,.(Leiden1, term, count, color)], "dictionary" = dictionary)
  
  return (list_return)
}



########################### Old working functions in case ############################################
tf_idf_old <- function(tbl = tbl, color_table = color, number_of_words = 10, n_columns = 4, n_com=1000)
{
  #' function for creating a graph
  #' 
  #' @tbl
  #' A tidygraph, you need a Leiden1 column for community, and a Titre column for Titles
  #' @color_table
  #' A table with a Leiden 1 column, and a color column for the color of communities
  #' @number_of_words
  #' How many words on the graph

# Cleaning the titles
tf_idf <- tbl %>% activate(nodes) %>% as.data.table()
tf_idf <- tf_idf[Titre!="NULL"]
tf_idf[,Titre := stripWhitespace(Titre)]
tf_idf[,Titre := removePunctuation(Titre)]
tf_idf[,Titre := removeNumbers(Titre)]
tf_idf[,Titre := tolower(Titre)]
tf_idf[,Titre := removeWords(Titre, stopwords("english"))]
tf_idf[,Titre := as.character(Titre)]
tible_tf_idf <- tf_idf[,paste(Titre, collapse = ", "), by="Leiden1"]

dictionary <- tible_tf_idf #Dictionnary to find the root of stem word before stemming
dictionary <- dictionary %>% unnest_tokens(word, V1)

tible_tf_idf[,V1 := stemDocument(V1)]

# tf-idf using quanteda
tible_tf_idf <- corpus(tible_tf_idf, text_field = "V1")

tible_tf_idf <- dfm(tible_tf_idf)

tible_tf_idf <- dfm_tfidf(tible_tf_idf)

# Keep column of names
documents_names <- cbind(docvars(tible_tf_idf), as.data.frame(tible_tf_idf)) %>% as.data.table()
tible_tf_idf <- tidy(tible_tf_idf) %>% as.data.table()
tible_tf_idf <- merge(tible_tf_idf, documents_names[,.(doc_id, Leiden1)], by.x = "document", by.y = "doc_id")

tf_idf_table <- tible_tf_idf[order(-count)][, head(.SD, number_of_words), Leiden1]
tf_idf_table <- merge(tf_idf_table, color_table, by = "Leiden1", all.x = TRUE)

tf_idf_table[,unstemmed_word:=stemCompletion(tf_idf_table$term, dictionary$word, type = "prevalent")] # unstem with most common word
tf_idf_table[unstemmed_word=="",unstemmed_word:=term] # unstem with most common word

plot <- ggplot(tf_idf_table[as.integer(Leiden1)<=n_com], aes(reorder_within(unstemmed_word, count, color), count, fill = color)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  labs(title = "Highest tf-idf",
       x = "Words", y = "tf-idf") +
  facet_wrap(~Leiden1, ncol = n_columns, scales = "free") +
  scale_x_reordered() +
  scale_fill_identity() +
  coord_flip() 

list_return <- list("plot" = plot, "list_words" = tf_idf_table, "dictionary" = dictionary)

return (list_return)
}



sigma_it <- function(tbl = tbl)
{
nodes <- tbl %>% activate(nodes) %>% as.data.table()
nodes[,id:=as.character(Id)]
nodes[,label:=as.character(Label)]
edges <- tbl %>% activate(edges) %>% as.data.table()
edges[,id:=as.character(seq(1, nrow(edges)), by=1)]
edges[,target:=as.character(Target)]
edges[,source:=as.character(Source)]
sigmajs() %>% # initialise
  sg_nodes(nodes, id, label, size, color) %>% # add nodes
  sg_edges(edges, id, source, target) %>% # add edges
  sg_settings(drawLabels = TRUE, drawEdgeLabels = FALSE) %>% 
  sg_force() %>% 
  sg_force_stop() %>% # stop force after 5 seconds
  sg_drag_nodes() %>% 
  sg_noverlap() %>%
  sg_settings(defaultEdgeType = "curve") %>%
  sg_button(c("force_start", "force_stop"), "Start Layout") %>%
  sg_button(c("force_stop"), "Stop Layout") %>%
  sg_button(c("noverlap"), "No Overlap")
}


detect_walktrap <- function(tbl)
{
  #Leiden
  leiden <- cluster_walktrap(tbl, weights = E(tbl)$weight, steps = 4)
  tbl <- tbl %>% activate(nodes) %>% mutate(Leiden_num = leiden$membership) %>% mutate(Leiden1 = leiden$membership) %>% mutate(Leiden1 = sprintf("%02d", Leiden1)) %>% mutate(Leiden1 = as.character(Leiden1))
  
  #Share Leiden
  share_l <- tbl %>% activate(nodes) %>% as.data.table()
  share_l <- share_l[,share_leiden:= .N/share_l[,.N], Leiden1]
  tbl <- tbl %>% 
    activate(nodes) %>% 
    left_join(share_l)
  
  return (tbl)
}

detect_infomap <- function(tbl)
{
  #Leiden
  leiden <- cluster_infomap(tbl, e.weights = E(tbl)$weight)
  tbl <- tbl %>% activate(nodes) %>% mutate(Leiden_num = leiden$membership) %>% mutate(Leiden1 = leiden$membership) %>% mutate(Leiden1 = sprintf("%02d", Leiden1)) %>% mutate(Leiden1 = as.character(Leiden1))
  
  #Share Leiden
  share_l <- tbl %>% activate(nodes) %>% as.data.table()
  share_l <- share_l[,share_leiden:= .N/share_l[,.N], Leiden1]
  tbl <- tbl %>%
    activate(nodes) %>%
    left_join(share_l)
  
  return (tbl)
}


detect_leiden_igraph <- function(tbl)
{
  #Leiden
  leiden <- cluster_leiden(tbl, objective_function="modularity", weight = E(tbl)$weight, n_iterations=1000)
  tbl <- tbl %>% activate(nodes) %>% mutate(Leiden_num = leiden$membership) %>% mutate(Leiden1 = leiden$membership) %>% mutate(Leiden1 = sprintf("%02d", Leiden1)) %>% mutate(Leiden1 = as.character(Leiden1))
  
  #Share Leiden
  share_l <- tbl %>% activate(nodes) %>% as.data.table()
  share_l <- share_l[,share_leiden:= .N/share_l[,.N], Leiden1]
  tbl <- tbl %>%
    activate(nodes) %>%
    left_join(share_l)
  print("Done")
  return (tbl)
}

