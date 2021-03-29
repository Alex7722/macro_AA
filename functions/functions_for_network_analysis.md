List of the functions built for network analysis
================
Aurélien Goutsmedt and Alexandre Truc
/ Last compiled on 2021-03-11

  - [1 What is this script for?](#what-is-this-script-for)
  - [2 GENERAL FUNCTIONS FOR NETWORK
    ANALYSIS](#general-functions-for-network-analysis)
      - [2.1 `bibliographic_coupling()`](#bibliographic_coupling)
      - [2.2
        `bibliographic_coupling_alt()`](#bibliographic_coupling_alt)
      - [2.3
        `bibliographic_coupling_authors()`](#bibliographic_coupling_authors)
      - [2.4 `bibliographic_cocitation()`](#bibliographic_cocitation)
  - [3 Building graphs - Basics](#building-graphs---basics)
      - [3.1 `tbl_main_components()`](#tbl_main_components)
  - [4 Building Graphs - Secondary and to be
    improved](#building-graphs---secondary-and-to-be-improved)
  - [5 Dynamic networks: building the different
    lists](#dynamic-networks-building-the-different-lists)
      - [5.1 `dynamic_biblio_coupling()`](#dynamic_biblio_coupling)
  - [6 Functions for word analysis (titles) of
    networks](#functions-for-word-analysis-titles-of-networks)
      - [6.1 `tf_idf()`](#tf_idf)

# 1 What is this script for?

This script lists all the functions built for network analysis for the
project. The functions are documenting in a way to be later implemented
in packages or at least for generating a help page (see below).

We first load the
[docstring](https://cran.r-project.org/web/packages/docstring/vignettes/docstring_intro.html)
package to be able to write documentation in a roxygen2 style. You can
then run docstring(name\_of\_function) to get the standard help page.

``` r
if ("docstring" %in% installed.packages() == FALSE) {
  install.packages("docstring", dependencies = TRUE)
}
library(docstring)
```

# 2 GENERAL FUNCTIONS FOR NETWORK ANALYSIS

The functions in this section are uniquely used in the
[Static\_Network\_Analysis](/Static_Network_Analysis) directory to build
networks for different sub-periods, but are not used any more as they
have been integrated and improved in the
[biblionetwork](https://agoutsmedt.github.io/biblionetwork/index.html)
package.

## 2.1 `bibliographic_coupling()`

This function could now be replaced by
[biblio\_coupling()](https://agoutsmedt.github.io/biblionetwork/reference/biblio_coupling.html)
in the biblionetwork package.

``` r
bibliographic_coupling <- function(dt, source, ref, normalized_weight_only = TRUE, weight_threshold = 1, output_in_character = TRUE) {
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
  #' the column name of the references that are cited.
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
  setnames(dt, c(source, ref), c("id_art", "id_ref"))
  dt <- dt[, list(id_art, id_ref)]
  setkey(dt, id_ref, id_art)

  # removing duplicated citations with exactly the same source and target
  dt <- unique(dt)

  # remove loop
  dt <- dt[id_art != id_ref]

  # Removing references cited only once:
  dt <- dt[, N := .N, by = id_ref][N > 1][, list(id_art, id_ref)]

  # Computing how many items each citing document has (necessary for normalization later)
  id_nb_cit <- dt[, list(nb_cit = .N), by = id_art]

  # Creating every combinaison of articles per references
  bib_coup <- dt[, list(
    Target = rep(id_art[1:(length(id_art) - 1)], (length(id_art) - 1):1),
    Source = rev(id_art)[sequence((length(id_art) - 1):1)]
  ),
  by = id_ref
  ]

  # remove loop
  bib_coup <- bib_coup[Source != Target]

  # Calculating the weight
  bib_coup <- bib_coup[, .N, by = list(Target, Source)] # This is the number of go references

  # keeping edges over threshold
  bib_coup <- bib_coup[N >= weight_threshold]

  # We than do manipulations to normalize this number with the cosine measure
  bib_coup <- merge(bib_coup, id_nb_cit, by.x = "Target", by.y = "id_art")
  setnames(bib_coup, "nb_cit", "nb_cit_Target")
  bib_coup <- merge(bib_coup, id_nb_cit, by.x = "Source", by.y = "id_art")
  setnames(bib_coup, "nb_cit", "nb_cit_Source")
  bib_coup[, weight := N / sqrt(nb_cit_Target * nb_cit_Source)] # cosine measure

  # Renaming columns
  setnames(
    bib_coup, c("N"),
    c("nb_shared_references")
  )

  # Transforming the Source and Target columns in character (and keeping the Source and Target in copy)
  # Then selection which columns to return
  if (output_in_character == TRUE) {
    bib_coup$from <- as.character(bib_coup$Source)
    bib_coup$to <- as.character(bib_coup$Target)
    if (normalized_weight_only == TRUE) {
      return(bib_coup[, c("from", "to", "weight", "Source", "Target")])
    } else {
      return(bib_coup[, c("from", "to", "weight", "nb_shared_references", "Source", "Target")])
    }
  }
  else {
    if (normalized_weight_only == TRUE) {
      return(bib_coup[, c("Source", "Target", "weight")])
    } else {
      return(bib_coup[, c("Source", "Target", "weight", "nb_shared_references")])
    }
  }
}
```

## 2.2 `bibliographic_coupling_alt()`

This function could now be replaced by
[coupling\_strength()](https://agoutsmedt.github.io/biblionetwork/reference/coupling_strength.html)
in the biblionetwork package.

``` r
bibliographic_coupling_alt <- function(dt, source, ref, weight_threshold = 1) {
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
  #' the column name of the references that are cited.
  #'
  #' @param weight_threshold
  #' Correspond to the value of the non-normalized weights of edges. The function just keeps the edges
  #' that have a non-normalized weight superior to the `weight_threshold`.


  # Making sure the table is a datatable
  dt <- data.table(dt)
  # Renaming and simplifying
  setnames(dt, c(source, ref), c("id_art", "id_ref"))
  dt <- dt[, list(id_art, id_ref)]
  setkey(dt, id_ref, id_art)
  # removing duplicated citations with exactly the same source and target
  dt <- unique(dt)
  # remove loop
  dt <- dt[id_art != id_ref]
  # Removing references cited only once:
  dt <- dt[, N := .N, by = id_ref][N > 1][, list(id_art, id_ref)]
  # Computing how many items each citing document has (necessary for normalization later)
  id_nb_ref <- dt[, list(nb_ref = .N), by = id_art]
  # Computing how many times each cited document
  ref_nb_cit <- dt[, list(nb_cit = .N), by = id_ref]
  # Computing how many times each cited document
  nb_doc <- dt[unique(id_art)][, list(n_document = .N)]
  # Creating every combinaison of articles per references
  bib_coup <- dt[, list(
    Target = rep(id_art[1:(length(id_art) - 1)], (length(id_art) - 1):1),
    Source = rev(id_art)[sequence((length(id_art) - 1):1)]
  ),
  by = id_ref
  ]
  # remove loop
  bib_coup <- bib_coup[Source != Target]
  # Inverse Source and Target so that couple of Source/Target are always on the same side
  bib_coup <- bib_coup[Source > Target, c("Target", "Source") := list(Source, Target)] # exchanging

  ###### Add columns with info for weighting
  # Calculating the number of references in common and deleting the links between articles that share less than weight_threshold
  bib_coup <- bib_coup[, N := .N, by = list(Target, Source)][N >= weight_threshold]

  # nb_doc
  bib_coup[, nb_doc := nb_doc]
  # merge the number of occurence of a ref in a document
  bib_coup <- merge(bib_coup, ref_nb_cit, by = "id_ref")
  # merge the lenght of reference list
  bib_coup <- merge(bib_coup, id_nb_ref, by.x = "Target", by.y = "id_art")
  setnames(bib_coup, "nb_ref", "nb_ref_Target")
  bib_coup <- merge(bib_coup, id_nb_ref, by.x = "Source", by.y = "id_art")
  setnames(bib_coup, "nb_ref", "nb_ref_Source")

  # CS
  bib_coup[, weight := (sum(log(nb_doc / nb_cit))) / (nb_ref_Target * nb_ref_Source), .(Source, Target)]
  # Keep only unique couple
  # bib_coup <- bib_coup[, head(.SD, 1), .(Source,Target)]

  bib_coup$from <- as.character(bib_coup$Source)
  bib_coup$to <- as.character(bib_coup$Target)

  bib_coup <- unique(bib_coup[, c("from", "to", "weight", "Source", "Target")])

  return(bib_coup)
}
```

## 2.3 `bibliographic_coupling_authors()`

This function could now be replaced by
[coupling\_entity()](https://agoutsmedt.github.io/biblionetwork/reference/coupling_entity.html)
in the biblionetwork package.

``` r
bibliographic_coupling_authors <- function(dt, source, ref, authors, weight_threshold = 1) {
  #' function for edges of bibliographic coupling
  #'
  #' This function creates a network of authors from bibliographic coupling. Coupling links are calculated depending
  #' of the number references two authors share, taking into account the number of time each author cited each of
  #' these references, the total number of references cited by each author, and the total number of citations in the
  #' whole corpus of each reference.
  #'
  #' @param dt
  #' The table with citing and cited documents.
  #'
  #' @param source
  #' the column name of the source identifiers, that is the documents that are citing.
  #'
  #' @param ref
  #' the column name of the references that are cited.
  #'
  #' @param authors
  #' the column name of the authors that are citing.
  #'
  #' @param weight_threshold
  #' Correspond to the value of the non-normalized weights of edges. The function just keeps the edges
  #' that have a non-normalized weight superior to the `weight_threshold`.

  # Making sure the table is a datatable
  dt <- data.table(authors_edges)

  # Renaming, calculating number of articles and simplifying
  setnames(dt, c(source, authors, ref), c("id_art", "authors", "id_ref"))

  # Computing how many times each cited document
  nb_doc <- length(unique(dt[, id_art]))

  # Computing how many times each document is cited (by id_art, not by author, to avoid double-counting)
  ref_nb_cit <- unique(unique(dt[, c("id_art", "id_ref")])[, list(nb_cit = .N), by = id_ref])

  dt <- dt[, list(authors, id_ref)]
  setkey(dt, id_ref, authors)

  # calculating the number of ref per-author and the number of time a ref is cited by an author
  dt <- unique(dt[, nb_ref := .N, by = "authors"][, nb_cit_author := .N, by = c("authors", "id_ref")])



  # removing duplicated citations with exactly the same source and target
  # dt <- unique(dt)

  # Removing references cited only once by one author
  dt <- dt[, nb_cit_alt := .N, by = id_ref][nb_cit_alt > 1]
  # Computing how many items each citing document has (necessary for normalization later)
  # id_nb_ref <-  dt[,list(nb_ref = .N),by=authors]

  # Computing how many times each cited document
  # ref_nb_cit <-  dt[,list(nb_cit = .N),by=id_ref]


  # Creating every combinaison of articles per references
  dt_reduce <- dt[, list(authors, id_ref)]
  bib_coup <- dt_reduce[, list(
    Target = rep(authors[1:(length(authors) - 1)], (length(authors) - 1):1),
    Source = rev(authors)[sequence((length(authors) - 1):1)]
  ),
  by = id_ref
  ]
  # remove loop
  bib_coup <- bib_coup[Source != Target]
  # Inverse Source and Target so that couple of Source/Target are always on the same side
  bib_coup <- bib_coup[Source > Target, c("Target", "Source") := list(Source, Target)] # exchanging

  ###### Add columns with info for weighting
  # Calculating the number of references in common and deleting the links between articles that share less than weight_threshold
  bib_coup <- bib_coup[, N := .N, by = list(Target, Source)][N >= weight_threshold]

  # nb_doc
  bib_coup[, nb_doc := nb_doc]
  # merge the number of occurence of a ref in a document
  bib_coup <- merge(bib_coup, ref_nb_cit, by = "id_ref")
  # merge the lenght of reference list
  bib_coup <- merge(bib_coup, unique(dt[, c("authors", "nb_ref")]), by.x = "Target", by.y = "authors")
  setnames(bib_coup, "nb_ref", "nb_ref_Target")
  bib_coup <- merge(bib_coup, unique(dt[, c("authors", "nb_ref")]), by.x = "Source", by.y = "authors")
  setnames(bib_coup, "nb_ref", "nb_ref_Source")

  # merge the number of times a ref is cited by an author
  bib_coup <- merge(bib_coup, unique(dt[, c("authors", "id_ref", "nb_cit_author")]), by.x = c("Target", "id_ref"), by.y = c("authors", "id_ref"))
  setnames(bib_coup, "nb_cit_author", "nb_cit_author_Target")
  bib_coup <- merge(bib_coup, unique(dt[, c("authors", "id_ref", "nb_cit_author")]), by.x = c("Source", "id_ref"), by.y = c("authors", "id_ref"))
  setnames(bib_coup, "nb_cit_author", "nb_cit_author_Source")

  # CS
  bib_coup[, weight := (sum(min(nb_cit_author_Target, nb_cit_author_Source) * log(nb_doc / nb_cit))) / (nb_ref_Target * nb_ref_Source), .(Source, Target)]

  # Keep only unique couple
  # bib_coup <- bib_coup[, head(.SD, 1), .(Source,Target)]

  bib_coup$from <- as.character(bib_coup$Source)
  bib_coup$to <- as.character(bib_coup$Target)

  bib_coup <- unique(bib_coup[, c("from", "to", "weight", "Source", "Target")])

  return(bib_coup)
}
```

## 2.4 `bibliographic_cocitation()`

This function could now be replaced by
[biblio\_cocitation()](https://agoutsmedt.github.io/biblionetwork/reference/biblio_cocitation.html)
in the biblionetwork package.

``` r
bibliographic_cocitation <- function(dt, source, ref, normalized_weight_only = TRUE, weight_threshold = 1, output_in_character = TRUE) {
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
  #' the column name of the references that are cited.
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
  setnames(dt, c(source, ref), c("id_art", "id_ref"))
  dt <- dt[, list(id_art, id_ref)]
  setkey(dt, id_ref, id_art)

  # removing duplicated citations with exactly the same source and target
  dt <- unique(dt)

  # remove loop
  dt <- dt[source != ref]

  # Removing documents with only one reference:
  dt <- dt[, N := .N, by = id_art][N > 1][, list(id_art, id_ref)]

  # Computing how many items each cited document has (necessary for normalization later)
  id_nb_cit <- dt[, list(nb_cit = .N), by = id_ref]

  # Creating every combinaison of articles per references
  bib_cocit <- dt[, list(
    Target = rep(id_ref[1:(length(id_ref) - 1)], (length(id_ref) - 1):1),
    Source = rev(id_ref)[sequence((length(id_ref) - 1):1)]
  ),
  by = id_art
  ]


  # remove loop
  bib_cocit <- bib_cocit[Source != Target]

  # counting the number of identical links across citing articles
  bib_cocit <- bib_cocit[, .N, .(Source, Target)]
  # keeping edges over threshold
  bib_cocit <- bib_cocit[N >= weight_threshold]

  # We then do manipulations to normalize this number with the cosine measure
  bib_cocit <- merge(bib_cocit, id_nb_cit, by.x = "Target", by.y = "id_ref")
  setnames(bib_cocit, "nb_cit", "nb_cit_Target")
  bib_cocit <- merge(bib_cocit, id_nb_cit, by.x = "Source", by.y = "id_ref")
  setnames(bib_cocit, "nb_cit", "nb_cit_Source")
  bib_cocit[, weight := N / sqrt(nb_cit_Target * nb_cit_Source)] # cosine measure

  # Renaming columns
  setnames(
    bib_cocit, c("N"),
    c("nb_shared_references")
  )

  # Transforming the Source and Target columns in character (and keeping the Source and Target in copy)
  if (output_in_character == TRUE) {
    bib_cocit$from <- as.character(bib_cocit$Source)
    bib_cocit$to <- as.character(bib_cocit$Target)
    if (normalized_weight_only == TRUE) {
      return(bib_cocit[, c("from", "to", "weight", "Source", "Target")])
    } else {
      return(bib_cocit[, c("from", "to", "weight", "nb_shared_references", "Source", "Target")])
    }
  }
  else {
    if (normalized_weight_only == TRUE) {
      return(bib_cocit[, c("Source", "Target", "weight")])
    } else {
      return(bib_cocit[, c("Source", "Target", "weight", "nb_shared_references")])
    }
  }

  # Selecting which columns to return
  # if(normalized_weight_only==TRUE){
  #    return (bib_cocit[, c("from","to","weight","Source","Target")])
  #  } else {
  #    return (bib_cocit[, c("from","to","weight","nb_shared_references")])
  #  }
}
```

# 3 Building graphs - Basics

Some of the functions here are not necessary anymore as they have been
implemented and improved in the
[networkflow](https://github.com/agoutsmedt/networkflow) package.

## 3.1 `tbl_main_components()`

See now the
[`tbl_main_component()`](https://github.com/agoutsmedt/networkflow/blob/master/R/tbl_main_component.R)
function in networkflow package.

``` r
tbl_main_components <- function(edges, nodes, directed = FALSE, node_key = NULL, nb_components = 1, threshold_alert = 0.05) {
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
    rename_at(1, ~"Id") # renamed the first column to a standard format

  # looking at the biggest component just after the last one we have kept
  threshold <- graph %>%
    filter(components_att == nb_components + 1) %>%
    rename_at(1, ~"Id")

  # looking at the percentage of nodes in the biggest component just after the last one we have kept
  # trigger a warning if superior to the threshold_alert
  if (length(V(threshold)$Id) / length(V(graph)$Id) > threshold_alert) warning(paste0("Warning: you have removed a component gathering more than ", threshold_alert, "% of the nodes"))

  # keeping only the number of components we want
  graph <- graph %>%
    filter(components_att == nb_components) %>%
    select(-components_att) # we remove the column as it won't be useful after that
}

components_distribution <- function(edges, nodes, directed = FALSE, node_key = NULL) {
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
    rename_at(1, ~"Id") %>%
    as.data.table()

  Components_data <- Components_data[, N_nodes_components := .N, by = "components_att"][, N_nodes_total := .N][, percentage_nodes_components := N_nodes_components / N_nodes_total * 100]
  Components_data <- unique(Components_data[order(components_att), c("components_att", "N_nodes_components", "percentage_nodes_components")])
}

# Running Leiden for two different resolutions and associating edges to communities
leiden_improved <- function(graph, res_1 = 1, res_2 = NULL, res_3 = NULL, n_iterations = 500) {
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
  graph <- graph %>%
    activate(nodes) %>%
    mutate(Com_ID = sprintf("%02d", leiden)) %>%
    mutate(Com_ID = as.character(Com_ID))

  # calculate the size of the community
  graph <- graph %>%
    group_by(Com_ID) %>%
    mutate(Size_com = n()) %>%
    mutate(Size_com = Size_com / length(V(graph)$Com_ID)) %>%
    ungroup()

  # Add an attribute to edges, depending on the community of their nodes
  # (If communities are different between the two nodes, edges takes the total number of communities plus 1 as attribute)
  # (Another possibility in the future would be for edges with nodes from different communities to be the average of the
  # two communities number. It would allow the edges to take as color the mix of the two communities color)
  graph <- graph %>%
    activate(edges) %>%
    mutate(com_ID_to = .N()$Com_ID[to], com_ID_from = .N()$Com_ID[from], Com_ID = ifelse(com_ID_from == com_ID_to, com_ID_from, (max(leiden) + 1))) # .N() makes the node data available while manipulating edges


  # Doing the same for the second resolution
  if (is.null(res_2)) {
    return(graph)
  }
  if (!is.null(res_2)) {
    leiden_2 <- leiden(graph, resolution_parameter = res_2, n_iterations = n_iterations)
    graph <- graph %>%
      activate(nodes) %>%
      mutate(Com_ID_2 = sprintf("%02d", leiden)) %>%
      mutate(Com_ID_2 = as.character(Com_ID))

    graph <- graph %>%
      activate(edges) %>%
      mutate(Com_ID_2_to = .N()$Com_ID_2[to], Com_ID_2_from = .N()$Com_ID_2[from], Com_ID_2 = ifelse(Com_ID_2_from == Com_ID_2_to, Com_ID_2_from, (max(leiden_2) + 1)))
  }

  # Doing the same for the third resolution
  if (is.null(res_3)) {
    return(graph)
  }
  if (!is.null(res_3)) {
    leiden_3 <- leiden(graph, resolution_parameter = res_3, n_iterations = n_iterations)
    graph <- graph %>%
      activate(nodes) %>%
      mutate(Com_ID_3 = sprintf("%02d", leiden)) %>%
      mutate(Com_ID_3 = as.character(Com_ID))

    graph <- graph %>%
      activate(edges) %>%
      mutate(Com_ID_3_to = .N()$Com_ID_3[to], Com_ID_3_from = .N()$Com_ID_3[from], Com_ID_3 = ifelse(Com_ID_3_from == Com_ID_3_to, Com_ID_3_from, (max(leiden_3) + 1)))
  }
}

# function for integrating the color to the communities
community_colors <- function(graph, palette) {
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

  # Setup Colors
  color <- data.table(
    Com_ID = 1:500,
    color = palette
  )
  color <- color %>%
    mutate(Com_ID = sprintf("%02d", Com_ID)) %>%
    mutate(Com_ID = as.character(Com_ID))

  # Add color to nodes
  graph <- graph %>%
    activate(nodes) %>%
    left_join(color)
  # Mix color for edges of different color
  graph <- graph %>% # mix color
    activate(edges) %>%
    mutate(color_com_ID_to = .N()$color[to], color_com_ID_from = .N()$color[from]) %>%
    mutate(color_edges = MixColor(color_com_ID_to, color_com_ID_from, amount1 = 0.5))
}

# Computing different centrality stats
centrality <- function(graph) {
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
    mutate(
      Degree = centrality_degree(),
      Strength = centrality_degree(weights = weight),
      Closeness = centrality_closeness(weights = weight, normalized = TRUE),
      Eigen_centrality = centrality_eigen(weights = weight),
      Betweeness = centrality_betweenness()
    )
}

naming_communities <- function(graph, centrality_measure = "Strength", naming = "Label", Community = "Com_ID") {
  #' Naming communities
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
    activate(nodes) %>%
    as_tibble()

  # Changing the name of the variable chosen
  colnames(Community_names)[colnames(Community_names) == naming] <- "Label"
  colnames(Community_names)[colnames(Community_names) == Community] <- "Com_ID"
  colnames(Community_names)[colnames(Community_names) == centrality_measure] <- "Centrality"

  Community_names <- Community_names %>%
    arrange(Com_ID, desc(Centrality)) %>%
    mutate(Community_name = Label) %>%
    select(Community_name, Com_ID) %>%
    group_by(Com_ID) %>%
    slice(1) %>%
    mutate(Community_name = paste0(Com_ID, "-", Community_name))

  # adding the name as an attribute to the nodes.
  graph <- graph %>%
    activate(nodes) %>%
    inner_join(Community_names, by = "Com_ID")

  # Reproducing the same operation for Com_ID_2 if it exists
  if (!is.null(V(graph)$Com_ID_2)) {
    Community_names <- graph %>%
      activate(nodes) %>%
      rename(
        Centrality = centrality_measure,
        Label = naming
      ) %>%
      as_tibble() %>%
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
  if (!is.null(V(graph)$Com_ID_3)) {
    Community_names <- graph %>%
      activate(nodes) %>%
      rename(
        Centrality = centrality_measure,
        Label = naming
      ) %>%
      as_tibble() %>%
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
force_atlas <- function(graph, seed = NULL, ew.influence = 1, kgrav = 1, iter_1 = 5000, iter_2 = 200, barnes.hut = FALSE, change_size = TRUE, size_min = 10, size_max = 50) {
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
  #' @param change_size If `TRUE`, it activates the normalization of the size of nodes for the non-overlapping algorithm.
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

  if (change_size == TRUE) {
    graph <- graph %>%
      activate(nodes) %>%
      mutate(size = ((size - min(size)) / (max(size) - min(size))) * (size_max - size_min) + size_min)
  }
  # running FA for the first time (without prevent.overlap)
  set.seed(seed)
  fa <- layout_forceatlas2(graph,
    ew.influence = ew.influence, kgrav = kgrav, iter = iter_1,
    prevent.overlap = FALSE, fixed = NULL, stopping.tolerance = 0.001,
    barnes.hut = barnes.hut
  )

  fa <- as.data.table(fa$lay)
  graph <- graph %>%
    activate(nodes) %>%
    mutate(x = fa$x, y = fa$y)

  # Force Atlas 2 algorithm for the graph (with prevent.overlap)
  set.seed(seed)
  fa <- layout_forceatlas2(graph,
    ew.influence = ew.influence, kgrav = kgrav, iter = iter_2,
    prevent.overlap = TRUE, fixed = NULL, stopping.tolerance = 0.001,
    barnes.hut = barnes.hut
  )

  fa <- as.data.table(fa$lay)
  graph <- graph %>%
    activate(nodes) %>%
    mutate(x = fa$x, y = fa$y)
}

# Simple functions for keeping a number n of nodes with highest citations measures per communities
top_ordering <- function(graph, ordering_column = "nb_cit", top_n = 20, top_n_com = 1, biggest_community = FALSE, community_threshold = 0.01) {
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
  colnames(top_variable_com)[colnames(top_variable_com) == ordering_column] <- "ordering_column"

  # Keeping only the biggest communites if the parameter is TRUE
  if (biggest_community == TRUE) {
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

  colnames(top_variable_general)[colnames(top_variable_general) == ordering_column] <- "ordering_column"

  top_variable_general <- top_variable_general %>%
    arrange(desc(ordering_column)) %>%
    slice(1:top_n) %>%
    as.data.table()

  # adding the two and removing the doublons
  top_variable_sum <- unique(rbind(top_variable_general, top_variable_com))

  colnames(top_variable_sum)[colnames(top_variable_general) == "ordering_column"] <- ordering_column

  return(top_variable_sum)
}

# Simple functions for keeping a number n of nodes with highest citations measures per communities
label_com <- function(graph, biggest_community = FALSE, community_threshold = 0.01, community_name_column = "Community_name", community_size_column = "Size_com") {
  #' Displaying the highest cited nodes
  #'
  #' A simple function to calculate the mean of coordinates x and y for each community. These coordinates
  #' are to be used to plot the name of the community on the graph.
  #'
  #' @param graph
  #' A tidygraph object.
  #'
  #' @param biggest_community
  #' If true, you have the possibility to remove the smallest community, depending of the `community_threshold`
  #' you have set.
  #'
  #' @param community_threshold
  #' If `biggest_community` is true, the function selects the nodes that belong to communities which represent
  #' at least x% of the total number of nodes. By default, the parameter is set to 1%.
  #'
  #' @param community_name_column
  #' Name of the column with the name of the community to be used as label
  #'
  #' @param community_size_column
  #' Name of the column with the share of nodes in each community.


  # Top nodes per community for the variable chosen
  label_com <- graph %>%
    activate(nodes) %>%
    as_tibble()

  # Changing the name of the variable chosen
  colnames(label_com)[colnames(label_com) == community_name_column] <- "Community_name"
  colnames(label_com)[colnames(label_com) == community_size_column] <- "Size_com"

  # Keeping only the biggest communites if the parameter is TRUE
  if (biggest_community == TRUE) {
    label_com <- label_com %>%
      filter(Size_com > community_threshold)
  }

  # Keeping the n top nodes per community
  label_com <- label_com %>%
    group_by(Community_name) %>%
    mutate(x = mean(x), y = mean(y)) %>%
    select(Community_name, x, y, color, Size_com) %>%
    as_tibble() %>%
    unique()

  return(label_com)
}
```

# 4 Building Graphs - Secondary and to be improved

This function takes as input a tidygraph object, it switches the names
of individual nodes by the name of their community, calculate the number
of nodes in the community, and transforms the whole community as a
unique node.

``` r
graph_community <- function(graph, Community_name = "Community_name", nb_components = 1, preparing_graph = TRUE) {
  #' Function for building of graph with community as nodes
  #'
  #' This function takes as input a tidygraph object, it switches the names of individual nodes
  #' by the name of their community, calculate the number of nodes in the community, and
  #' transforms the whole community as a unique node.
  #'
  #' @param graph
  #' A tidygraph object.
  #'
  #' @param Community_name
  #' Select the column that you want to be used to name the community. By default, the function
  #' considers that you have a column called `Community_name`. Be careful to choose a variable that
  #' identifies all members of a community share. It could be the number of the community, or the
  #' name of the node with the highest degree, number of citations, etc...
  #'
  #' @param nb_components
  #' The number of components you want to keep in the network. Usually, as nodes are community, it is
  #' likely that no node is isolated from the rest of the network.
  #'
  #' @param preparing_graph
  #' If `TRUE`, the function prepares the graph to be plotted with ggraph. It attributes color to the name
  #' of the nodes, depending of the color attributed in the original graph (the tidygraph use in the parameter
  #' `graph`). The `graph` which serves as input thus need to already have a `color` column. The function
  #' then run the Force Atlas algorithm to calculate the position of nodes.
  #'
  #' @details
  #' For running the Force Atlas, we have standard parameters that are not modifiable via the parameters of the
  #' function, as in general one doesn't have more than 40/50 communities and that you don't need
  #' to adjust the parameters. If one has many communities and one wants to find the communities of these
  #' communities as node, it is possible to do out from the output of the function.
  #'
  #' @details
  #' The links between nodes(ie communities) are normalized such as not being overestimated for bigger
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
    mutate(Com_name_from = .N()$Community_name[from], Com_name_to = .N()$Community_name[to]) %>%
    as_tibble() %>%
    select(Com_name_from, Com_name_to, weight) %>%
    group_by(Com_name_from, Com_name_to) %>%
    mutate(weight = sum(weight)) %>%
    unique() %>%
    as.data.table()

  # calculating the sum of weigths for each node (i.e. each community), that is the sum
  # of the weights of all the node edges
  Weight_com1 <- Edges_Com[Com_name_from != Com_name_to, c("Com_name_from", "weight")]
  Weight_com2 <- Edges_Com[Com_name_from != Com_name_to, c("Com_name_to", "weight")]
  Weight_com3 <- Edges_Com[Com_name_from == Com_name_to, c("Com_name_from", "weight")]
  Weight_com <- rbind(Weight_com1, Weight_com2, Weight_com3, use.names = FALSE)
  Weight_com <- Weight_com[, Com_weight := sum(weight), by = "Com_name_from"]
  colnames(Weight_com)[1] <- "Com_name"
  Weight_com <- unique(Weight_com[, c("Com_name", "Com_weight")])

  # merging the two nodes of each edge with their respective total weight
  Edges_Com <- merge(Edges_Com, Weight_com, by.x = "Com_name_from", by.y = "Com_name")
  Edges_Com <- merge(Edges_Com, Weight_com, by.x = "Com_name_to", by.y = "Com_name")
  Edges_Com <- Edges_Com[, c("Com_name_from", "Com_name_to", "weight", "Com_weight.x", "Com_weight.y")]
  colnames(Edges_Com) <- c("from", "to", "weight", "Com_weight_from", "Com_weight_to")

  # Cosine normalization of the edges weights
  Edges_Com <- Edges_Com[, weight := weight / sqrt(Com_weight_from * Com_weight_to), by = c("from", "to")]

  # using our tbl_main_components function to build the tidygraph with a main component
  graph_community <- tbl_main_components(Edges_Com, Nodes_Com, node_key = "name", nb_components = nb_components)

  if (preparing_graph == TRUE) {
    # merging with the colors attributed to community before
    color_community <- graph %>%
      activate(nodes) %>%
      as_tibble() %>%
      select(Community_name, color) %>%
      unique()

    graph_community <- graph_community %>%
      activate(nodes) %>%
      left_join(color_community, by = c("Id" = "Community_name"))

    # Integration a size variable for implementing non-overlapping function of Force Atlas
    graph_community <- graph_community %>%
      activate(nodes) %>%
      mutate(size = nb_nodes)

    # Running Force Atlas layout
    graph_community <- force_atlas(graph_community, seed = NULL, ew.influence = 1, kgrav = 1, iter_1 = 6000, iter_2 = 2000, barnes.hut = FALSE, size_min = 50, size_max = 200)
  }
}


# A function to concentrate nodes with a similar attribute in one singular node and build the corresponding graph
graph_from_attribute <- function(nodes, edges, palette, Attribute_name, nb_components = 1, preparing_graph = TRUE, size_min = 50, size_max = 200) {
  #' Function for building of graph with an attribute as nodes
  #'
  #' This function takes as input a tidygraph object, it switches the names of individual nodes
  #' by one of their attribute, calculate the number of nodes having this attribute, and
  #' transforms all the nodes with this attribute as a unique node.
  #'
  #' @param nodes
  #' The list of the nodes of your network.
  #'
  #' @param edges
  #' The list of the edges of your network.
  #'
  #' @param palette
  #' A color palette that will be used to attribute color to the communities calculated
  #' by the Leiden algorithm
  #'
  #' @param Attribute_name
  #' Select the column that you want to be used for aggregating the nodes.
  #'
  #' @param nb_components
  #' The number of components you want to keep in the network. Usually, as nodes are community, it is
  #' likely that no node is isolated from the rest of the network.
  #'
  #' @param preparing_graph
  #' If `TRUE`, the function prepares the graph to be plotted with ggraph. It will run Leiden, attribute
  #' color to the community, calculate some centrality measures and then run the Force Atlast
  #' algorithm to calculate the position of nodes.
  #'
  #' @param size_min
  #' A parameter used in the Force Atlas algorithm to avoid the overlappping of nodes. See the
  #' `force_atlas` function for documentation.
  #'
  #' @param size_max
  #' See `size_min`.
  #'

  colnames(nodes)[colnames(nodes) == Attribute_name] <- "Attribute_name"
  graph <- tbl_main_components(edges = edges, nodes = nodes, node_key = "ID_Art", threshold_alert = 0.05, directed = FALSE)

  Nodes_Com <- graph %>%
    activate(nodes) %>%
    as_tibble() %>%
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
    mutate(Att_name_from = .N()$Attribute_name[from], Att_name_to = .N()$Attribute_name[to]) %>%
    as_tibble() %>%
    select(Att_name_from, Att_name_to, weight) %>%
    group_by(Att_name_from, Att_name_to) %>%
    mutate(weight = sum(weight)) %>%
    unique() %>%
    as.data.table()

  # calculating the sum of weigths for each node (i.e. each community), that is the sum
  # of the weights of all the node edges
  Weight_com1 <- Edges_Com[Att_name_from != Att_name_to, c("Att_name_from", "weight")]
  Weight_com2 <- Edges_Com[Att_name_from != Att_name_to, c("Att_name_to", "weight")]
  Weight_com3 <- Edges_Com[Att_name_from == Att_name_to, c("Att_name_from", "weight")]
  Weight_com <- rbind(Weight_com1, Weight_com2, Weight_com3, use.names = FALSE)
  Weight_com <- Weight_com[, Com_weight := sum(weight), by = "Att_name_from"]
  colnames(Weight_com)[1] <- "Att_name"
  Weight_com <- unique(Weight_com[, c("Att_name", "Com_weight")])

  # merging the two nodes of each edge with their respective total weight
  Edges_Com <- merge(Edges_Com, Weight_com, by.x = "Att_name_from", by.y = "Att_name")
  Edges_Com <- merge(Edges_Com, Weight_com, by.x = "Att_name_to", by.y = "Att_name")
  Edges_Com <- Edges_Com[, c("Att_name_from", "Att_name_to", "weight", "Com_weight.x", "Com_weight.y")]
  colnames(Edges_Com) <- c("from", "to", "weight", "Att_name_from", "Att_name_to")

  # Cosine normalization of the edges weights
  Edges_Com <- Edges_Com[, weight := weight / sqrt(Att_name_from * Att_name_to), by = c("from", "to")]

  # using our tbl_main_components function to build the tidygraph with a main component
  graph_community <- tbl_main_components(Edges_Com, Nodes_Com, node_key = "name", nb_components = nb_components)

  if (preparing_graph == TRUE) {
    # Identifying communities with Leiden algorithm
    graph_community <- leiden_improved(graph_community, res_1 = 1, res_2 = NULL, res_3 = NULL, n_iterations = 500)

    # Giving colors to communities
    graph_community <- community_colors(graph_community, palette)

    # Calculating different centrality measures
    graph_community <- centrality(graph_community)

    # Integration a size variable for implementing non-overlapping function of Force Atlas
    graph_community <- graph_community %>%
      activate(nodes) %>%
      mutate(size = Size_att)

    # Running Force Atlas layout
    graph_community <- force_atlas(graph_community, seed = NULL, ew.influence = 1, kgrav = 1, iter_1 = 6000, iter_2 = 2000, barnes.hut = TRUE, size_min = size_min, size_max = size_max)
  }
}

clustering_communities <- function(graph, label_size = 6, number_size = 6, threshold_com = 0.01) {
  #' Function for building a heatmap of the communities
  #'
  #' This function takes as input a tidygraph object with communities as nodes and produce a heatmap of the links
  #' between communities, and a dendrogram of these communities.
  #'
  #' @param graph
  #' A tidygraph object with nodes being communities and a column `Size_com` which represent the percentage of
  #' total nodes in the community
  #'
  #' @param label_size
  #' The size of the labels displayed in the heatmap plot
  #'
  #' #' @param threshold_com
  #' The minimun percentage of nodes in the community for the community to be displayed on the plot.
  #'
  #' @section Future improvements:
  #' Find a way to plot only the biggest communities, but by removing the communities before the plotting, not to
  #' biased the values.
  #'
  #' @section Future improvements:
  #' Using a different method for plotting, by mixing ggplot and ggraph for the dendrogram, to have more options.

  # Extracting edges with only the biggest communities, and integrating the name of communities for source and target of edges
  edges <- graph %>%
    activate(edges) %>%
    mutate(com_name_to = .N()$Id[to], com_name_from = .N()$Id[from]) %>%
    as.data.table()

  # making the matrix from the edges
  matrix <- as.matrix(get.adjacency(graph.data.frame(edges[, c("com_name_to", "com_name_from", "weight")], directed = FALSE), type = "both", attr = "weight"))

  # clustering and creation of a dendrogram from the Matrix.
  dendro <- as.dendrogram(hclust(dist(matrix)))
  plot_dendro <- ggdendrogram(dendro, rotate = TRUE) # saving the plot of the dendrogram
  order_dendro <- order.dendrogram(dendro) # extracting the order of nodes

  # keeping only biggest communities
  nodes <- graph %>%
    activate(nodes) %>%
    as.data.table()

  nodes <- nodes[, total := sum(nb_nodes)][, share_com := nb_nodes / total][share_com > threshold_com, "Id"]

  # cleaning the matrix for plotting the heatmap
  matrix <- scale(matrix, center = FALSE, scale = colSums(matrix))
  matrix <- melt(matrix) %>% as.data.table()
  matrix$value <- matrix$value * 100
  matrix$value <- round(matrix$value, digits = 1)
  matrix <- matrix[matrix$Var1 %in% nodes$Id & matrix$Var2 %in% nodes$Id]
  matrix$Var1 <- str_wrap(matrix$Var1, width = 10)
  matrix$Var2 <- str_wrap(matrix$Var2, width = 200)

  # ordering the nodes depending of the order of the dendrogram
  matrix$Var1 <- factor(
    x = matrix$Var1,
    levels = unique(matrix$Var1)[order_dendro],
    ordered = TRUE
  )
  matrix$Var2 <- factor(
    x = matrix$Var2,
    levels = unique(matrix$Var2)[order_dendro],
    ordered = TRUE
  )

  # saving the heat map
  plot_heatmap <- ggplot(matrix, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(show.legend = FALSE) +
    theme(text = element_text(size = label_size)) +
    geom_text(aes(x = Var1, y = Var2, label = value), color = "black", size = number_size) +
    scale_fill_viridis(discrete = FALSE) +
    ylab("In the cluster...") +
    xlab("...X% of links goes to")

  list_return <- list("heatmap" = plot_heatmap, "dendrogram" = plot_dendro)
  return(list_return)
}

# Simple functions for keeping a number n of nodes with highest centrality measures per communities
top_centrality_com <- function(graph, centrality, top_n = 3) {
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
important_nodes <- function(graph, top_n = 3) {
  strength_range <- as_tibble(graph) %>%
    arrange(desc(Strength)) %>%
    group_by(Com_ID) %>%
    slice(1:top_n)

  betweeness_range <- as_tibble(graph) %>%
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
```

# 5 Dynamic networks: building the different lists

## 5.1 `dynamic_biblio_coupling()`

This function creates a list of tbl graph from a corpus and its
references in a direct citation data frame (the list of the references
cited by each document) of the corpus. You can set different types of
time windows, use different types of coupling methods, as well as to
choose different methods to reduce the number of nodes and edges if you
want to avoid creating too large networks.

``` r
dynamic_biblio_coupling <- function(corpus, 
                                    direct_citation_dt, 
                                    source = "ID_Art",
                                    source_as_ref = "ItemID_Ref",
                                    ref = "New_id2", 
                                    time_variable = "Annee_Bibliographique",
                                    coupling_method = c("coupling_angle","coupling_strength","coupling_similarity"),
                                    time_window_length = 5,
                                    time_window_move = 0,
                                    weight_treshold = 1,
                                    nodes_threshold = 0,
                                    controlling_nodes = FALSE,
                                    controlling_edges = FALSE,
                                    nodes_limit = 10000,
                                    edges_limit = 300000,
                                    distribution_pruning = FALSE,
                                    quantile_threshold = 1,
                                    quantile_move = 0){  
  #' Creating a List of Tidygraph Graph for Different Time Windows
  #' 
  #' @description 
  #' This function creates a list of tbl graph from a corpus and its references in a direct
  #' citation data frame (the list of the references cited by each document) of the corpus.
  #' You can set different types of time windows, use different types of coupling methods,
  #' as well as to choose different methods to reduce the number of nodes and edges if you
  #' want to avoid creating too large networks. 
  #' 
  #' @param corpus
  #' The corpus with all your citing documents. This data frame will be used as the `nodes`
  #' to put in your network. 
  #' 
  #' @param direct_citation_dt
  #' The list of all the citations of references by the documents of `corpus`. That is the
  #' data frame that will be used to find the edges list and calculate the weights.
  #'
  #' @param source
  #' The column name of the identifier of citing articles in the `direct_citation_dt`. 
  #' That is the identifier that  will be use in the `biblio_coupling()` or the 
  #' `coupling_strength()` functions as the `source` parameter to do the coupling.
  #'
  #' @param source_as_ref
  #' The column name of the second identifier of citing artices. It is used when you have 
  #' to merge the `corpus` and `direct_citation_dt` through cited documents (not citing).
  #' It is used, for instance, when you want to calculate the number of times an article 
  #' in your `corpus` is cited as a reference in the `direct_citation_dt`. It could be the 
  #' same as the `ref` parameter. In other words, that is an identifier of the references
  #' (in the `direct_citation_dt`) that are also in the `corpus`.   
  #' 
  #' @param ref 
  #' The column name of the identifier of cited references. That is the column is in the
  #' `biblio_coupling()` or `coupling_strength()` function as the `ref` parameter to do
  #' the coupling. 
  #' 
  #' @time_variable
  #' Variable that identify the year of publication
  #' 
  #' @time_window
  #' How much time should be covered by the network
  #' 
  #' @weight_treshold_value
  #' Treshold value for coupling (see function)
  if(!coupling_method %in% c("coupling_angle","coupling_strength","coupling_similarity")) 
    stop('You did not choose a proper method for coupling computation. You have to choose between:\n - coupling_angle\n - coupling_strength\n - coupling_similarity')
  
  
  nodes <- as.data.table(corpus)
  nodes <- nodes[, `:=` (source = as.character(get(source)), 
                         time_variable = as.character(get(time_variable)),
                         source_as_ref = as.character(get(source_as_ref)))]
  
  
  direct_citation_dt <- as.data.table(direct_citation_dt)
  direct_citation_dt <- direct_citation_dt[,`:=` (source = as.character(get(source)),
                                                  ref = as.character(get(ref)),
                                                  source_as_ref = as.character(get(source_as_ref)))]
  direct_citation_dt <- direct_citation_dt[ref!="NULL"]
  
  ######################### Dynamics networks **********************
  #nodes <- nodes[order(time_variable)]
  
  # Find the time_window
  first_year <- as.integer(min(nodes$time_variable))
  last_year <- (as.integer(max(nodes$time_variable)) - time_window_length +1) # +1 to get the very last year in the window
  all_years <- first_year:last_year
  
  # Prepare our list
  tbl_list <- list()
  
  for (Year in all_years) {
    
    # fixing the initial value of the threshold
    edges_threshold <- weight_treshold
    threshold_2 <- nodes_threshold
    
    if(Year > first_year){
      quantile_threshold <- (quantile_threshold - quantile_move)
      quantile <- quantile_threshold
    } else {
      quantile <- quantile_threshold
    }
    
    message(paste0("- Creation of the network for the ", Year, "-", Year + time_window - 1, " window."))
    nodes_of_the_year <- nodes[time_variable >= (Year + time_window_move) & 
                                 time_variable < (Year + time_window_move + time_window)] # < for time_window being the number of years, note the value of the addition
    edges_of_the_year <- direct_citation_dt[source %in% nodes_of_the_year$source]
    # size of nodes
    nb_cit <- edges_of_the_year[, .N, source_as_ref]
    colnames(nb_cit)[colnames(nb_cit) == "N"] <- "nb_cit"
    # Tricky things here: you need the to take the identifier which is both in direct citation dataframe and in corpus.
    # That is the `source_as_ref` parameter in the function. In WoS, that is "ItemID_Ref". 
    # But we are using the "New_id2" variable in our database to do the coupling, this is 
    # why we have the `ref` variable.
    nodes_of_the_year <- merge(nodes_of_the_year, nb_cit, by = "source_as_ref", all.x = TRUE)
    nodes_of_the_year[is.na(nb_cit),nb_cit:=0]
    
    # coupling
    if(coupling_method == "coupling_angle" ){
      message(paste("The method use for bibliometric coupling is the coupling angle method. The edge threshold is:",edges_threshold))
      edges <- biblionetwork::biblio_coupling(dt = edges_of_the_year, 
                                              source = source, 
                                              ref = ref, 
                                              weight_threshold = edges_threshold, 
                                              output_in_character = TRUE)
    }else{
      if(coupling_method == "coupling_strength" ){
        message(paste("The method use for bibliometric coupling is the coupling strength method.The edge threshold is:",edges_threshold))
        edges <- biblionetwork::coupling_strength(dt = edges_of_the_year, 
                                                  source = source, 
                                                  ref = ref,  
                                                  weight_threshold = edges_threshold, 
                                                  output_in_character = TRUE)
      }else{
        message(paste("The method use for bibliometric coupling is the coupling similarity method.The edge threshold is:",edges_threshold))
        edges <- biblionetwork::coupling_similarity(dt = edges_of_the_year, 
                                                    source = source, 
                                                    ref = ref,  
                                                    weight_threshold = edges_threshold, 
                                                    output_in_character = TRUE) 
      }
    }
    
    if (distribution_pruning == TRUE) {
      message(paste("Pruning edges by distribution criterium. We are keeping the ",quantile*100," percent of edges with the highest weight value."))
      edges_prob_threshold <- quantile(edges$weight, prob = (1 - quantile))
      edges <- edges[weight >= edges_prob_threshold]
    }
    
    # loop for reducing the number of edges
    if (controlling_edges == TRUE){
      if (length(edges$from) > edges_limit) {
        for (k in 1:100) {
          message(paste("There are ", length(edges$from)," edges in the network. Superior to the ",edges_limit," edges limit."))
          
          if(distribution_pruning == FALSE){
            edges_threshold <- edges_threshold + 1
          } else {
            quantile <- quantile - 0.02
          }
          
          # coupling
          if(coupling_method == "coupling_angle" ){
            message(paste("Round ",k,": The method use for bibliometric coupling is the coupling angle method. The edge threshold is:",edges_threshold))
            edges <- biblionetwork::biblio_coupling(dt = edges_of_the_year, 
                                                    source = source, 
                                                    ref = ref, 
                                                    weight_threshold = edges_threshold, 
                                                    output_in_character = TRUE)
          }else{
            if(coupling_method == "coupling_strength" ){
              message(paste("Round ",k,": The method use for bibliometric coupling is the coupling strength method. The edge threshold is:",edges_threshold))
              edges <- biblionetwork::coupling_strength(dt = edges_of_the_year, 
                                                        source = source, 
                                                        ref = ref,  
                                                        weight_threshold = edges_threshold, 
                                                        output_in_character = TRUE)
            }else{
              message(paste("Round ",k,": The method use for bibliometric coupling is the coupling similarity method. The edge threshold is:",edges_threshold))
              edges <- biblionetwork::coupling_similarity(dt = edges_of_the_year, 
                                                          source = source, 
                                                          ref = ref,  
                                                          weight_threshold = edges_threshold, 
                                                          output_in_character = TRUE) 
            }
          }
          
          if (distribution_pruning == TRUE) {
            message(paste("Round ",k,": Pruning edges by distribution criterium to reduce the number of edges:\nWe are keeping the ",quantile*100," percent of edges with the highest weight value."))
            edges_prob_threshold <- quantile(edges$weight, prob = (1 - quantile))
            edges <- edges[weight >= edges_prob_threshold]
          }
          if (length(edges$from) < edges_limit) {
            message(paste("Round ",k,": Eventually, we have kept ",quantile*100," percent of edges with the highest weight value."))
            break
          }
        }
      }
    }
    
    # remove nodes with no edges
    nodes_of_the_year <- nodes_of_the_year[ID_Art %in% edges$from | ID_Art %in% edges$to]
    
    # Loop to avoid to large networks - Step 2: reducing nodes
    if (controlling_nodes == TRUE){
      if (length(nodes_of_the_year$source) > nodes_limit) {
        for (j in 1:100) {
          message(paste("There are ", length(nodes_of_the_year$source)," nodes in the network. Superior to the ",nodes_limit," nodes limit."))
          threshold_2 <- threshold_2 + 1
          # creating nodes
          nodes_of_the_year <- nodes_of_the_year[nb_cit >= threshold_2]
          edges_of_the_year <- direct_citation_dt[source %in% nodes_of_the_year$source]
          
          # coupling
          if(coupling_method == "coupling_angle" ){
            message(paste("Round ",j,": The method use for bibliometric coupling is the coupling angle method. The nodes threshold is:",threshold_2))
            edges <- biblionetwork::biblio_coupling(dt = edges_of_the_year, 
                                                    source = source, 
                                                    ref = ref, 
                                                    weight_threshold = edges_threshold, 
                                                    output_in_character = TRUE)
          }else{
            if(coupling_method == "coupling_strength" ){
              message(paste("Round ",j,": The method use for bibliometric coupling is the coupling strength method. The nodes threshold is:",threshold_2))
              edges <- biblionetwork::coupling_strength(dt = edges_of_the_year, 
                                                        source = source, 
                                                        ref = ref,  
                                                        weight_threshold = edges_threshold, 
                                                        output_in_character = TRUE)
            }else{
              message(paste("Round ",j,": The method use for bibliometric coupling is the coupling similarity method. The nodes threshold is:",threshold_2))
              edges <- biblionetwork::coupling_similarity(dt = edges_of_the_year, 
                                                          source = source, 
                                                          ref = ref,  
                                                          weight_threshold = edges_threshold, 
                                                          output_in_character = TRUE) 
            }
          }
          if (distribution_pruning == TRUE) {
            message(paste("Round ",j,": Keeping the same distribution criteria for edges:\nWe are keeping the ",quantile*100," percent of edges with the highest weight value."))
            edges_prob_threshold <- quantile(edges$weight, prob = (1 - quantile))
            edges <- edges[weight >= edges_prob_threshold]
          }
          if (length(nodes_of_the_year$source) < nodes_limit) {
            break
          }
        }
      }
      # remove nodes with no edges
      nodes_of_the_year <- nodes_of_the_year[source %in% edges$from | source %in% edges$to]
    }
    
    # giving threshold
    message(paste0("The final threshold for edges is:", edges_threshold))
    edges$threshold <- edges_threshold
    message(paste0("The final threshold for nodes is:", threshold_2))
    nodes_of_the_year$threshold <- threshold_2
    
    # make tbl
    tbl_list[[as.character(Year)]] <- tbl_graph(nodes = nodes_of_the_year, edges = edges, directed = FALSE, node_key = source)
  }
  
  return (tbl_list)
}
```

# 6 Functions for word analysis (titles) of networks

## 6.1 `tf_idf()`

This function takes as input a tidygraph object or a data frame with
nodes, both with a community attribute, and analyzes the words use in
the title of the articles to calculate the words with the highest TF-IDF
value for each community.

``` r
tf_idf <- function(graph = NULL, nodes = NULL, title_column = "Titre", com_column = "Com_ID", color_column = "color",
                   com_name_column = "Community_name", com_size_column = "Size_com", treshold_com = 0.01, number_of_words = 12,
                   palette = NULL, size_title_wrap = 8, unstemming = TRUE) {
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
  #' @param palette
  #' If you don't already have a color attribute for your communities in your tidygraph object,
  #' the function will generate one from a palette that you can add in the paramaters (NULL by default).
  #'
  #' @param size_title_wrap
  #' The size of the community title in the plot.
  #' 
  #' @param unstemming
  #' Chose whether you want to unstem or not the words

  # extracting the nodes
  if (!is.null(graph)) {
    tf_idf_save <- graph %>%
      activate(nodes) %>%
      as.data.table()
  }
  else {
    tf_idf_save <- nodes %>% as.data.table()
  }

  # changing the names of the column for titles and communities
  colnames(tf_idf_save)[colnames(tf_idf_save) == com_column] <- "Com_ID"
  colnames(tf_idf_save)[colnames(tf_idf_save) == title_column] <- "Titre"
  colnames(tf_idf_save)[colnames(tf_idf_save) == color_column] <- "color"
  colnames(tf_idf_save)[colnames(tf_idf_save) == com_name_column] <- "Community_name"
  colnames(tf_idf_save)[colnames(tf_idf_save) == com_size_column] <- "Size_com"



  # adding a color column attribute in case it doesn't exist
  if (colnames(tf_idf_save)[colnames(tf_idf_save) == "color"] != "color") {
    color <- data.table(
      Com_ID = 1:500,
      color = mypalette
    )
    color <- color %>%
      mutate(Com_ID = sprintf("%02d", Com_ID)) %>%
      mutate(Com_ID = as.character(Com_ID))

    tf_idf <- merge(tf_idf_save, color, by = "Com_ID", all.x = TRUE)
  }

  tf_idf <- tf_idf_save # we will need tf_idf_save later
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Unigram ####
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Cleaning the titles

  tf_idf <- tf_idf[Titre != "NULL"]
  tf_idf[, Titre := removeWords(Titre, stopwords("english"))]
  tf_idf[, Titre := stripWhitespace(Titre)]
  tf_idf[, Titre := removePunctuation(Titre)]
  tf_idf[, Titre := removeNumbers(Titre)]
  tf_idf[, Titre := tolower(Titre)]
  tf_idf[, Titre := removeWords(Titre, stopwords("english"))]
  tf_idf[, Titre := as.character(Titre)]
  tible_tf_idf <- tf_idf[, paste(Titre, collapse = " "), by = "Com_ID"]
  tible_tf_idf[, V1 := stripWhitespace(V1)]
  # Dictionnary to find the root of stem word before stemming
  dictionary <- tible_tf_idf
  dictionary <- dictionary %>% unnest_tokens(word, V1) %>% as.data.table()
  tible_tf_idf[, V1 := stemDocument(V1)]
  # tf-idf using quanteda
  tible_tf_idf <- corpus(tible_tf_idf, text_field = "V1")
  tible_tf_idf <- dfm(tible_tf_idf)
  tible_tf_idf <- dfm_tfidf(tible_tf_idf)
  # Keep column of names
  documents_names <- cbind(docvars(tible_tf_idf), quanteda::convert(tible_tf_idf, to = "data.frame")) %>% as.data.table()
  tible_tf_idf <- tidy(tible_tf_idf) %>% as.data.table()
  tf_idf_table <- merge(tible_tf_idf, documents_names[, .(doc_id, Com_ID)], by.x = "document", by.y = "doc_id")
  
  if(unstemming==TRUE){
    tf_idf_table[,unstemmed_word:=stemCompletion(tf_idf_table$term, dictionary$word, type = "prevalent")] # unstem with most common word
    tf_idf_table[unstemmed_word=="",unstemmed_word:=term] # unstem with most common word
  }
  if(unstemming==FALSE){
    tf_idf_table[,unstemmed_word:=term]
  }

  tf_idf_table[, term := unstemmed_word]
  tf_idf_table_uni <- tf_idf_table

  
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Bigram ####
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # cleaning

  tf_idf <- tf_idf_save[Titre != "NULL"]
  tf_idf[, Titre := removeWords(Titre, stopwords("english"))]
  tf_idf[, Titre := stripWhitespace(Titre)]
  tf_idf[, Titre := removePunctuation(Titre)]
  tf_idf[, Titre := removeNumbers(Titre)]
  tf_idf[, Titre := tolower(Titre)]
  tf_idf[, Titre := removeWords(Titre, stopwords("english"))]
  tf_idf[, Titre := as.character(Titre)]
  # ngraming
  tf_idf[, Titre := stemDocument(Titre)]
  tf_idf$Titre <- tokens(tf_idf$Titre, remove_punct = TRUE)
  tf_idf$Titre <- tokens_ngrams(tf_idf$Titre, n = 2)
  tible_tf_idf <- tf_idf[, paste(Titre, collapse = " "), by = "Com_ID"]
  tible_tf_idf[, V1 := stripWhitespace(V1)]
  # tf-idf using quanteda
  tible_tf_idf <- corpus(tible_tf_idf, text_field = "V1")
  tible_tf_idf <- dfm(tible_tf_idf)
  tible_tf_idf <- dfm_tfidf(tible_tf_idf)
  # Keep column of names
  documents_names <- cbind(docvars(tible_tf_idf), quanteda::convert(tible_tf_idf, to = "data.frame")) %>% as.data.table()
  tible_tf_idf <- tidy(tible_tf_idf) %>% as.data.table()
  tf_idf_table <- merge(tible_tf_idf, documents_names[, .(doc_id, Com_ID)], by.x = "document", by.y = "doc_id")
  # Unstemming bigram: first term, then second term, them bringing them together
  tf_idf_table$term <- gsub("_", " ", tf_idf_table$term)
  tf_idf_table[, term1 := str_extract(tf_idf_table$term, "\\S+")]
  tf_idf_table[, term2 := str_extract(tf_idf_table$term, "\\S+$")]
  
  if(unstemming==TRUE){
    tf_idf_table[,unstemmed_word1:=stemCompletion(tf_idf_table$term1, dictionary$word, type = "prevalent")] # unstem with most common word
    tf_idf_table[unstemmed_word1=="",unstemmed_word:=term1]
    tf_idf_table[,unstemmed_word2:=stemCompletion(tf_idf_table$term2, dictionary$word, type = "prevalent")] # unstem with most common word
    tf_idf_table[unstemmed_word2=="",unstemmed_word:=term2]
  }
  if(unstemming==FALSE){
    tf_idf_table[,unstemmed_word1:=term1]
    tf_idf_table[,unstemmed_word2:=term2]
  }

  tf_idf_table[, term := paste(unstemmed_word1, unstemmed_word2)]
  tf_idf_table_bi <- tf_idf_table

  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Plot ####
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  tf_idf_table <- rbind(tf_idf_table_uni, tf_idf_table_bi, fill = TRUE)
  tf_idf_table <- tf_idf_table[order(-count)][, head(.SD, number_of_words), Com_ID]

  # Get info about size of communities
  Size_com <- unique(tf_idf_save[, .(Com_ID, Community_name, Size_com, color)])
  tf_idf_table <- merge(tf_idf_table, Size_com, by = "Com_ID", all.x = TRUE) # merge

  # Wrap Name and Reorder according to share_leiden
  tf_idf_table$Com_wrap <- str_wrap(tf_idf_table$Community_name, width = 10)
  tf_idf_table <- tf_idf_table[order(-Size_com)] # order by variable
  tf_idf_table <- tf_idf_table[order(-Size_com)] # order by variable
  tf_idf_table$Com_wrap <- factor(tf_idf_table$Com_wrap) # make a factor
  tf_idf_table$Com_wrap <- fct_inorder(tf_idf_table$Com_wrap) # by order of appearance

  # fixing the number of columns, depending of the number of communities
  n_columns <- 3
  if (length(unique(tf_idf_table[Size_com >= treshold_com]$Com_wrap)) > 9) {
    n_columns <- 4
  }

  if (length(unique(tf_idf_table[Size_com >= treshold_com]$Com_wrap)) > 12) {
    n_columns <- 5
  }

  if (length(unique(tf_idf_table[Size_com >= treshold_com]$Com_wrap)) > 15) {
    n_columns <- 6
  }

  # plotting the graph
  tf_idf_plot <- ggplot(tf_idf_table[Size_com >= treshold_com], aes(reorder_within(term, count, color), count, fill = color)) +
    geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
    labs(
      title = "Highest tf-idf",
      x = "Words", y = "tf-idf"
    ) +
    facet_wrap(~Com_wrap, ncol = n_columns, scales = "free") +
    scale_x_reordered() +
    scale_fill_identity() +
    theme(strip.text = element_text(size = size_title_wrap)) +
    coord_flip()

  list_return <- list("plot" = tf_idf_plot, "list_words" = tf_idf_table)
  return(list_return)
}
```
