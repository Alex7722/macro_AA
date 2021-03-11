#' ---
#' title: "Script for building the networks for different sub-periods"
#' author: "Aurélien Goutsmedt and Alexandre Truc"
#' date: "/ Last compiled on `r format(Sys.Date())`"
#' output: 
#'   github_document:
#'     toc: true
#'     number_sections: true
#' ---

#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)

#' # What is this script for?
#' 
#' In this script, we build different networks (cocitation, coupling, coupling with authors, co-authorship, _etc._)
#' for different subperiods. Subperiods are fixed in another [script](/Static_Network_Analysis/Script_paths_and_basic_objects.R) loaded at the beginning
#' (see the `start_date` and `end_date` objects). All the networks created are saved as `prior_` network and then loaded in
#' the following [script](/Static_Network_Analysis/2_Script_Static_Network_Analysis.md). Initially, we have 
#' created three seven-year periods (1970-1976, 1977-1983, 1984-1990) and three six-year
#' periods (1991-1996, 1997-2002, 2003-2008). We maintained the break in 1990/1991 to take
#' into account the JEL change of classification.
#' 
#' > WARNING: This script represents a first step of the project, and some processes have been
#' improved (notably by the creation of new functions).
#' 
#' # Loading packages, paths and data
#' 
#' ## External scripts

source("~/macro_AA/functions/functions_for_network_analysis.R")
source("~/macro_AA/Static_Network_Analysis/Script_paths_and_basic_objects.R")

#' ## Loading Data

nodes_JEL <- readRDS(paste0(data_path, "JEL_matched_corpus_nodes.rds"))
nodes_old_JEL <- readRDS(paste0(data_path, "Old_JEL_matched_corpus_nodes.rds"))
nodes_JEL <- rbind(nodes_JEL, nodes_old_JEL)

edges_JEL <- readRDS(paste0(data_path, "JEL_matched_corpus_edges.rds"))
edges_old_JEL <- readRDS(paste0(data_path, "Old_JEL_matched_corpus_edges.rds"))
edges_JEL <- rbind(edges_JEL, edges_old_JEL)

authors_JEL <- readRDS(paste0(data_path, "JEL_matched_corpus_authors.rds"))
authors_old_JEL <- readRDS(paste0(data_path, "Old_JEL_matched_corpus_authors.rds"))
authors_JEL <- rbind(authors_JEL, authors_old_JEL)

institutions_info_JEL <- fread(paste0(data_path, "Macro_AA_Institutions_Cleaned.csv"), quote = "", fill = TRUE) %>% data.table()

ref_info_JEL <- readRDS(paste0(data_path, "JEL_matched_corpus_references_info.rds"))
ref_info_old_JEL <- readRDS(paste0(data_path, "Old_JEL_matched_corpus_references_info.rds"))
ref_info_JEL <- unique(rbind(ref_info_JEL, ref_info_old_JEL))

# keeping only refs with a title and a ESpecialite, then removing doublons
ref_info_JEL <- unique(ref_info_JEL[Titre != "NA" & ESpecialite != "NA", c("New_id2", "Titre", "ESpecialite")])
doublons <- which(duplicated(ref_info_JEL$New_id2))

if (length(doublons) != 0) {
  ref_info_JEL <- ref_info_JEL[-doublons]
}


# Adding info to references
edges_JEL <- merge(unique(edges_JEL), unique(ref_info_JEL[Titre != "NA" & ESpecialite != "NA", c("New_id2", "Titre", "ESpecialite")]), by = "New_id2", all.x = TRUE)
edges_JEL <- merge(edges_JEL, unique(nodes_JEL[, c("ID_Art", "Annee_Bibliographique")]), by = "ID_Art", all.x = TRUE)

# Adding institutions to Articles
institutions_info_JEL$ID_Art <- as.integer(institutions_info_JEL$ID_Art)
institutions_info_JEL$Ordre <- as.integer(institutions_info_JEL$Ordre)
authors_JEL <- merge(authors_JEL, institutions_info_JEL, by = c("ID_Art", "Ordre"), all.x = TRUE)

# removing useless files

rm(ref_info_JEL)
rm(institutions_info_JEL)

# passing name column to upper letters
edges_JEL <- edges_JEL[, Nom := toupper(Nom)]
authors_JEL <- authors_JEL[, Nom := toupper(Nom)]
nodes_JEL <- nodes_JEL[, Nom := toupper(Nom)]

# fixing threshold
Limit_nodes <- 20000
Limit_edges <- 300000


#' # Bibliographic Cocitation

############################### Building Nodes and Edges for co-citation #######################
for (i in 1:length(start_date)) {
  percent_nodes_threshold <- 0.05
  edges_threshold <- 1

  # creating nodes
  nodes_cocit_JEL <- edges_JEL[between(Annee_Bibliographique, start_date[i], end_date[i]), ]
  # nodes_cocit_JEL_1990s <- merge(nodes_cocit_JEL_1990s, nodes_JEL[,c("ItemID_Ref","Titre","Code_Revue")], by = "ItemID_Ref", all.x = TRUE)
  nodes_cocit_JEL <- unique(nodes_cocit_JEL[, nb_cit := .N, by = "New_id2"][nb_cit >= 1, c("New_id2", "Annee", "Nom", "Titre", "Revue_Abbrege", "ESpecialite", "nb_cit")])
  doublons <- which(duplicated(nodes_cocit_JEL$New_id2))
  nodes_cocit_JEL <- nodes_cocit_JEL[-doublons]

  for (n in 1:100) {
    # running the test for the minimum percentage of nodes
    test <- unique(nodes_cocit_JEL[, total := .N][, distrib := .N, by = "nb_cit"][, distrib := (distrib / total)][order(nb_cit), c("nb_cit", "distrib")])

    if (sum(test[n:length(test$distrib)]$distrib) < percent_nodes_threshold) {
      break
      return(n)
    }
  }
  nodes_cocit_threshold <- n - 1

  # starting again from the beginning but this time with the threshold
  # creating nodes
  nodes_cocit_JEL <- edges_JEL[between(Annee_Bibliographique, start_date[i], end_date[i]), ]
  # nodes_cocit_JEL_1990s <- merge(nodes_cocit_JEL_1990s, nodes_JEL[,c("ItemID_Ref","Titre","Code_Revue")], by = "ItemID_Ref", all.x = TRUE)
  nodes_cocit_JEL <- unique(nodes_cocit_JEL[, nb_cit := .N, by = "New_id2"][nb_cit >= nodes_cocit_threshold, c("New_id2", "Annee", "Nom", "Titre", "Revue_Abbrege", "ESpecialite", "nb_cit")])
  doublons <- which(duplicated(nodes_cocit_JEL$New_id2))
  nodes_cocit_JEL <- nodes_cocit_JEL[-doublons]

  # creating a label for nodes
  nodes_cocit_JEL <- nodes_cocit_JEL[, Label := paste0(Nom, "-", Annee)]

  # creating edges
  edges_cocit_JEL <- unique(edges_JEL[New_id2 %in% nodes_cocit_JEL$New_id2 & between(Annee_Bibliographique, start_date[i], end_date[i])][, c("ID_Art", "New_id2")])

  # creation of the edges for the co-citation network

  edges_cocit_JEL <- biblio_coupling(edges_cocit_JEL, source = "New_id2", ref = "ID_Art", weight_threshold = edges_threshold)

  # Loop to avoid to large networks - Step 1: reducing nodes
  if (length(nodes_cocit_JEL$New_id2) > Limit_nodes) {
    for (j in 1:100) {
      nodes_cocit_threshold <- nodes_cocit_threshold + 1
      # creating nodes
      nodes_cocit_JEL <- edges_JEL[between(Annee_Bibliographique, start_date[i], end_date[i]), ]
      # nodes_cocit_JEL_1990s <- merge(nodes_cocit_JEL_1990s, nodes_JEL[,c("ItemID_Ref","Titre","Code_Revue")], by = "ItemID_Ref", all.x = TRUE)
      nodes_cocit_JEL <- unique(nodes_cocit_JEL[, nb_cit := .N, by = "New_id2"][nb_cit >= nodes_cocit_threshold, c("New_id2", "Annee", "Nom", "Titre", "Revue_Abbrege", "ESpecialite", "nb_cit")])
      doublons <- which(duplicated(nodes_cocit_JEL$New_id2))
      nodes_cocit_JEL <- nodes_cocit_JEL[-doublons]

      # creating a label for nodes
      nodes_cocit_JEL <- nodes_cocit_JEL[, Label := paste0(Nom, "-", Annee)]

      # creating edges
      edges_cocit_JEL <- unique(edges_JEL[New_id2 %in% nodes_cocit_JEL$New_id2 & between(Annee_Bibliographique, start_date[i], end_date[i])][, c("ID_Art", "New_id2")])

      # creation of the edges for the co-citation network

      edges_cocit_JEL <- biblio_coupling(edges_cocit_JEL, source = "New_id2", ref = "ID_Art", weight_threshold = edges_threshold)

      if (length(nodes_cocit_JEL$New_id2) < Limit_nodes) {
        break
      }
    }
  }

  # Loop to avoid to large networks - Step 2: reducing edges
  if (length(edges_cocit_JEL$from) > Limit_edges) {
    for (k in 1:100) {
      edges_threshold <- edges_threshold + 1
      # creating nodes
      nodes_cocit_JEL <- edges_JEL[between(Annee_Bibliographique, start_date[i], end_date[i]), ]
      # nodes_cocit_JEL_1990s <- merge(nodes_cocit_JEL_1990s, nodes_JEL[,c("ItemID_Ref","Titre","Code_Revue")], by = "ItemID_Ref", all.x = TRUE)
      nodes_cocit_JEL <- unique(nodes_cocit_JEL[, nb_cit := .N, by = "New_id2"][nb_cit >= nodes_cocit_threshold, c("New_id2", "Annee", "Nom", "Titre", "Revue_Abbrege", "ESpecialite", "nb_cit")])
      doublons <- which(duplicated(nodes_cocit_JEL$New_id2))
      nodes_cocit_JEL <- nodes_cocit_JEL[-doublons]

      # creating a label for nodes
      nodes_cocit_JEL <- nodes_cocit_JEL[, Label := paste0(Nom, "-", Annee)]

      # creating edges
      edges_cocit_JEL <- unique(edges_JEL[New_id2 %in% nodes_cocit_JEL$New_id2 & between(Annee_Bibliographique, start_date[i], end_date[i])][, c("ID_Art", "New_id2")])

      # creation of the edges for the co-citation network
      edges_cocit_JEL <- biblio_coupling(edges_cocit_JEL, source = "New_id2", ref = "ID_Art", weight_threshold = edges_threshold)

      if (length(edges_cocit_JEL$from) < Limit_edges) {
        break
      }
    }
  }

  # creating the tidygraph object
  nodes_cocit_JEL <- nodes_cocit_JEL[, New_id2 := as.character(New_id2)]
  nodes_cocit_JEL$threshold <- nodes_cocit_threshold
  edges_cocit_JEL$threshold <- edges_threshold

  graph_cocit <- tbl_main_components(edges = edges_cocit_JEL, nodes = nodes_cocit_JEL, node_key = "New_id2", threshold_alert = 0.05, directed = FALSE)

  saveRDS(graph_cocit, paste0(graph_data_path, "prior_graph_cocit_", start_date[i], "-", end_date[i], ".rds"))
}


#' # Bibliographic Coupling

############################### Building Nodes and Edges for coupling #######################-----------

for (i in 1:length(start_date)) {
  nodes_coupling_threshold <- 0
  edges_threshold <- 2

  # creating the nodes with the number of citation of the nodes in the corpus
  nodes_coupling_JEL <- nodes_JEL[between(Annee_Bibliographique, start_date[i], end_date[i]), ]
  nb_cit <- edges_JEL[between(Annee_Bibliographique, start_date[i], end_date[i]) & ItemID_Ref != 0][, nb_cit := .N, by = "ItemID_Ref"]
  nodes_coupling_JEL <- merge(nodes_coupling_JEL, unique(nb_cit[, c("ItemID_Ref", "nb_cit")]), by = "ItemID_Ref", all.x = "TRUE")

  rm(nb_cit) # not useful anymore
  # replacing NA value by 0 in nb_cit (it means that the nodes will be used in Leiden and Force Atlas, but
  # not displayed in the graph)

  nodes_coupling_JEL[is.na(nb_cit), ]$nb_cit <- 0

  # reducing the number of nodes depending of the number of citations
  nodes_coupling_JEL <- unique(nodes_coupling_JEL[nb_cit >= nodes_coupling_threshold, c("ID_Art", "Annee_Bibliographique", "Nom", "Label", "Titre", "Revue", "ESpecialite", "nb_cit", "ItemID_Ref")])

  # building edges
  edges_coupling_JEL <- unique(edges_JEL[ID_Art %in% nodes_coupling_JEL$ID_Art])

  # creation of the edges for the co-citation network

  edges_coupling_JEL <- biblio_coupling(edges_coupling_JEL, source = "ID_Art", ref = "New_id2", weight_threshold = edges_threshold)

  # Loop to avoid to large networks - Step 1: reducing nodes
  if (length(nodes_coupling_JEL$ID_Art) > Limit_nodes) {
    for (j in 1:100) {
      nodes_coupling_threshold <- nodes_coupling_threshold + 1

      # creating the nodes with the number of citation of the nodes in the corpus
      nodes_coupling_JEL <- nodes_JEL[between(Annee_Bibliographique, start_date[i], end_date[i]), ]
      nb_cit <- edges_JEL[between(Annee_Bibliographique, start_date[i], end_date[i]) & ItemID_Ref != 0][, nb_cit := .N, by = "ItemID_Ref"]
      nodes_coupling_JEL <- merge(nodes_coupling_JEL, unique(nb_cit[, c("ItemID_Ref", "nb_cit")]), by = "ItemID_Ref", all.x = "TRUE")

      rm(nb_cit) # not useful anymore
      # replacing NA value by 0 in nb_cit (it means that the nodes will be used in Leiden and Force Atlas, but
      # not displayed in the graph)

      nodes_coupling_JEL[is.na(nb_cit), ]$nb_cit <- 0

      # reducing the number of nodes depending of the number of citations
      nodes_coupling_JEL <- unique(nodes_coupling_JEL[nb_cit >= nodes_coupling_threshold, c("ID_Art", "Annee_Bibliographique", "Nom", "Label", "Titre", "Revue", "ESpecialite", "nb_cit", "ItemID_Ref")])

      # building edges
      edges_coupling_JEL <- unique(edges_JEL[ID_Art %in% nodes_coupling_JEL$ID_Art])

      # creation of the edges for the co-citation network

      edges_coupling_JEL <- biblio_coupling(edges_coupling_JEL, source = "ID_Art", ref = "New_id2", weight_threshold = edges_threshold)

      if (length(nodes_coupling_JEL$ID_Art) < Limit_nodes) {
        break
      }
    }
  }

  # Loop to avoid to large networks - Step 2: reducing edges
  if (length(edges_coupling_JEL$from) > Limit_edges) {
    for (k in 1:100) {
      edges_threshold <- edges_threshold + 1

      # creating the nodes with the number of citation of the nodes in the corpus
      nodes_coupling_JEL <- nodes_JEL[between(Annee_Bibliographique, start_date[i], end_date[i]), ]
      nb_cit <- edges_JEL[between(Annee_Bibliographique, start_date[i], end_date[i]) & ItemID_Ref != 0][, nb_cit := .N, by = "ItemID_Ref"]
      nodes_coupling_JEL <- merge(nodes_coupling_JEL, unique(nb_cit[, c("ItemID_Ref", "nb_cit")]), by = "ItemID_Ref", all.x = "TRUE")

      rm(nb_cit) # not useful anymore
      # replacing NA value by 0 in nb_cit (it means that the nodes will be used in Leiden and Force Atlas, but
      # not displayed in the graph)

      nodes_coupling_JEL[is.na(nb_cit), ]$nb_cit <- 0

      # reducing the number of nodes depending of the number of citations
      nodes_coupling_JEL <- unique(nodes_coupling_JEL[nb_cit >= nodes_coupling_threshold, c("ID_Art", "Annee_Bibliographique", "Nom", "Label", "Titre", "Revue", "ESpecialite", "nb_cit", "ItemID_Ref")])

      # building edges
      edges_coupling_JEL <- unique(edges_JEL[ID_Art %in% nodes_coupling_JEL$ID_Art])

      # creation of the edges for the co-citation network

      edges_coupling_JEL <- biblio_coupling(edges_coupling_JEL, source = "ID_Art", ref = "New_id2", weight_threshold = edges_threshold)

      if (length(edges_coupling_JEL$from) < Limit_edges) {
        break
      }
    }
  }

  # creating the tidygraph object
  nodes_coupling_JEL <- nodes_coupling_JEL[, ID_Art := as.character(ID_Art)]
  nodes_coupling_JEL$threshold <- nodes_coupling_threshold
  edges_coupling_JEL$threshold <- edges_threshold

  graph_coupling <- tbl_main_components(edges = edges_coupling_JEL, nodes = nodes_coupling_JEL, node_key = "ID_Art", threshold_alert = 0.05, directed = FALSE)

  saveRDS(graph_coupling, paste0(graph_data_path, "prior_graph_coupling_", start_date[i], "-", end_date[i], ".rds"))
}

#' # Bibliographic Coupling with Authors

################## Building an author bibliographic coupling graph ######################----

for (i in 1:length(start_date)) {
  nodes_coupling_threshold <- 0
  edges_threshold <- 2
  nb_art_threshold <- 2

  # creating the nodes with the number of citation of the nodes in the corpus
  nodes_coupling <- nodes_JEL[between(Annee_Bibliographique, start_date[i], end_date[i]), ]

  authors_JEL <- authors_JEL[, citing_author := Nom]
  nodes_coupling <- merge(nodes_coupling, unique(authors_JEL[, c("ID_Art", "citing_author")]), by = "ID_Art")


  # calculating the number of articles
  nodes_coupling <- nodes_coupling[, nb_art := .N, by = "citing_author"][nb_art >= nb_art_threshold]

  # calculating the number of citations
  nb_cit <- edges_JEL[between(Annee_Bibliographique, start_date[i], end_date[i]) & ItemID_Ref != 0][, nb_cit := .N, ItemID_Ref]
  nodes_coupling <- merge(unique(nodes_coupling), unique(nb_cit[, c("ItemID_Ref", "nb_cit")]), by = "ItemID_Ref", all.x = "TRUE")

  rm(nb_cit) # not useful anymore
  # replacing NA value by 0 in nb_cit (it means that the nodes will be used in Leiden and Force Atlas, but
  # not displayed in the graph)

  nodes_coupling[is.na(nb_cit), ]$nb_cit <- 0


  # choosing the nodes to keep
  nodes_coupling <- nodes_coupling[nb_cit >= nodes_coupling_threshold]

  # building edges
  edges_coupling <- unique(edges_JEL[ID_Art %in% nodes_coupling$ID_Art])

  authors_edges <- merge(edges_coupling, unique(nodes_coupling[, c("ID_Art", "citing_author")]), by = "ID_Art", all.x = TRUE, allow.cartesian = TRUE)

  authors_edges <- bibliographic_coupling_authors(authors_edges, source = "ID_Art", authors = "citing_author", ref = "New_id2", weight_threshold = edges_threshold)

  # reducing the number of nodes depending of the number of citations

  authors_nodes <- unique(nodes_coupling[, nb_cit_author := sum(nb_cit), by = "citing_author"][order(-nb_cit_author), c("citing_author", "nb_cit_author", "nb_art")])


  # Loop to avoid to large networks - Step 2: reducing edges
  if (length(authors_edges$from) > Limit_edges) {
    for (k in 1:100) {
      edges_threshold <- edges_threshold + 1

      # creating the nodes with the number of citation of the nodes in the corpus
      nodes_coupling <- nodes_JEL[between(Annee_Bibliographique, start_date[i], end_date[i]), ]

      authors_JEL <- authors_JEL[, citing_author := Nom]
      nodes_coupling <- merge(nodes_coupling, unique(authors_JEL[, c("ID_Art", "citing_author")]), by = "ID_Art")


      # calculating the number of articles
      nodes_coupling <- nodes_coupling[, nb_art := .N, by = "citing_author"][nb_art >= nb_art_threshold]

      # calculating the number of citations
      nb_cit <- edges_JEL[between(Annee_Bibliographique, start_date[i], end_date[i]) & ItemID_Ref != 0][, nb_cit := .N, ItemID_Ref]
      nodes_coupling <- merge(unique(nodes_coupling), unique(nb_cit[, c("ItemID_Ref", "nb_cit")]), by = "ItemID_Ref", all.x = "TRUE")

      rm(nb_cit) # not useful anymore
      # replacing NA value by 0 in nb_cit (it means that the nodes will be used in Leiden and Force Atlas, but
      # not displayed in the graph)

      nodes_coupling[is.na(nb_cit), ]$nb_cit <- 0


      # choosing the nodes to keep
      nodes_coupling <- nodes_coupling[nb_cit >= nodes_coupling_threshold]

      # building edges
      edges_coupling <- unique(edges_JEL[ID_Art %in% nodes_coupling$ID_Art])

      authors_edges <- merge(edges_coupling, unique(nodes_coupling[, c("ID_Art", "citing_author")]), by = "ID_Art", all.x = TRUE, allow.cartesian = TRUE)

      authors_edges <- bibliographic_coupling_authors(authors_edges, source = "ID_Art", authors = "citing_author", ref = "New_id2", weight_threshold = edges_threshold)

      # reducing the number of nodes depending of the number of citations

      authors_nodes <- unique(nodes_coupling[, nb_cit_author := sum(nb_cit), by = "citing_author"][order(-nb_cit_author), c("citing_author", "nb_cit_author", "nb_art")])

      if (length(authors_edges$from) < Limit_edges) {
        break
      }
    }
  }


  authors_nodes$threshold <- nodes_coupling_threshold
  authors_edges$threshold <- edges_threshold

  # creating the tidygraph object
  graph_authors_coupling <- tbl_main_components(edges = authors_edges, nodes = authors_nodes, node_key = "citing_author", threshold_alert = 0.05, directed = FALSE)

  saveRDS(graph_authors_coupling, paste0(graph_data_path, "prior_graph_authors_coupling_", start_date[i], "-", end_date[i], ".rds"))
}

#' # Institutions network from Coupling


for (i in 1:length(start_date)) {
  nb_art_threshold <- 10
  edges_threshold <- 10

  # creating the nodes with the number of citation of the nodes in the corpus
  institutions_coupling_JEL <- nodes_JEL[between(Annee_Bibliographique, start_date[i], end_date[i]), ]

  authors_JEL <- authors_JEL[, citing_author := Nom]
  institutions_coupling_JEL <- merge(institutions_coupling_JEL, unique(authors_JEL[, c("ID_Art", "citing_author", "Institution", "Pays")]), by = "ID_Art")

  # Calculating the weighted number of articles per institution
  institutions_coupling_JEL <- institutions_coupling_JEL[, share_authors := .N, by = "ID_Art"][, share_authors := 1 / share_authors][, nb_art := sum(share_authors), by = "Institution"]
  institutions_coupling_JEL <- institutions_coupling_JEL[Institution != "NA" & nb_art > nb_art_threshold]

  # building edges
  edges_JEL_bis <- unique(edges_JEL[ID_Art %in% institutions_coupling_JEL$ID_Art])

  institutions_edges <- merge(edges_JEL_bis, unique(institutions_coupling_JEL[, c("ID_Art", "Institution")]), by = "ID_Art", all.x = TRUE, allow.cartesian = TRUE)

  institutions_edges <- bibliographic_coupling(institutions_edges, "Institution", "New_id2", weight_threshold = edges_threshold)

  # reducing the number of nodes depending of the number of citations

  institutions_nodes <- unique(institutions_coupling_JEL[order(-nb_art), c("Institution", "nb_art", "Pays")])

  institutions_nodes$threshold <- nb_art_threshold
  institutions_edges$threshold <- edges_threshold

  # creating the tidygraph object
  graph_institutions_coupling <- tbl_main_components(edges = institutions_edges, nodes = institutions_nodes, node_key = "Institution", threshold_alert = 0.05, directed = FALSE)

  saveRDS(graph_institutions_coupling, paste0(graph_data_path, "prior_graph_institutions_coupling_", start_date[i], "-", end_date[i], ".rds"))
}

#' # Co-authorship

################## Building a co-authorship graph ######################----

for (i in 1:length(start_date)) {
  # creating the nodes with the number of citation of the nodes in the corpus
  coauthorship_nodes_1990s <- nodes_JEL[between(Annee_Bibliographique, start_date[i], end_date[i]), ]
  authors_JEL <- authors_JEL[, citing_author := Nom]

  coauthorship_nodes_1990s <- merge(coauthorship_nodes_1990s, unique(authors_JEL[, c("ID_Art", "citing_author")]), by = "ID_Art")

  # building edges
  coauthorship_edges_1990s <- bibliographic_coupling(unique(coauthorship_nodes_1990s[, c("ID_Art", "citing_author")]), source = "citing_author", ref = "ID_Art", weight_threshold = 1)

  # building nodes
  coauthorship_nodes_1990s <- unique(coauthorship_nodes_1990s[, "citing_author"])

  # creating the tidygraph object
  graph_coauthorship <- tbl_main_components(edges = coauthorship_edges_1990s, nodes = coauthorship_nodes_1990s, node_key = "citing_author", threshold_alert = 0.05, directed = FALSE)

  # Saving the graph
  saveRDS(graph_coauthorship, paste0(graph_data_path, "prior_graph_coauthorship_", start_date[i], "-", end_date[i], ".rds"))
}

############################## institutions network from co-authorship data #########################
# creating the nodes with the number of citation of the nodes in the corpus

for (i in 1:length(start_date)) {
  nb_art_threshold <- 10
  edges_threshold <- 1

  institutions_nodes <- nodes_JEL[between(Annee_Bibliographique, start_date[i], end_date[i]), ]

  # Merging with authors
  authors_JEL <- authors_JEL[, citing_author := Nom]
  institutions_nodes <- merge(institutions_nodes, unique(authors_JEL), by = "ID_Art")
  institutions_nodes <- institutions_nodes[!is.na(Institution)]

  # Calculating the weighted number of articles per institution
  institutions_nodes <- institutions_nodes[, share_authors := .N, by = "ID_Art"][, share_authors := 1 / share_authors][, nb_art := sum(share_authors), by = "Institution"]
  institutions_nodes <- institutions_nodes[Institution != "NA" & nb_art >= nb_art_threshold]


  # building edges
  institutions_edges <- bibliographic_coupling(unique(institutions_nodes[, c("ID_Art", "Institution")]), source = "Institution", ref = "ID_Art", weight_threshold = edges_threshold)

  # building nodes

  institutions_nodes <- unique(institutions_nodes[, c("Institution", "Pays", "nb_art")])

  # creating the tidygraph object
  graph_institutions <- tbl_main_components(edges = institutions_edges, nodes = institutions_nodes, node_key = "Institution", threshold_alert = 0.05, directed = FALSE)

  saveRDS(graph_institutions, paste0(graph_data_path, "prior_graph_co-authorship_institutions_", start_date[i], "-", end_date[i], ".rds"))
}
