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
#' In this script, we find communities on the coupling networks of each sub-periods for different
#' level of resolution. In the Leiden algorithm, a higher resolution will lead the algorithm to 
#' find a higher number of communities. We set the basis for the projection of Sankey diagrams
#' which give us an information about how communities are merged together when we decrease the 
#' resolution level.
#' 
#' > WARNING: This script represents a first step of the project, and some processes have been
#' improved (notably by the creation of new functions). Besides, it is a bit sketchy and needs
#' a lot of cleaning.

#' # Loading packages, paths and data

source("~/macro_AA/functions/functions_for_network_analysis.R")
source("~/macro_AA/Static_Network_Analysis/Script_paths_and_basic_objects.R")

#' # Testing Leiden

#' ## Finding communities for different resolution of Leiden
#' 
#' We set different resolution levels first. Then we run the algorithm for these different levels, automatically
#' attributes names to communiites. We then merge with the names chosen for the communities found in 
#' [2_Script_Static_Network_Analysis](/Static_Network_Analysis/2_Script_Static_Network_Analysis.md) (probably with
#' a resolution equals to 1). 

resolution_set <- c(2, 1.5, 1, 0.5)
for (i in 1:length(start_date)) {
  graph_coupling <- readRDS(paste0(graph_data_path, "graph_coupling_", start_date[i], "-", end_date[i], ".rds"))
  Leiden_coupling <- graph_coupling %>%
    activate(nodes) %>%
    select(Id, Label, Titre, nb_cit, Community_name, Size_com) %>%
    as.data.table()
  # colnames(Leiden_coupling)[colnames(Leiden_coupling) == "Community_name"] = "Community_name_1"
  # colnames(Leiden_coupling)[colnames(Leiden_coupling) == "Size_com"] = "Size_com_1"
  Leiden_coupling$Community_name <- gsub(".*-", "", Leiden_coupling$Community_name)

  # Leiden_coupling$Id <- as.integer(Leiden_coupling$Id)
  for (j in 1:length(resolution_set)) {
    graph_coupling <- readRDS(paste0(graph_data_path, "prior_graph_coupling_", start_date[i], "-", end_date[i], ".rds"))

    # Identifying communities with Leiden algorithm
    graph_coupling <- leiden_improved(graph_coupling, res_1 = resolution_set[j], res_2 = NULL, res_3 = NULL, n_iterations = -1)

    # Giving colors to communities
    graph_coupling <- community_colors(graph_coupling, mypalette)

    # Naming communities
    graph_coupling <- naming_communities(graph_coupling, centrality_measure = "nb_cit", naming = "Label")

    # Renaming columns
    Com <- graph_coupling %>%
      activate(nodes) %>%
      select(Id, Com_ID, Community_name, Size_com, color) %>%
      filter(Size_com > 0.01) %>%
      as.data.table()
    Com$Community_name <- gsub(".*-", "", Com$Community_name)
    setnames(Com, c("Com_ID", "Community_name", "Size_com", "color"), c(
      paste0("Com_ID_", resolution_set[j]),
      paste0("Community_name_", resolution_set[j]),
      paste0("Size_com_", resolution_set[j]),
      paste0("color_", resolution_set[j])
    ))

    Leiden_coupling <- merge(Leiden_coupling, Com, by = "Id")
  }

  Leiden_coupling <- Leiden_coupling[, `:=`(Community_name_1 = Community_name, Size_com_1 = Size_com)]
  Leiden_coupling <- Leiden_coupling[, .SD, .SDcols = c(colnames(Leiden_coupling)[(colnames(Leiden_coupling) != c("Community_name", "Size_com"))])]
  # Saving the data
  saveRDS(Leiden_coupling, paste0(graph_data_path, "Leiden_coupling_", start_date[i], "-", end_date[i], ".rds"))
}

#' ## Building the Sankey diagrams
#' 
#' You don't have to run this code. It is used in [Static_Nework_Analysis.Rmd](/Static_Network_Analysis/Static_Nework_Analysis.Rmd) 
#' to project the Sankey Diagrams for each subperiod.
#' 

#Leiden_coupling <- readRDS(paste0(graph_data_path, "Leiden_coupling_", start_date[i], "-", end_date[i], ".rds")) %>% as.data.table()

# identifying the most cited nodes per resolution
All_com <- data.table()
for (j in 1:(length(resolution_set))) {
  Com <- Leiden_coupling[, .SD, .SDcols = c(1, grep("Community_name", colnames(Leiden_coupling))[j], grep("Size_com", colnames(Leiden_coupling))[j])]
  Com$Resolution <- gsub(".*_", "", colnames(Leiden_coupling[, .SD, .SDcols = grep("Community_name", colnames(Leiden_coupling))[j]]))

  All_com <- rbind(All_com, Com, use.names = FALSE)
}

# cleaning the table and keeping the top n values
n = 8

Leiden_coupling_data <- merge(Leiden_coupling[,c("Id","Label", "Titre", "nb_cit")],All_com, by = "Id")
colnames(Leiden_coupling_data)[colnames(Leiden_coupling_data) == "Community_name_2"] = "Community_name"
colnames(Leiden_coupling_data)[colnames(Leiden_coupling_data) == "Size_com_2"] = "Size_com"
Leiden_coupling_data <- Leiden_coupling_data %>% 
  arrange(desc(Resolution),Community_name,desc(nb_cit)) %>%
  group_by(Resolution,Community_name) %>%
  slice(1:n)  %>%
  arrange(desc(Resolution), desc(Size_com), desc(nb_cit))


# Building the sankey diagrams:

# Keeping only the relevant columns and counting the links
sankey_data <- Leiden_coupling[, .SD, .SDcols = c(1, grep("Community_name", colnames(Leiden_coupling)))]
sankey_data <- unique(sankey_data[, value := .(.N), by = c(colnames(Leiden_coupling[, .SD, .SDcols = grep("Community_name", colnames(Leiden_coupling))]))])[, 2:length(sankey_data)]

# reogranising in a lode form for plotting
sankey_data <- to_lodes_form(sankey_data,
  axes = 1:length(resolution_set),
  id = "Cohort"
)

ggplot(
  sankey_data,
  aes(
    x = x, stratum = stratum, alluvium = Cohort,
    fill = stratum, label = stratum
  )
) +
  scale_fill_manual(values = c(mypalette, scico(n = 15, palette = "roma"))) +
  geom_flow() +
  geom_stratum() +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggsave(paste0(picture_path, "Sankey_diagram_Leiden_", start_date[i], "-", end_date[i], ".png"), width = 30, height = 40, units = "cm")
