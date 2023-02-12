source("functions/functions_dynamics_networks_alex.R")
# source("functions/functions_networks_alex.R")
source("functions/functions_for_network_analysis.R")
source("functions/Script_paths_and_basic_objectsV2.R")


nodes <- fread(here(data_path,"macro_AA","5_platform_data","nodes_lf.csv")) %>% as.data.table
institutions <- readRDS(here(data_path,"macro_AA","3_Corpus_WoS","MACRO_AA_INSTITUTIONS.rds")) %>% as.data.table

nodes_countries <- merge(nodes[,.SD[1],ID_Art][,.(ID_Art)], institutions[,.SD[1],.(ID_Art,Pays)][,.(ID_Art,Pays)], by = "ID_Art", all.x = TRUE)
nodes_countries[is.na(Pays), Pays:="Missing Information"]
nodes_countries <- aggregate(Pays ~ ID_Art, nodes_countries, paste, collapse = ", ") %>% as.data.table()

nodes_institutions <- merge(nodes[,.SD[1],ID_Art][,.(ID_Art)], institutions[,.SD[1],.(ID_Art,Institution)][,.(ID_Art,Institution)], by = "ID_Art", all.x = TRUE)
nodes_institutions[is.na(Institution), Institution:="Missing Information"]
nodes_institutions <- aggregate(Institution ~ ID_Art, nodes_institutions, paste, collapse = ", ") %>% as.data.table()

affiliation_info_table <- merge(nodes_countries, nodes_institutions, by = "ID_Art", all.x = TRUE, all.y = TRUE)
write_csv(affiliation_info_table, here(data_path,"macro_AA","5_platform_data","nodes_affiliation_info_table.csv"))

