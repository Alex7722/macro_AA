# THIS IS A TEMPORARY VERSION FOR PHILIPPE, TO REDO FOR NEW DATA

require(data.table)
require(stringr)
require(here)
require(dplyr)
require(tidygraph)

`%notin%` <- Negate(`%in%`)
if (str_detect(getwd(), "goutsmedt")) {
  data_path <- "C:/Users/goutsmedt/Mon Drive/data"
} else {
  if (str_detect(getwd(), "Dropbox")) {
    data_path <- "G:/.shortcut-targets-by-id/1EHqA0pp2uozTykWv0_Stf5xyrvo_dkN5/data"
  } else {
    data_path <- "/projects/data/macro_AA"
  }
}
source("functions/Script_paths_and_basic_objectsV2.R")
source("functions/functions_for_network_analysis.R")
`%notin%` <- Negate(`%in%`)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Gather necessary tables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## Nodes Table
list_graph_position <- readRDS(here(data_path,"macro_AA","4_Networks","list_graph_position_intertemporal_naming_1969-2011.RDS"))
nodes_lf <- lapply(list_graph_position, function(tbl) (tbl %>% activate(nodes) %>% as.data.table()))
nodes_lf <- lapply(nodes_lf, function(dt) (dt[, .(ID_Art,ItemID_Ref,Com_ID,new_Id_com)]))
nodes_lf <- rbindlist(nodes_lf, idcol = "window")
nodes <- nodes_lf[, window := paste0(window, "-", as.integer(window) + time_window-1)][order(ID_Art, window)]

## tf-idf Table
tf_idf_table <- readRDS(here(graph_data_path, paste0("alluv_dt_", first_year, "-", last_year, ".rds")))
tf_idf_table <- split(tf_idf_table[order(Window)], by="Window")

## Authors Table
authors <- readRDS(here(data_path, "macro_AA","OST_generic_data", "all_aut.RDS")) %>% .[ID_Art %in% nodes$ID_Art]
authors[,ID_Art:=as.character(ID_Art)]
## keep one letter after name
authors <- authors[, name_short := str_replace_all(Nom,"\\.", "-")] # replace . by -
authors <- authors[, name_short := str_replace_all(name_short,"(?<=\\-.).*", "")]
authors$name_short <- toupper(authors$name_short)
authors <- authors[, Nom := name_short]
authors[, c("name_short") := NULL]

## Refs Table
refs <- arrow::read_parquet(here(data_path, "macro_AA","OST_generic_data", "all_ref.parquet"),as.data.frame=FALSE) %>% .[ID_Art %in% nodes$ID_Art]
refs[,ID_Art:=as.character(ID_Art)]

## Refs_info Table
refs_info <- readRDS(here(data_path, "macro_AA","3_Corpus_WoS", "JEL_matched_corpus_references_info.rds"))

## Number of nodes per community per window
n_nodes_window_com <- nodes[,.N,.(window,new_Id_com)] %>% rename(n_nodes_window_com = N)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Most Occurring Authors Table ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

authors_table <- merge(nodes[,.(ID_Art,window,new_Id_com)],authors[,.(ID_Art,Nom,Ordre)],by="ID_Art",allow.cartesian=TRUE)

## Compute
authors_table[,n_authors:=.N,.(window,new_Id_com,Nom)]
authors_table <- merge(authors_table, n_nodes_window_com,by=c("window","new_Id_com"), all.x = TRUE)
authors_table[,share_of_paper_authored:=n_authors/n_nodes_window_com*100]

## final table
authors_table <- authors_table[order(window,new_Id_com,-share_of_paper_authored,Nom),.(window,new_Id_com,Nom,share_of_paper_authored)] %>% unique()
authors_table <- authors_table[,table_name:="Most_occurring_authors"] %>% rename("Share of Paper Authored" = share_of_paper_authored)
authors_table[,`Share of Paper Authored`:=paste0(round(`Share of Paper Authored`,2)," %")]

authors_table <- authors_table[, head(.SD, 20), .(window,new_Id_com)]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Most Cited Refs Table ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

most_cited_refs <- merge(nodes[,.(ID_Art,window,new_Id_com)],refs[New_id2!=0,.(ID_Art,New_id2,Nom,Annee,ItemID_Ref,Revue_Abbrege)],by="ID_Art",allow.cartesian=TRUE)

# Label column
most_cited_refs <- most_cited_refs[, name_short:=  gsub("-.*","",Nom)]
most_cited_refs$name_short <- toupper(most_cited_refs$name_short)
most_cited_refs <- most_cited_refs[,Label_Target:=paste0(name_short,",",Annee)]
most_cited_refs[, c("name_short"):=NULL]
most_cited_refs[, c("Nom"):=NULL]
most_cited_refs[, c("Annee"):=NULL]

# keep most common label per New_id2
better_label <- copy(most_cited_refs[,.N,.(New_id2,Label_Target)])
better_label <- better_label[order(-N), head(.SD, 1), .(New_id2)]

# Add better labels
most_cited_refs[, c("Label_Target"):=NULL]
most_cited_refs <- merge(most_cited_refs, better_label,by="New_id2", all.x = TRUE)

## Compute
most_cited_refs[,n_citations:=.N,.(window,new_Id_com,New_id2)]
most_cited_refs <- merge(most_cited_refs, n_nodes_window_com,by=c("window","new_Id_com"), all.x = TRUE)
most_cited_refs[,share_of_paper_citing:=n_citations/n_nodes_window_com*100]

## Add refs info
most_cited_refs_info <- merge(most_cited_refs, refs_info[,.(ItemID_Ref,Titre,Revue)], by="ItemID_Ref",all.x = TRUE)
most_cited_refs_info[is.na(Revue),Revue:=Revue_Abbrege]
stopifnot(nrow(most_cited_refs)==nrow(most_cited_refs_info))

## final table
most_cited_refs_info <- most_cited_refs_info[order(window,new_Id_com,-share_of_paper_citing,Label_Target),.(window,new_Id_com,Label_Target,Titre,Revue,share_of_paper_citing)] %>% unique()
most_cited_refs_info <- most_cited_refs_info[,table_name:="Most_cited_references"] %>% rename("Share of Paper Citing" = share_of_paper_citing)
most_cited_refs_info[,`Share of Paper Citing`:=paste0(round(`Share of Paper Citing`,2)," %")]

most_cited_refs_info <- most_cited_refs_info[, head(.SD, 20), .(window,new_Id_com)]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### tf-idf Table ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

tf_idf_results <- lapply(tf_idf_table, function(dt){tf_idf(nodes = dt,
                                                           com_name_column = "new_Id_com",
                                                           number_of_words = 20, 
                                                           threshold_com = 0.05,
                                                           com_size_column = "share_leiden",
                                                           size_title_wrap = 10)})

tf_idf_table_final <- lapply(tf_idf_results, function(dt) dt[[2]])
tf_idf_table_final <- lapply(tf_idf_table_final, function(dt) dt[,new_Id_com:=Com_ID])
tf_idf_table_final <- lapply(tf_idf_table_final, function(dt) dt[,.(new_Id_com,word,tf_idf,count)])
tf_idf_table_final <- rbindlist(tf_idf_table_final, idcol = "window")
tf_idf_table_final <- tf_idf_table_final[order(window, new_Id_com, -tf_idf), window := paste0(window, "-", as.integer(window) + time_window-1)]
tf_idf_table_final <- tf_idf_table_final[,table_name:="tf_idf"]
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### All Tables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

master_table_info_com <- rbind(authors_table,most_cited_refs_info, tf_idf_table_final, fill=TRUE)
master_table_info_com[is.na(master_table_info_com)] <- ""

write.csv(master_table_info_com, here(data_path,"macro_AA","5_platform_data","master_table_info_com.csv"))

