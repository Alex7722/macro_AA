# THIS IS A TEMPORARY VERSION FOR PHILIPPE, TO REDO FOR NEW DATA

require(data.table)
require(stringr)
require(here)
require(dplyr)

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

nodes <- fread(here(data_path, "macro_AA","platform_data","nodes_lf.csv"))
authors <- readRDS(here(data_path, "macro_AA","OST_generic_data", "all_aut.RDS")) %>% .[ID_Art %in% nodes$ID_Art]
refs <- arrow::read_parquet(here(data_path, "macro_AA","OST_generic_data", "all_ref.parquet"),as.data.frame=FALSE) %>% .[ID_Art %in% nodes$ID_Art]

n_nodes_window_com <- nodes[,.N,.(window,Com_ID)] %>% rename(n_nodes_window_com = N)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Most Occurring Authors Table ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

authors_table <- merge(nodes[,.(ID_Art,window,Com_ID)],authors[,.(ID_Art,Nom,Ordre)],by="ID_Art",allow.cartesian=TRUE)

## keep one letter after name
authors_table <- authors_table[, name_short := str_replace_all(Nom,"(?<=-.).", "")]
authors_table$name_short <- toupper(authors_table$name_short)
authors_table <- authors_table[, Nom := name_short]
authors_table[, c("name_short") := NULL]

## compute
authors_table[,n_authors:=.N,.(window,Com_ID,Nom)]
authors_table <- merge(authors_table, n_nodes_window_com,by=c("window","Com_ID"), all.x = TRUE)
authors_table[,share_of_paper_authored:=n_authors/n_nodes_window_com*100]
authors_table <- authors_table[order(window,Com_ID,-share_of_paper_authored,Nom),.(window,Com_ID,Nom,share_of_paper_authored)] %>% unique()

## final table
authors_table <- authors_table[,table_name:="Most_occurring_authors"] %>% rename("Share of Paper Authored" = share_of_paper_authored)
authors_table[,`Share of Paper Authored`:=paste0(round(`Share of Paper Authored`,2)," %")]

authors_table <- authors_table[, head(.SD, 20), .(window,Com_ID)]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Most Cited Refs Table ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

most_cited_refs <- merge(nodes[,.(ID_Art,window,Com_ID)],refs[New_id2!=0,.(ID_Art,New_id2,Nom,Annee)],by="ID_Art",allow.cartesian=TRUE)

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

# add better labels
most_cited_refs[, c("Label_Target"):=NULL]
most_cited_refs <- merge(most_cited_refs, better_label,by="New_id2", all.x = TRUE)


most_cited_refs[,n_citations:=.N,.(window,Com_ID,New_id2)]
most_cited_refs <- merge(most_cited_refs, n_nodes_window_com,by=c("window","Com_ID"), all.x = TRUE)
most_cited_refs[,share_of_paper_citing:=n_citations/n_nodes_window_com*100]
most_cited_refs <- most_cited_refs[order(window,Com_ID,-share_of_paper_citing,Label_Target),.(window,Com_ID,Label_Target,share_of_paper_citing)] %>% unique()

most_cited_refs <- most_cited_refs[,table_name:="Most_cited_references"] %>% rename("Share of Paper Citing" = share_of_paper_citing)
most_cited_refs[,`Share of Paper Citing`:=paste0(round(`Share of Paper Citing`,2)," %")]

most_cited_refs <- most_cited_refs[, head(.SD, 20), .(window,Com_ID)]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### All Tables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

master_table_info_com <- rbind(authors_table,most_cited_refs,fill=TRUE)
master_table_info_com[is.na(master_table_info_com)] <- ""

write.csv(master_table_info_com, here(data_path,"macro_AA","5_platform_data","master_table_info_com.csv"))
