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
refs <- arrow::read_parquet(here(data_path, "macro_AA","OST_generic_data", "all_ref.parquet"),as.data.frame=FALSE) %>% 
  .[ID_Art %in% nodes$ID_Art]
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
####  Origin Table ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

alluv_dt <- readRDS(here(graph_data_path, "alluv_dt_named_colored.rds"))
origin <- split(alluv_dt[order(Window)], by="Window")

list_years_origin <- list()
for (Year in all_years[-1]) {
  present <- origin[[paste0(Year)]]
  present_ls <- split(present[order(new_Id_com)], by="new_Id_com") # split communities as list
  past <- origin[[paste0(Year-1)]] # get nodes from past
  
  com_list <- as.character(present[,.N,new_Id_com][,new_Id_com])
  list_com_origin <- list()
  for (new_Id_com in com_list) {
    present_nodes <- present_ls[[paste0(new_Id_com)]][,ID_Art]
    nodes_from_the_past <- past[ID_Art %in% present_nodes] # for each com find out the nodes that are in common with the past
    com_origin <- nodes_from_the_past[,.N,new_Id_com] # compute share
    com_origin[,`Share of Origin`:=round(N/length(present_nodes)*100,1)]
    com_origin[,`Community of Origin`:=new_Id_com]
    
    #compute new nodes
    share_new <- com_origin[,100-sum(`Share of Origin`)]
    com_origin <- rbind(com_origin, data.table(new_Id_com=new_Id_com, `Community of Origin`="New Articles", `Share of Origin`=share_new),fill=TRUE)
    
    list_com_origin[[paste0(new_Id_com)]] <- com_origin[,.(`Community of Origin`, `Share of Origin`)]
  }
  list_years_origin[[paste0(Year)]] <- rbindlist(list_com_origin, idcol = "new_Id_com")
}
com_origin <- rbindlist(list_years_origin, idcol = "window")
com_origin <- com_origin[, window := paste0(window, "-", as.integer(window) + time_window-1)]
com_origin <- com_origin[order(-`Share of Origin`,window,new_Id_com), head(.SD, 10), .(window,new_Id_com)][,.(window,new_Id_com,`Community of Origin`,`Share of Origin`)]

com_origin <- merge(com_origin,
                    alluv_dt[,.(new_Id_com,`sub-name`,`meta-name`)][,head(.SD,1),new_Id_com], # retrieve the name of communities evolution/origin
                    by.x="Community of Origin", by.y="new_Id_com", all.x=TRUE)
com_origin[`Community of Origin`=="New Articles",`meta-name`:="New Articles"]
com_origin[,`Community of Origin`:=paste0(`meta-name`," - ",`sub-name`)]
com_origin[`Community of Origin`=="NA - NA",`Community of Origin`:="Unnamed Community"]
com_origin[`Community of Origin`=="New Articles - NA",`Community of Origin`:="New Articles"]

com_origin[,table_name:="Origin_communities"]

com_origin <- com_origin[,.(new_Id_com, window, `Community of Origin`, `Share of Origin`,table_name)]
com_origin <- com_origin[new_Id_com %in% alluv_dt[!is.na(`meta-name`)]$new_Id_com]

com_origin <- com_origin[order(window,new_Id_com,-`Share of Origin`,)]
com_origin <- com_origin[,`Share of Origin`:=paste0(`Share of Origin`," %")]

good_example_origin <- com_origin[window=="1983-1987" & new_Id_com=="o2GS150I"]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
####  Evolution Table ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

alluv_dt <- readRDS(here(graph_data_path, "alluv_dt_named_colored.rds"))
origin <- split(alluv_dt[order(Window)], by="Window")

list_years_origin <- list()
for (Year in all_years[-length(all_years)]) {
  present <- origin[[paste0(Year)]]
  present_ls <- split(present[order(new_Id_com)], by="new_Id_com") # split communities as list
  future <- origin[[paste0(Year+1)]] # get nodes from past
  
  com_list <- as.character(present[,.N,new_Id_com][,new_Id_com])
  list_com_origin <- list()
  for (new_Id_com in com_list) {
    present_nodes <- present_ls[[paste0(new_Id_com)]][,ID_Art]
    nodes_from_the_future <- future[ID_Art %in% present_nodes] # for each com find out the nodes that are in common with the past
    com_future <- nodes_from_the_future[,.N,new_Id_com] # compute share
    com_future[,`Share of Destiny`:=round(N/length(present_nodes)*100,1)] # compute % as share of present nodes (for share of comon nodes replace lenght() by sum(N))
    com_future[,`Communities of Destiny`:=new_Id_com]
    
    #compute new nodes
    share_new <- com_future[,100-sum(`Share of Destiny`)]
    com_future <- rbind(com_future, data.table(new_Id_com=new_Id_com, `Communities of Destiny`="Dropped Articles", `Share of Destiny`=share_new),fill=TRUE)
    
    list_com_origin[[paste0(new_Id_com)]] <- com_future[,.(`Communities of Destiny`, `Share of Destiny`)]
  }
  list_years_origin[[paste0(Year)]] <- rbindlist(list_com_origin, idcol = "new_Id_com")
}
com_future <- rbindlist(list_years_origin, idcol = "window")
com_future <- com_future[, window := paste0(window, "-", as.integer(window) + time_window-1)]
com_future <- com_future[order(-`Share of Destiny`,window,new_Id_com), head(.SD, 10), .(window,new_Id_com)][,.(window,new_Id_com,`Communities of Destiny`,`Share of Destiny`)]

com_future <- merge(com_future, 
                    alluv_dt[,.(new_Id_com,`sub-name`,`meta-name`)][,head(.SD,1),new_Id_com], # retrieve the name of communities evolution/origin
                    by.x="Communities of Destiny", by.y="new_Id_com", all.x=TRUE)
com_future[`Communities of Destiny`=="Dropped Articles",`meta-name`:="Dropped Articles"]
com_future[,`Communities of Destiny`:=paste0(`meta-name`," - ",`sub-name`)]
com_future[`Communities of Destiny`=="NA - NA",`Communities of Destiny`:="Unnamed Community"]
com_future[`Communities of Destiny`=="Dropped Articles - NA",`Communities of Destiny`:="Dropped Articles"]

com_future[,table_name:="Evolution_communities"]

com_future <- com_future[,.(new_Id_com, window, `Communities of Destiny`, `Share of Destiny`,table_name)]
com_future <- com_future[new_Id_com %in% alluv_dt[!is.na(`meta-name`)]$new_Id_com]

com_future <- com_future[order(window,new_Id_com,-`Share of Destiny`,)]
com_future <- com_future[,`Share of Destiny`:=paste0(`Share of Destiny`," %")]

good_example_future <- com_future[window=="1983-1987" & new_Id_com=="o2GS150I"]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Most Cited Communities ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#com_size <- copy(nodes[,.N,.(window,new_Id_com)])
#setnames(com_size, "N", "com_size")
#setnames(com_size, "new_Id_com", "ref_com")

most_cited_communities_nodes <- copy(nodes[ItemID_Ref!=0])
most_cited_communities_nodes <- most_cited_communities_nodes[,.(window, ref_com=new_Id_com, ItemID_Ref)]

most_cited_refs <- merge(nodes[,.(ID_Art,window,new_Id_com)],refs[ItemID_Ref!=0,.(ID_Art,ItemID_Ref)],by="ID_Art",allow.cartesian=TRUE)

#most_cited_com <- merge(most_cited_refs, most_cited_communities_nodes, by = c("window","ItemID_Ref"), all.x = TRUE) # we merge the most cited ref with the table of nodes on windows and ItemID_Ref (so ref of nodes in the same network)
#most_cited_com <- most_cited_com[!is.na(ref_com),.N,.(window,new_Id_com,ref_com)][order(-N)]
#setnames(most_cited_com, "N", "n_cite")
#most_cited_com[,tot_cit:=sum(n_cite),.(window, new_Id_com)]
#most_cited_com[,share_citation_by_com:=n_cite/tot_cit]
#most_cited_com <- merge(most_cited_com, com_size, by = c("window","ref_com"), all.x = TRUE) # we merge the most cited ref with the table of nodes on windows and ItemID_Ref (so ref of nodes in the same network)
#most_cited_com[,tot_size:=sum(com_size),.(window, new_Id_com)]
#most_cited_com[,share_size:=com_size/tot_size]

#most_cited_com[,weighted_share := share_citation_by_com/share_size]

#most_cited_com[,`Weighted Share of Citations to`:= weighted_share/sum(weighted_share),.(window, new_Id_com)]
#most_cited_com[,`Weighted Share of Citations to`:= paste0(round(`Weighted Share of Citations to`*100,2)," %")]

#most_cited_com[,`Share of Citations to`:= paste0(round(share_citation_by_com*100,2)," %")]

#most_cited_com[window=="2009-2013" & new_Id_com=="oVarxM1p"][order(-n_cite)]

#most_cited_com_table <- copy(most_cited_com)
#setnames(most_cited_com_table, "ref_com", "Citations to")

#most_cited_com_table <- merge(most_cited_com_table, 
 #                   alluv_dt[,.(new_Id_com,`sub-name`,`meta-name`)][,head(.SD,1),new_Id_com], # retrieve the name of communities evolution/origin
  #                  by.x="Citations to", by.y="new_Id_com", all.x=TRUE)
#most_cited_com_table[,`Citations to`:= paste0(`sub-name`, " - ",`meta-name`)]
#most_cited_com_table[`Citations to`=="NA - NA",`Citations to`:="Unnamed Community"]

#most_cited_com_table[,table_name:="Cluster_Cited"]

#most_cited_com_table <- most_cited_com_table[,.(window, new_Id_com, `Citations to`, `Share of Citations to`, `Weighted Share of Citations to`, table_name)]

#most_cited_com_table[window=="2009-2013" & new_Id_com=="oVarxM1p"][order(-`Share of Citations to`)]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Most Citing and Cited Communities ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#com_size <- copy(nodes[,.N,.(window,new_Id_com)])
#setnames(com_size, "N", "com_size")

com_links <- merge(most_cited_refs, most_cited_communities_nodes, by = c("window","ItemID_Ref"), all.x = TRUE) # we merge the most cited ref with the table of nodes on windows and ItemID_Ref (so ref of nodes in the same network)
com_links <- com_links[!is.na(ref_com), total_citing := .N, .(window, new_Id_com)] %>% # Number of citations given per a cluster
  .[!is.na(ref_com),] %>% 
  .[, total_cited := .N, .(window, ref_com)] %>% # Number of citations received per a cluster
  .[, n_cited := .N, .(window,new_Id_com,ref_com)] %>% # Sum of links between two clusters
  select(-ItemID_Ref, - ID_Art) %>% 
  unique() %>% 
  .[, `:=` (share_from = n_cited/total_cited, # share of citation coming from a specific cluster
            share_to = n_cited/total_citing, # share of citation going to a specific cluster
            weighted_share_from = n_cited/total_citing, # take into account number of citations of citing cluster
            weighted_share_to = n_cited/total_cited)] %>% # take into account number of citations of cited cluster
  .[, `:=` (weighted_share_from = weighted_share_from/sum(weighted_share_from)), .(window, ref_com)] %>% 
  .[, `:=` (weighted_share_to = weighted_share_to/sum(weighted_share_to)), .(window, new_Id_com)] %>% 
  mutate(across(contains("share_"), ~round(., 4) * 100)) 

setnames(com_links,
         c("share_from", "share_to", "weighted_share_from", "weighted_share_to"),
         c("Share of citations from", "Share of citations going to", "Weighted share of citations from", "Weighted share of citations going to"))
  


#most_cited_from_com <- merge(most_cited_refs, most_cited_communities_nodes, by = c("window","ItemID_Ref"), all.x = TRUE) %>% # we merge the most cited ref with the table of nodes on windows and ItemID_Ref (so ref of nodes in the same network)
#.[!is.na(ref_com)]  
#most_cited_from_com <- most_cited_from_com[, .N,.(window,new_Id_com,ref_com)][order(-N)]
#setnames(most_cited_from_com, "N", "n_cited")
#most_cited_from_com[,tot_cit:=sum(n_cited),.(window, ref_com)]
#most_cited_from_com[,share_citation_from_com:=n_cited/tot_cit]
#most_cited_from_com <- merge(most_cited_from_com, com_size, by = c("window","new_Id_com"), all.x = TRUE) # we merge the most cited ref with the table of nodes on windows and ItemID_Ref (so ref of nodes in the same network)
#most_cited_from_com[,tot_size:=sum(com_size),.(window, ref_com)]
#most_cited_from_com[,share_size:=com_size/tot_size]

#most_cited_from_com[,weighted_share_from := share_citation_from_com/share_size]
#most_cited_from_com[,`Weighted Share of Citations from`:= weighted_share_from/sum(weighted_share_from),.(window, new_Id_com)]
#most_cited_from_com[,`Weighted Share of Citations from`:= paste0(round(`Weighted Share of Citations from`*100,2)," %")]

#most_cited_from_com[,`Share of Citations from`:=paste0(round(share_citation_from_com*100,2)," %")]

#most_cited_from_com[window=="2009-2013" & ref_com=="oVarxM1p"][order(-n_cited)]

#most_cited_from_com_table <- copy(most_cited_from_com)
#setnames(most_cited_from_com_table, "new_Id_com", "Citations from")
#setnames(most_cited_from_com_table, "ref_com", "new_Id_com")

most_cited_from_com_table <- merge(com_links, 
                              alluv_dt[,.(new_Id_com,`sub-name`,`meta-name`)][,head(.SD,1),new_Id_com], # retrieve the name of communities evolution/origin
                              by = "new_Id_com", all.x=TRUE) 
most_cited_from_com_table[,`Citations from`:= paste0(`sub-name`, " - ",`meta-name`)]
most_cited_from_com_table[`Citations from`=="NA - NA",`Citations from`:="Unnamed Community"]
most_cited_from_com_table <- most_cited_from_com_table %>% 
  select(window, new_Id_com = ref_com, `Citations from`, `citations` = n_cited, `Share of citations from`, `Weighted share of citations from`) %>% 
  mutate(table_name = "Cluster_citing_origins")

#most_cited_from_com_table <- most_cited_from_com_table[,.(window, new_Id_com, `Citations from`, `Share of Citations from`, `Weighted Share of Citations from`, table_name)]

# most_cited_from_com_table[window=="2009-2013" & new_Id_com=="oVarxM1p"][order(-`Share of Citations from`)]
  
  most_cited_to_com_table <- merge(com_links, 
                                     alluv_dt[,.(new_Id_com,`sub-name`,`meta-name`)][,head(.SD,1),new_Id_com], # retrieve the name of communities evolution/origin
                                     by.x = "ref_com", by.y = "new_Id_com", all.x=TRUE) 
  most_cited_to_com_table[,`Citations to`:= paste0(`sub-name`, " - ",`meta-name`)]
  most_cited_to_com_table[`Citations to`=="NA - NA",`Citations to`:="Unnamed Community"]
  most_cited_to_com_table <- most_cited_to_com_table %>% 
    select(window, new_Id_com, `Citations to`, `citations` = n_cited, `Share of citations going to`, `Weighted share of citations going to`) %>% 
  mutate(table_name = "Cluster_citing_destination")
  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### All Tables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

master_table_info_com <- rbind(authors_table,most_cited_refs_info, tf_idf_table_final, com_origin, com_future, most_cited_from_com_table, most_cited_to_com_table, fill=TRUE)
master_table_info_com[is.na(master_table_info_com)] <- ""

write.csv(master_table_info_com, here(data_path,"macro_AA","5_platform_data","master_table_info_com.csv"))

