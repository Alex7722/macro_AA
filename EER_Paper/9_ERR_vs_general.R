#' ---
#' title: "Script for building the EER networks for moving time window"
#' author: "Aurélien Goutsmedt and Alexandre Truc"
#' date: "/ Last compiled on `r format(Sys.Date())`"
#' output: 
#'   github_document:
#'     toc: true
#'     number_sections: true
#' ---
#' 
#' # What is this script for?
#' 
#' This script aims at replicating the [1_building_dynamic_networks.md](/dynamic_networks/1_building_dynamic_networks.md) 
#' but for the EER. It uses the data extracted in [1_Script_EER_corpus.md](/EER_Paper/1_Script_EER_corpus.md) and 
#' it creates the networks of EER publications for different time windows. 
#' We want one-year moving time windows on the whole period (1969-2016) and we need functions automating the creation
#' of the different windows. This script creates the networks, finds communities, integrates the name 
#' of communities, calculates coordinates and saves the nodes and edges data in a long format.
#' 
#' 
#' > WARNING: This script still needs a lot of cleaning
#' 
#' 
#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)
set.seed(500)
#' # Loading packages, paths and data
#' 
library(png)
library(grid)
library(ggnewscale)
library(vite)
library(RMySQL)
library(NetworkToolbox)
library(broom)
library(igraph)
library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)
library(tm)
library(tidyr)
library(tidytext)
library('cluster')
library('ggraph')
library('tibble')
library('tidygraph')
library(ggrepel)
library(readr)
# library(leiden)
library(ggraph)
library(ggnewscale)
library(remotes)
library(vite)
library("reticulate")
library(reticulate)
# library(leiden)
library(tidygraph)
library(rlang)
# library(leiden)
library(ggforce)
library(d3Network)
library(scales)
library(RColorBrewer)
require(DescTools)
require(stringr)
library(docstring)
library(quanteda)
library(pander)
library(DT)
require(forcats)
require(tidyverse)
library(sigmajs)
setwd("/projects/data/macro_AA")
data_path <- "/projects/data/macro_AA/"

source("~/macro_AA/EER_Paper/Script_paths_and_basic_objects_EER.R")
source("~/macro_AA/functions/functions_for_network_analysis.R")
source("/home/alexandre/functions_dynamics_networks.R")
source("/home/alexandre/functions_networks.R")
source("~/macro_AA/logins_DO_NOT_UPLOAD.R")
ESH <- dbConnect(MySQL(),
                 user = usr, password = pswd, dbname = "OST_Expanded_SciHum",
                 host = "127.0.0.1"
)
# Corpus1 <- readRDS(file = paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Corpus.rds"))
# Institutions <- readRDS(file = paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Institutions.rds"))
# Authors <- readRDS(paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Authors.rds"))
# Refs1 <- readRDS(paste0(data_path,"EER/1_Corpus_Prepped_and_Merged/Refs.rds"))
# Refs1 <- Refs1[ItemID_Ref_Target!=0]

Corpus2 <- readRDS("/projects/data/macro_AA/Corpus_Econlit_Matched_WoS/JEL_matched_corpus_nodes.rds")
Refs2 <- readRDS(file = "/projects/data/macro_AA/Corpus_Econlit_Matched_WoS/JEL_matched_corpus_references_info.rds")

Refs3 <- readRDS(file = "/projects/data/macro_AA/Corpus_Econlit_Matched_WoS/Old_JEL_matched_corpus_references_info.rds")
Corpus3 <- readRDS("/projects/data/macro_AA/Corpus_Econlit_Matched_WoS/Old_JEL_matched_corpus_nodes.rds")


# Refs2 <- Refs2[ItemID_Ref!=0]
# Refs3 <- Refs3[ItemID_Ref!=0]
# Corpus

Corpus <- rbind(Corpus2, Corpus3, fill=TRUE)
Refs <- rbind(Refs2, Refs3, fill=TRUE)
Corpus <- Corpus[Code_Revue=="9662" | Code_Revue=="13694" | Code_Revue=="4695" | Code_Revue=="13992" | Code_Revue=="758" | Code_Revue=="5200"]
Refs <- Refs[ID_Art %in% Corpus$ID_Art]

Top5_abstract <- fread("EER/Top5/TOP5_AB.csv")
long_ab <- spread(Top5_abstract, Ordre, Abstract)
long_ab <- tidyr::unite(long_ab, Abstract, -Id_Art) 

Top5_art <- fread("EER/Top5/TOP5_ART.csv")





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Collaborations ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

Institution <- fread(file = "/projects/data/macro_AA/Corpus_Econlit_Matched_WoS/Macro_AA_Institutions_Cleaned.csv",fill=TRUE)
# Institution[,ID_Art:=as.character(ID_Art)]

institutions <- merge(Corpus[,.(ID_Art, Annee_Bibliographique)], Institution, by="ID_Art",all.x = TRUE)
# institutions <- institutions[,head(.SD, 1),.(ID_Art,Institution)]
institutions <- institutions[Pays=="PEOPLES-R-CHINA", Pays:="CHINA"]
institutions[Pays=="FED-REP-GER", Pays:="GERMANY"]
institutions[Pays=="WEST-GERMANY", Pays:="GERMANY"]
institutions[Pays=="CZECHOSLOVAKIA" | Pays=="CZECH-REPUBLIC", Pays:="CZECH REPUBLIC"]
institutions_unique <- institutions[,head(.SD, 1),.(ID_Art,Institution)]
institutions_unique <- institutions_unique[Institution!="NULL" | Pays!="NULL" ]
institutions_unique[, Countries_grouped:=Pays]
institutions_unique[Countries_grouped=="CZECHOSLOVAKIA" | Countries_grouped=="CZECH-REPUBLIC", Countries_grouped:="CZECH REPUBLIC"]
institutions_unique[Countries_grouped=="FED-REP-GER", Countries_grouped:="GERMANY"]

UE <- fread("EER/Europe_continent.csv") %>% data.table 
institutions_unique[toupper(Pays) %in% toupper(UE$Countries), Countries_grouped:="Europe"]
# Identifying Collaborations
institutions_unique[,EU:=0][Countries_grouped=="Europe", EU:=1][,EU:=sum(EU),ID_Art]
institutions_unique[,US:=0][Countries_grouped=="USA", US:=1][,US:=sum(US),ID_Art]
institutions_unique[EU>=1 & US>=1, EU_US_collab:= "Collaborations", ID_Art]
institutions_unique[EU==0 & US>=1, EU_US_collab:= "Americans\n(No Europeans)", ID_Art]
institutions_unique[EU>=1 & US==0, EU_US_collab:= "Europeans\n(No Americans)", ID_Art]
institutions_unique[EU==0 & US==0, EU_US_collab:= "Neither", ID_Art]
institutions_unique[,ID_Art:=as.character(ID_Art)]
require(dplyr)
count_year <- institutions_unique[, head(.SD, 1), .(ID_Art)][Annee_Bibliographique<2016]
count_year <- count_year %>% group_by(Annee_Bibliographique, EU_US_collab) %>% summarise(n = n()) %>%  mutate(freq = n / sum(n)) %>% as.data.table()

count_year <- complete(count_year, Annee_Bibliographique, EU_US_collab) %>% as.data.table
count_year[is.na(n),n:=1]
count_year[is.na(freq),freq:=0]

ggplot(count_year, aes(x=Annee_Bibliographique, y=freq, group=EU_US_collab, color=EU_US_collab)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.4) +
  # geom_point() +
  labs(fill = "Countries (Top 7)") +
  scale_x_continuous("Years") +
  scale_y_continuous("Share of Papers Authored by European and American Economists", limits = c(0, 0.8), breaks = seq(0,.8,0.2)) +
  theme_minimal() +
  scale_fill_manual(values = c(rev(brewer.pal(n = 9, name = "Paired")))) +
  scale_color_discrete(guide = "none") +
  ggrepel::geom_label_repel(aes(label = EU_US_collab), data = count_year[Annee_Bibliographique==2015], nudge_x = 1, segment.color = NA) +
  coord_cartesian(xlim = c(NA, 2024)) 
  ggsave("Graphs/Collab.png", width=286, height=215, units = "mm")

institutions_unique <- institutions_unique[,ID_Art:=as.integer(ID_Art)]
Corpus <- merge(Corpus, institutions_unique[,.N,.(EU_US_collab,ID_Art)][,.N,.(EU_US_collab,ID_Art)][,.(EU_US_collab,ID_Art)], by="ID_Art",all.x=TRUE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Share of Refs####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
Corpus_EER <- Corpus[Code_Revue=="5200"]
Corpus_top5 <- Corpus[Code_Revue=="9662" | Code_Revue=="13694" | Code_Revue=="4695" | Code_Revue=="13992" | Code_Revue=="758"]
Refs_top5 <- Refs[ID_Art %in% Corpus_top5$ID_Art]
Refs_EER <- Refs[ID_Art %in% Corpus_EER$ID_Art]

all_EER <- readRDS(file = "EER/1_Corpus_Prepped_and_Merged/Corpus.rds")
all_top5 <- dbGetQuery(ESH, paste0("SELECT * FROM OST_Expanded_SciHum.Articles;")) %>% data.table()
all_top5 <- all_top5[Code_Revue=="9662" | Code_Revue=="13694" | Code_Revue=="4695" | Code_Revue=="13992" | Code_Revue=="758"]



# Refs EER in top 5

Refs_top5[ItemID_Ref %in% all_EER$ItemID_Ref][,.N,Revue_Abbrege]
Refs_top5[,EER_ref:=0]
Refs_top5[Revue_Abbrege %like% "EUR ECON REV%" | 
            Revue_Abbrege %like% "EUROPEAN EC REV%" | 
            Revue_Abbrege %like% "EUROPEAN ECONOMIC RE%" | 
            Revue_Abbrege %like% "EUR EC REV%",
          EER_ref:=1]

Refs_top5 <- merge(Refs_top5, Corpus_top5[,.(ID_Art,Annee_Bibliographique)], by="ID_Art",all.x = TRUE)

share_EER_Refs <- Refs_top5[,sum(EER_ref)/.N,Annee_Bibliographique]

# n_ref <- Refs_top5[, .N, .(Annee_Bibliographique)]
# share_ref_EER <- Refs_top5[, sum(EER_ref), .(Annee_Bibliographique)]

ggplot(share_EER_Refs, aes(x=Annee_Bibliographique, y=V1)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.75)+
  # geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.75)) +
  scale_x_continuous("Years") +
  scale_y_continuous("Share of EER in Top 5 Macro Refs") +
  coord_cartesian(ylim = c(0,NA), xlim = c(NA, 2022)) +
  scale_color_discrete(guide = FALSE) +
  theme_minimal() 



# Refs top 5 in EER

Refs_EER[ItemID_Ref %in% all_top5$ItemID_Ref][,.N,Revue_Abbrege]
Refs_EER[,top5_ref:=0]
Refs_EER[Revue_Abbrege %in% Refs_EER[ItemID_Ref %in% all_top5$ItemID_Ref][,.N,Revue_Abbrege]$Revue_Abbrege, top5_ref:=1]

Corpus_EER[,ID_Art:=as.character(ID_Art)]
Refs_EER <- merge(Refs_EER, Corpus_EER[,.(ID_Art,Annee_Bibliographique)], by="ID_Art",all.x = TRUE)

share_top5_Refs <- Refs_EER[,sum(top5_ref)/.N,Annee_Bibliographique]

ggplot(share_top5_Refs, aes(x=Annee_Bibliographique, y=V1)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.75)+
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.75)) +
  scale_x_continuous("Years") +
  scale_y_continuous("Share of Top 5 in EEN Macro Refs") +
  coord_cartesian(ylim = c(0,NA), xlim = c(NA, 2022)) +
  scale_color_discrete(guide = FALSE) +
  theme_minimal() 


library(latticeExtra)
# --> construct separate plots for each series
obj1 <- xyplot(V1 ~ Annee_Bibliographique, share_top5_Refs[order(Annee_Bibliographique)], type = "l" , lwd=2, col="steelblue")
obj2 <- xyplot(V1 ~ Annee_Bibliographique, share_EER_Refs[order(Annee_Bibliographique)], type = "l", lwd=2, col="#69b3a2")

# --> Make the plot with second y axis:
doubleYScale(obj1, obj2, add.ylab2 = TRUE, use.style=FALSE )

EER_top5 <- merge(share_top5_Refs,share_EER_Refs, by="Annee_Bibliographique",all.x = TRUE,all.y = TRUE)
EER_top5 <- melt(EER_top5, id.vars=c("Annee_Bibliographique"),  measure.vars=c("V1.x","V1.y"))
EER_top5[variable=="V1.x", variable:="Top 5 in EER"]
EER_top5[variable=="V1.y", variable:="EER in Top 5"]

ggplot(EER_top5, aes(x=Annee_Bibliographique, y=value, group=variable, color=variable)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.2)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.75)) +
  scale_x_continuous("Years") +
  scale_y_continuous("Share of EER in Top 5 Macro Refs") +
  coord_cartesian(ylim = c(0,NA), xlim = c(NA, 2022)) +
  scale_color_discrete(guide = FALSE) +
  ggrepel::geom_label_repel(aes(label = as.character(variable)), data = EER_top5[Annee_Bibliographique==2016], nudge_x = 1, segment.color = NA) +
  theme_minimal() 



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Dynamic Networks and Communities ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
Refs <- Refs[ItemID_Ref!=0]

time_window <- 7
first_year <- Corpus[order(Annee_Bibliographique), head(.SD, 1)]$Annee_Bibliographique
last_year <- (as.numeric(Corpus[order(-Annee_Bibliographique), head(.SD, 1)]$Annee_Bibliographique) - time_window + 1) # +1 to get the very last year in the window
last_year <- 1990
all_years <- first_year:last_year
all_years <- c(1975,1980,1985,1990,1995,2000,2005)

Corpus <- Corpus[Annee_Bibliographique>=1975]
Corpus[,ID_Art:=as.character(ID_Art)]
Corpus[,ItemID_Ref:=as.character(ItemID_Ref)]
Refs[,ID_Art:=as.character(ID_Art)]
Refs[,ItemID_Ref:=as.character(ItemID_Ref)]



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Coupling####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

tbl_coup_list <- dynamics_coupling_networks(corpus = Corpus, 
                                            references = Refs, 
                                            source = "ID_Art", 
                                            target = "ItemID_Ref", 
                                            time_variable = Annee_Bibliographique,
                                            time_window = time_window, 
                                            weight_treshold_value = 2)

Institutions[,.N,.(ID_Art,EU_US_collab)]

tbl_coup_list <- lapply(tbl_coup_list, function(tbl){tbl %>% activate(nodes) %>% mutate(Revue_EER_bin = ifelse(Code_Revue==5200,"EER","Top5"))})
tbl_coup_list <- lapply(tbl_coup_list, function(tbl){tbl %>% activate(edges) %>% mutate(EER_bin_to = .N()$Revue_EER_bin[to], EER_bin_from = .N()$Revue_EER_bin[from])})
tbl_coup_list <- lapply(tbl_coup_list, function(tbl){tbl %>% activate(edges) %>% mutate(EU_US_collab_to = .N()$EU_US_collab[to], EU_US_collab_from = .N()$EU_US_collab[from])})

inwardness <- function(tbl)
{
  separateness <- tbl %>% activate(edges) %>% as.data.table()
  nodes <- tbl %>% activate(nodes) %>% as.data.table()
  
  separateness[,EER_bin_from:=as.character(EER_bin_from)]
  separateness[,EER_bin_to:=as.character(EER_bin_to)]
  separateness <- separateness[EER_bin_from > EER_bin_to, c("EER_bin_to", "EER_bin_from") := list(EER_bin_from, EER_bin_to)]
  separateness[,tot_weight:=sum(weight)]
  
  separateness_couple <- separateness[,.(sum(weight), .N),.(EER_bin_from, EER_bin_to)]
  
  matrix <- as.matrix(get.adjacency(graph.data.frame(separateness_couple, directed=FALSE), type = "both", attr = "V1"))
  matrix <- scale(matrix, center = FALSE, scale = colSums(matrix))
  
  matrix <- melt(matrix) %>% as.data.table()
  return(matrix)
}


list_inwardness <- lapply(tbl_coup_list, inwardness)
list_inwardness <- rbindlist(list_inwardness, idcol = "Year")
list_inwardness[,couple:=paste0(Var1,"-",Var2)]

ggplot(list_inwardness,
       aes(x=as.numeric(Year), y=value, group=couple, color=couple)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  ylab("Share of Weighted Links Between Articles of the Same Category") +
  xlab(NULL) +
  # facet_wrap(~fct_rev(aut_id), scales = "free_y", nrow = 3) +
  geom_vline(xintercept = 1984, linetype="dashed",alpha=0.8) +
  geom_vline(xintercept = 1991, linetype="dashed",alpha=0.8) +
  # scale_fill_brewer(palette = "Set1",name = "Inwardness of:", labels = c("Economics Articles", "Non-Economics Articles")) + 
  # scale_color_brewer(palette = "Set1",name = "Inwardness of:", labels = c("Economics Articles", "Non-Economics Articles")) +
  theme_minimal() +
  coord_cartesian(ylim = c(0,1)) +
  theme(legend.position =  "bottom", legend.justification = "center") +
  guides(fill = guide_legend(title.position = "top", ncol= 2, title.hjust =0.5, byrow = TRUE))




tbl %>% #mix color
  activate(edges) %>%
  mutate(com_ID_to = .N()$color[to], com_ID_from = .N()$color[from]) %>%

# Main components and com
tbl_coup_list <- lapply(tbl_coup_list, main_components)
tbl_coup_list <- lapply(tbl_coup_list, detect_leiden_igraph, niter = 30000)
#' We name communities:
tbl_coup_list <- intertemporal_naming_function(tbl_coup_list, treshold_similarity = 0.55)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Alluvial ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
alluv_dt <- make_into_alluv_dt(tbl_coup_list)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Positions ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
list_networks <- lapply(tbl_coup_list, function(tbl){tbl %>% activate(nodes) %>% mutate(Leiden1=new_Id_com)})
list_networks <- lapply(list_networks, function(tbl){tbl %>% activate(nodes) %>% mutate(nb_cit=size)})
#What will be computed as size
list_networks <- lapply(list_networks, function(tbl){tbl %>% activate(nodes) %>% mutate(size=nb_cit_wos)})

list_graph_position <- list(list_networks$"1975",list_networks$"1980",list_networks$"1985",list_networks$"1990",list_networks$"1995",list_networks$"2000",list_networks$"2005")

list_graph_position <- list()
for (Year in all_years){
  list_graph_position[[as.character(Year)]] <- list_networks[[paste0(Year)]]
}

list_graph_position <- lapply(list_graph_position, layout_fa2_java)









list_chosen_journals <- list()
for (Year in all_years){
  
  list_chosen_journals[[as.character(Year)]] <- list_networks[[paste0(Year)]] %>% activate(nodes) %>% as.data.table %>% .[,.N,.(Revue)] %>% .$Revue %>% as.data.table()
}

journal_list <- rbindlist(list_chosen_journals)
journal_list <- journal_list %>% rename(Revue = ".")



color = data.table(
  Revue_EER_bin = c("EER","Not EER","Other"),
  color = brewer.pal(n = 3, name = 'Set1'))

color = data.table(
  EU_US_collab = Corpus[,.N,EU_US_collab]$EU_US_collab,
  color = brewer.pal(n = Corpus[,.N,EU_US_collab][,.N], name = 'Set1'))

# list_graph_position <- lapply(list_graph_position, function(tbl){tbl %>% activate(nodes) %>% mutate(Revue_EER_bin = ifelse(Code_Revue==5200,"EER","Not EER"))})
list_graph_position <- lapply(list_graph_position, function(tbl){tbl %>% activate(nodes) %>% select(-color)})
list_graph_position <- lapply(list_graph_position, color_tbl)
list_graph_position2 <- list_graph_position
list_ggplot <- list()

for (Year in all_years) {
  
  
  label_com <- list_graph_position2[[paste0(Year)]] %>% activate(nodes) %>% as.data.table()
  label_com <- label_com[,mean_coord_x:=mean(x), EU_US_collab]
  label_com <- label_com[,mean_coord_y:=mean(y), EU_US_collab]
  label_com <- label_com[color!="grey"][, head(.SD, 1), EU_US_collab]
  # label_com <- merge(label_com, label[,.(new_Id_com, Label_com)], all.x = TRUE)
  network_size <- label_com[,mean((mean_coord_x+mean_coord_y)/2)]
  
  # label_disc <- list_graph_position2[[paste0(Year)]] %>% activate(nodes) %>% as.data.table()
  # label_disc <- label_disc[,mean_coord_x:=mean(x), ESpecialite_custom]
  # label_disc <- label_disc[,mean_coord_y:=mean(y), ESpecialite_custom]
  # label_disc <- label_disc[color!="grey"][, head(.SD, 1), ESpecialite_custom]
  # label_disc <- merge(label_disc, label[,.(new_Id_com, Label_com)], all.x = TRUE)
  # network_size <- label_disc[,mean((mean_coord_x+mean_coord_y)/2)]
  
  list_ggplot[[as.character(Year)]] <- ggraph(list_graph_position2[[paste0(Year)]], "manual", x = x, y = y) +
    geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.5, strength =0.2) +
    geom_node_point(aes(fill = color, size = size), pch=21) +
    scale_edge_width_continuous(range = c(0.5, 1)) +
    theme_void() +
    ggrepel::geom_label_repel(data = label_com, aes(x = mean_coord_x, y = mean_coord_y, label = as.character(EU_US_collab), fill = color)) +
    theme(legend.position = "none") +
    scale_fill_identity() +
    scale_edge_colour_identity() +
    labs(title = paste0(as.character(Year),"-",as.character(Year+time_window-1)))
  # ggsave("Networks/coup_2000.png", width=30, height=20, units = "cm")
  
}
g <- ggpubr::ggarrange(plotlist=list_ggplot, common.legend = TRUE, legend="none")




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Direct Citation ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
tbl_coup_list <- dynamics_direct_networks(corpus = Corpus, 
                                          references = Refs, 
                                          source = "ID_Art", 
                                          target = "ItemID_Ref", 
                                          time_variable = Annee_Bibliographique,
                                          time_window = time_window, 
                                          weight_treshold_value = 1)

tbl_coup_list <- lapply(tbl_coup_list, function(tbl){tbl %>% activate(nodes) %>% mutate(Revue_EER_bin = ifelse(Code_Revue==5200,"EER","Top5"))})
tbl_coup_list <- lapply(tbl_coup_list, function(tbl){tbl %>% activate(edges) %>% mutate(EER_bin_to = .N()$Revue_EER_bin[to], EER_bin_from = .N()$Revue_EER_bin[from])})


tbl_coup_list <- lapply(tbl_coup_list, function(tbl){tbl %>% activate(edges) %>% as.data.table() %>% .[,.N,.(EER_bin_to,EER_bin_from)]})
test_flow_citations <- rbindlist(tbl_coup_list,idcol="Year")
test_flow_citations[,to_from:=paste0(EER_bin_from," to ",EER_bin_to)]

test_flow_citations[,tot_cit_year:=sum(N),Year]
test_flow_citations[,share:=N/tot_cit_year,]

ggplot(test_flow_citations, aes(x=as.numeric(Year), y=share, group=to_from, color=to_from)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.4) +
  geom_point() +
  # labs(fill = "Countries") +
  # scale_x_continuous("Years") +
  # scale_y_continuous("Countries Identified by Unique Institution by Paper") +
  theme_minimal() 
# scale_fill_manual(values = c(rev(brewer.pal(n = 9, name = "Paired")))) +
# scale_color_discrete(guide = FALSE) +
# geom_label_repel(aes(label = Countries_grouped), data = count_year[Annee_Bibliographique==2019], nudge_x = 1, segment.color = NA) +
# coord_cartesian(xlim = c(NA, 2022)) 
# ggsave("Graphs/country_time_stacked.png", scale = 1)

tbl_coup_list <- dynamics_direct_networks(corpus = Corpus, 
                                          references = Refs, 
                                          source = "ID_Art", 
                                          target = "ItemID_Ref", 
                                          time_variable = Annee_Bibliographique,
                                          time_window = time_window, 
                                          weight_treshold_value = 1)

# Main components and com
tbl_coup_list <- lapply(tbl_coup_list, main_components)
tbl_coup_list <- lapply(tbl_coup_list, detect_leidenalg, niter = 10000)
#' We name communities:
tbl_coup_list <- intertemporal_naming_function(tbl_coup_list, treshold_similarity = 0.55)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Alluvial ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
alluv_dt <- make_into_alluv_dt(tbl_coup_list)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Positions ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
list_networks <- lapply(tbl_coup_list, function(tbl){tbl %>% activate(nodes) %>% mutate(Leiden1=new_Id_com)})
list_networks <- lapply(list_networks, function(tbl){tbl %>% activate(nodes) %>% mutate(nb_cit=size)})
#What will be computed as size
list_networks <- lapply(list_networks, function(tbl){tbl %>% activate(nodes) %>% mutate(size=nb_cit_wos)})

list_graph_position <- list(list_networks$"1975",list_networks$"1980",list_networks$"1985",list_networks$"1990",list_networks$"1995",list_networks$"2000",list_networks$"2005")

list_graph_position <- list()
for (Year in all_years){
  list_graph_position[[as.character(Year)]] <- list_networks[[paste0(Year)]]
}

list_graph_position <- lapply(list_graph_position, layout_fa2_java, niter=30000)

list_chosen_journals <- list()
for (Year in all_years){
  
  list_chosen_journals[[as.character(Year)]] <- list_networks[[paste0(Year)]] %>% activate(nodes) %>% as.data.table %>% .[,.N,.(Revue)] %>% .$Revue %>% as.data.table()
}

journal_list <- rbindlist(list_chosen_journals)
journal_list <- journal_list %>% rename(Revue = ".")



color = data.table(
  Revue_EER_bin = c("EER","Not EER","Other"),
  color = brewer.pal(n = 3, name = 'Set1'))

list_graph_position <- lapply(list_graph_position, function(tbl){tbl %>% activate(nodes) %>% mutate(Revue_EER_bin = ifelse(Code_Revue==5200,"EER","Not EER"))})
list_graph_position <- lapply(list_graph_position, color_tbl)
list_graph_position2 <- list_graph_position
list_ggplot2 <- list()

for (Year in all_years) {
  
  
  label_com <- list_graph_position2[[paste0(Year)]] %>% activate(nodes) %>% as.data.table()
  label_com <- label_com[,mean_coord_x:=mean(x), Revue_EER_bin]
  label_com <- label_com[,mean_coord_y:=mean(y), Revue_EER_bin]
  label_com <- label_com[color!="grey"][, head(.SD, 1), Revue_EER_bin]
  # label_com <- merge(label_com, label[,.(new_Id_com, Label_com)], all.x = TRUE)
  network_size <- label_com[,mean((mean_coord_x+mean_coord_y)/2)]
  
  # label_disc <- list_graph_position2[[paste0(Year)]] %>% activate(nodes) %>% as.data.table()
  # label_disc <- label_disc[,mean_coord_x:=mean(x), ESpecialite_custom]
  # label_disc <- label_disc[,mean_coord_y:=mean(y), ESpecialite_custom]
  # label_disc <- label_disc[color!="grey"][, head(.SD, 1), ESpecialite_custom]
  # label_disc <- merge(label_disc, label[,.(new_Id_com, Label_com)], all.x = TRUE)
  # network_size <- label_disc[,mean((mean_coord_x+mean_coord_y)/2)]
  
  list_ggplot2[[as.character(Year)]] <- ggraph(list_graph_position2[[paste0(Year)]], "manual", x = x, y = y) +
    geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.5, strength =0.2) +
    geom_node_point(aes(fill = color, size = size), pch=21) +
    scale_edge_width_continuous(range = c(0.5, 1)) +
    theme_void() +
    ggrepel::geom_label_repel(data = label_com, aes(x = mean_coord_x, y = mean_coord_y, label = as.character(Revue_EER_bin), fill = color)) +
    theme(legend.position = "none") +
    scale_fill_identity() +
    scale_edge_colour_identity() +
    labs(title = paste0(as.character(Year),"-",as.character(Year+time_window-1)))
  # ggsave("Networks/coup_2000.png", width=30, height=20, units = "cm")
  
}

g2 <- ggpubr::ggarrange(plotlist=list_ggplot2, common.legend = TRUE, legend="none")
