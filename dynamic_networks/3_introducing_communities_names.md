Script for introducing the manual names of the communities
================
Aurélien Goutsmedt and Alexandre Truc
/ Last compiled on 2021-03-11

  - [1 What is this script for?](#what-is-this-script-for)
  - [2 Loading packages paths and
    data](#loading-packages-paths-and-data)
  - [3 Renaming using the new community names (in
    construction)](#renaming-using-the-new-community-names-in-construction)

# 1 What is this script for?

This script aims at creating the networks for different time windows. We
want one-year moving time windows on the whole period (1969-2016) and we
need functions automating the creation of the 44 windows. This script
creates the networks, finds communities, integrates the name of
communities, calculates coordinates and saves the nodes and edges data
in a long format, used for producing the platform.

> WARNING: This script is not finished at all

# 2 Loading packages paths and data

``` r
source("~/macro_AA/functions/functions_for_network_analysis.R")
source("~/macro_AA/dynamic_networks/Script_paths_and_basic_objects.R")
```

# 3 Renaming using the new community names (in construction)

``` r
######################### Label **********************
alluv_dt<-alluv_dt[, c("Label"):=NULL]
label[new_Id_com=="c6Nqp2v2", Label:="Macroeconomics"]
label[new_Id_com=="EEEzS4Be", Label:="Energy"]
label[new_Id_com=="piHtlAjF", Label:="Finance: Bubbles"]
label[new_Id_com=="p45jKmQu", Label:="Finance"]
label[new_Id_com=="LxxETsng", Label:="Finance: Taxes"]
label[new_Id_com=="GrfdrlDw", Label:="Environment: Technologies"]
label[new_Id_com=="82I5q6j7", Label:="Environment: Agriculture"]
label[new_Id_com=="CtSjS1j0", Label:="Knowledge Economics"]
label[new_Id_com=="Goht2842", Label:="Urban Economics"]
label[new_Id_com=="gjLRNe3f", Label:="Market: Labor"]
label[new_Id_com=="5hejpPdE", Label:="Logistics"]
label[new_Id_com=="rh5rsLvO", Label:="Interactions"]
label[new_Id_com=="VcbF4o2X", Label:="Applied ABM and GT"]
label[new_Id_com=="2ouXdo0x", Label:="Supply Chain"]
label[,Label_com:=Label]
alluv_dt<-merge(alluv_dt,label[,.(new_Id_com,Window,Label)], by = c("new_Id_com","Window"), all.x = TRUE )
label[,Leiden1:=Label_com]
alluv_dt<-alluv_dt[, c("Leiden1"):=NULL]
alluv_dt <- merge(alluv_dt,label[,.(new_Id_com,Leiden1)], by = c("new_Id_com"), all.x = TRUE )
alluv_dt[is.na(Leiden1)==TRUE,Leiden1:=new_Id_com]


alluv_dt$new_Id_com <- fct_reorder(alluv_dt$new_Id_com, alluv_dt$n_years,min, .desc = FALSE)
alluv_dt$new_Id_com <- fct_reorder(alluv_dt$new_Id_com, alluv_dt$color2,min, .desc = TRUE)
ggplot(alluv_dt,
       aes(x = Window, y=share, stratum = new_Id_com, alluvium = Id,
           fill = color2, label = new_Id_com)) +
  scale_fill_identity("Disciplines", labels = alluv$new_Id_com.x, breaks = alluv$color, guide = "legend") +
  geom_flow(aes.bind = "flows") +
  geom_stratum(alpha =1, size=1/10,) +
  theme(legend.position = "none") +
  geom_label_repel(stat = "stratum", size = 5, aes(label = Label)) +
  # geom_label_repel(aes(label = Label)) +
  ggtitle("") +
  ggsave("Graphs/Intertemporal_communities.png", width=30, height=20, units = "cm")


alluv_dt$new_Id_com <- as.character(alluv_dt$new_Id_com)
ggplot(alluv_dt,
       aes(x = Window, y=share, stratum = Leiden1, alluvium = Id,
           fill = color2, label = Leiden1)) +
  scale_fill_identity("Disciplines", labels = alluv$new_Id_com.x, breaks = alluv$color, guide = "legend") +
  geom_flow(aes.bind = "flows") +
  geom_stratum(alpha =1, size=1/10,) +
  theme(legend.position = "none") +
  geom_label_repel(stat = "stratum", size = 5, aes(label = Label)) +
  # geom_label_repel(aes(label = Label)) +
  ggtitle("") +
  ggsave("Graphs/Intertemporal_communities2.png", width=30, height=20, units = "cm")
```
