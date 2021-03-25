#' ---
#' title: "Script for cleaning the missing articles and references extracting in Scopus"
#' author: "Julien Gradoz"
#' date: "/ Last compiled on `r format(Sys.Date())`"
#' output: 
#'   github_document:
#'     toc: true
#'     number_sections: true
#' ---
#' 
#' # What is this script for?
#' 
#' This script has been built by Julien Gradoz. The input is a .txt with articles metadata and their references
#' extracted from scopus. It aims at producing a data.frame that we will merge with WoS data.
#' 
#' 
#' > WARNING: This script still needs a lot of cleaning
#' 
#' 
#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)

#' # Loading packages, paths and data
#' 

library(readr)
library(stringr)
library(tidyverse)
library(biblionetwork)

eer_data <- "/projects/data/macro_AA/EER/Corpus_EER/"

# Import list of articles extracted from SCOPUS

zal <- read_delim(paste0(eer_data,"EER_scopus.txt"), "\t", escape_double = FALSE, trim_ws = TRUE)


#### V?rifier qu'on a bien le bon nombre de r?f?rences. Comment faire?
#### Les infos sur les references sont d?finies avec des balises, donc on identifie la balise
#### annon?ant le d?but de la bibliographie ("REFERENCES:"), de longueur 11 caract?res
#### Puis on comptera le nombe de balises identifi?es
#### Cette d?marche nous permettra aussi de d?buter le travail sur la bibliographie


zal[,2]<-3
for(i in 1:nrow(zal)){
  if(str_sub (zal[i,1], 1,11)=="REFERENCES:"){zal[i,2]<-T}else{zal[i,2]<-F}
}


########## Si on a 2346 valeurs de 1 c'est gagn? (ici 2299)

table(zal[,2],useNA="always")



#### Maintenant, on identifie les balises qui suivent g?n?ralement la bibliographie 
#### ("CORRESPONDENCE", "ISSN"...)
#### Du coup, on a identifi? la borne inf?rieure et sup?rieure de la bibliographie des articles

zal[,3]<-3
for(i in 1:nrow(zal)){
  if(str_sub (zal[i,1], 1,14)=="CORRESPONDENCE"|str_sub (zal[i,1], 1,5)=="ISSN:"|str_sub (zal[i,1], 1,10)=="PUBLISHER:"){zal[i,3]<-T}else{zal[i,3]<-F}
}



################################################################################
# On fait une boucle qui va permettre d'identifier les r?f?rences biblios de chaque article
# Pour ce faire, l'id?e de la boucle est la suivante: "d?s que tu d?tectes une borne inf?rieure,
# tu commences ? marquer les lignes jusqu'? ce que tu rencontres une borne sup?rieure, puis tu 
# recommences quand tu tombes sur la borne inf?rieure d'apr?s, etc...". 
# Du coup, ?a identifie les r?f?rences bibliographiques, et donne un identifiant unique
# aux articles citant (d'o? le fait que deux colonnes sont cr??es)
################################################################################


zal[,4]<-4
zal[,5]<-5
k<-0


for(i in 1:nrow(zal)){
  
  j<-0
  k<-k+1
  
  if(zal[i,2]==1){while(zal[i+j,3]==0){
    
    zal[i+j,4]<-1
    
    zal[i+j,5]<-k
    
    j<-j+1
    
  }
    
    
  }else{zal[i,4]<-zal[i,4]}
}



###################################################################################################
###################################################################################################
##### Constitution des infos sur les noeuds -----------------------------------------
###################################################################################################
###################################################################################################

##### Cr?ation d'une nouvelle base pour les noeuds

zaler<-zal


##### On enregistre les identifiants uniques des articles citant

ids<-unique(zaler$...5)
ids<-as.data.frame(ids)

##### On remplace le 5 (valeur par d?faut de la boucle pr?c?dente) par un 1

ids[1,]<-1


##### On identifie les lignes o? les biblios cessent (ce qui permettra de supprimer les refs biblios ensuite)

zaler$V1<-6
for(i in 1:(nrow(zaler)-1)){if(zaler[i,4]==1 & zaler[i+1,4]==4){zaler[i+1,6]<-T}else{zaler[i+1,6]<-F}}


ids<-ids[-1,]


##### On detecte la pr?sence de la balise SOURCE, qui signifie la fin des articles

zaler$Cace<- str_detect(string = zaler$V1, pattern = "SOURCE")


##### On extrait la position des lignes o? on passe ? l'article suivant (on d?finit ainsi un intervalle pour chaque
##### article, ce qui evitera de d?passer dans une des boucles ult?rieure)

#### R?le central de la fonction which

Cave2<-which(zaler[,7]==TRUE)
Cave2<-as.data.frame(Cave2)
Cave2<-rbind(1,Cave2)


##### Boucle qui permet d'associer l'identifiant de l'article citant ? toutes les lignes qui correspondent ? cet article
##### (avant elle n'?taient associ?es qu'aux r?f?rences biblios)

j<-0
zaler$Cartel<-16790
for(i in 1:(nrow(Cave2)-1)){
  if(is.element(1,zaler$...4[Cave2[i,]:Cave2[i+1,]])){
    j<-j+1
  zaler$Cartel[Cave2[i,]:Cave2[i+1,]]<-ids[j]
  } else {zaler$Cartel[Cave2[i,]:Cave2[i+1,]]<-18888}
  }


##### idsbis vise ? comparer si on a la m?me liste d'identifiant que pour ids
##### la seule diff?rence est 18888, qui correspond aux articles n'ayant pas de biblios

idsbis<-unique(zaler$Cartel)
idsbis<-as.data.frame(idsbis)
ids<-as.data.frame(ids)


##### On supprime les refs biblios de notre base

zaler<-zaler[zal[,4]!=1,]

##### On ne garde que les colonnes d'int?ret

zaler<-zaler[,c(1,8)]


##### On extrait l'info sur le premier auteur grace ? une REGULAR EXPRESSION (REGEX)

zaler$Firstauthor<-"AB"
zaler$Firstauthor<-gsub("([A-z0-9]\\.\\,.*|([A-z0-9]\\.[A-z0-9]?\\.{1}$| [A-z0-9]?\\.{1}$))", "",zaler$V1)

##### On cr??e une colonne qui dit si la nouvelle colonne cr??e contient la m?me info que la pr?c?dente
##### (ce qui veut implicitement dire que que ce n'est pas une ligne d'auteur)

zaler$comp<-zaler$Firstauthor==zaler$V1


##### A partir de cette info, on ne garde que les infos que les auteurs

for(i in 1:nrow(zaler)){if(zaler$comp[i]==T){zaler$Firstauthor[i]<-"AB"}else{zaler$Firstauthor[i]<-zaler$Firstauthor[i]}}


##### Malgr? la relative efficacit? de la REGEX, cela n'empeche pas quelques autres infos 
##### de s'?tre gliss?es dans la colonne premier auteur. Comme beaucoup de ses infos se 
##### remarquent avec une balise, alors on peut les effacer

zaler$comp<-str_detect (string =zaler$Firstauthor, pattern = "ABSTRACT:")


for(i in 1:nrow(zaler)){if(zaler$comp[i]==T){zaler$Firstauthor[i]<-"AB"}else{zaler$Firstauthor[i]<-zaler$Firstauthor[i]}}


zaler$comp<-str_detect (string =zaler$Firstauthor, pattern = "PUBLISHER:")

for(i in 1:nrow(zaler)){if(zaler$comp[i]==T){zaler$Firstauthor[i]<-"AB"}else{zaler$Firstauthor[i]<-zaler$Firstauthor[i]}}


zaler$comp<-str_detect (string =zaler$Firstauthor, pattern = "ABBREVIATED SOURCE TITLE:")

for(i in 1:nrow(zaler)){if(zaler$comp[i]==T){zaler$Firstauthor[i]<-"AB"}else{zaler$Firstauthor[i]<-zaler$Firstauthor[i]}}


zaler$comp<-str_detect (string =zaler$Firstauthor, pattern = "AFFILIATIONS:")

for(i in 1:nrow(zaler)){if(zaler$comp[i]==T){zaler$Firstauthor[i]<-"AB"}else{zaler$Firstauthor[i]<-zaler$Firstauthor[i]}}


zaler$comp<-str_detect (string =zaler$Firstauthor, pattern = "CORRESPONDENCE ADDRESS:")

for(i in 1:nrow(zaler)){if(zaler$comp[i]==T){zaler$Firstauthor[i]<-"AB"}else{zaler$Firstauthor[i]<-zaler$Firstauthor[i]}}

zaler$comp<-str_detect (string =zaler$Firstauthor, pattern = "FUNDING TEXT")

for(i in 1:nrow(zaler)){if(zaler$comp[i]==T){zaler$Firstauthor[i]<-"AB"}else{zaler$Firstauthor[i]<-zaler$Firstauthor[i]}}


##### Comme ?a ne suffit pas ? tout purger, on regarder aussi les chaines de 
##### caract?res trop longues pour ?tre des noms

zaler$char<-0
for(i in 1:nrow(zaler)){zaler$char[i]<-nchar(zaler$Firstauthor[i])}


##### Qualitativement, on voit que aucune info au dessus de 30 caract?res correspond ? un 
##### auteur, donc on purge

for(i in 1:nrow(zaler)){if(zaler$char[i]>=30){zaler$Firstauthor[i]<-"AB"}else{zaler$Firstauthor[i]<-zaler$Firstauthor[i]}}


##### On passe maintenant ? l'extraction des infos sur la publication, qui 
##### s'identifient par le fait qu'elles commencent par une ann?e entre parenth?ses
##### de plus, m?me technique de comparaison que pour les premier auteur plus haut


zaler$publi<-"AB"
zaler$publi<-gsub("^\\([[:digit:]][[:digit:]][[:digit:]][[:digit:]]\\)", "",zaler$V1)
zaler$comp<-zaler$publi==zaler$V1
for(i in 1:nrow(zaler)){if(zaler$comp[i]==T){zaler$publi[i]<-"AB"}else{zaler$publi[i]<-zaler$publi[i]}}



####### On extrait le titre, qui est toujours une ligne au-dessus des infos de publication

zaler$titre<-"AB"
for(i in 2:nrow(zaler)){if(zaler$comp[i]==F){zaler$titre[i]<-zaler$V1[i-1]}else{zaler$titre[i-1]<-zaler$titre[i-1]}}


###### On extrait aussi l'ann?e de publication


zaler$Datepubli<- str_extract_all(zaler$V1, "^\\([[:digit:]][[:digit:]][[:digit:]][[:digit:]]\\)")

zaler$Datepubli[zaler$Datepubli == "character(0)"] <- "AB"


####### On extrait la position des lignes o? l'on passe d'un article ? un autre
####### ce qui s'observe par le changement d'identifiant unique dans la colonne Cartel


idsbis<- which(zaler$Cartel != dplyr::lag(zaler$Cartel))

idsbis<-as.data.frame(idsbis)

idsbis<-rbind(1,idsbis)



##### Boucle ing?nieuse, qui fait un peu pareil que des boucles plus longues que l'on retrouvera 
##### ailleurs dans le script. L'id?e est la suivante: "entre le d?but de l'utilisation d'un nouvel identifiant
##### unique et la fin de son utilisation, capte la position du premier element qui n'est pas un 
##### AB dans la colonne du premier auteur (le [1] ? la fin permet de selectionner le premier element
##### ce qui est super car cela evacue les affiliations ou les abstract qui auraient pu se glisser),
##### capte aussi la position de la ligne dans laquelle toutes les autres infos ont ?t? regroup?es
##### identifi?es par la position de la date, puis met l'info sur les auteurs sur la m?me ligne que
##### les autres infos. 
##### Ce qui est pas mal avec cette boucle, c'est qu'on est sur que les infos d'un article ne vont 
##### pas se retrouver dans un autre, car on d?finit des bornes strictes.


for(i in 1:(nrow(idsbis)-1)){
  j<-which(zaler$Firstauthor[idsbis[i,1]:(idsbis[i+1,1])]!="AB")[1]
  k<-which(zaler$Datepubli[idsbis[i,1]:(idsbis[i+1,1]+1)]!="AB")[1]
  zaler$Firstauthor[(idsbis[i,1]+k-1)]<-zaler$Firstauthor[(idsbis[i,1]+j-1)]}



##### On ne garde que les lignes avec une date de publi

desco<-zaler[zaler$Datepubli!="AB",]


##### On regarde s'il y a des ?l?ments en double dans les identifiants uniques
##### (ce qui serait tragiques pour le package)

jki<-desco[duplicated(desco$Cartel)|duplicated(desco$Cartel, fromLast=TRUE),]

##### On remarque que les seuls doublons sont des 18888, c'est-?-dire les identifiants 
##### d'articles sans r?f?rences bibliographiques. On peut donc les supprimer

desco<-desco[desco$Cartel!=18888,]


##### On compare les identifiants uniques de notre base de noeuds avec notre base complete

carlito<-desco$Cartel
carlito<-as.data.frame(carlito)
comp<-cbind(carlito,ids)

########## Les deux colonnes sont identiques, sah quel plaisir!


desco<-desco[,c(2,3,6,7,8)]


##### Sauvegarde de la base ? ce stade-l?, ce qui ?vite de devoir refaire tourner les boucles


#save(desco, file = "3-TraitementScopus/noeuds123.rda")



##############################################################################
##############################################################################
# Maintenant on fait le traitement des r?f?rences bibliographiques --------------------
##############################################################################
##############################################################################

#### On ne conserve que les refs biblios, et l'identifiant de l'article original

zal<-zal[zal[,4]==1,]

zal<-zal[,c(1,5)]




#### La commande ci-dessous d?termine le nombre d'identifiant unique des articles originaux
#### Donc en gros, ?a permet de v?rifier de combien d'articles de la base originale ont ? pu 
#### extraire grace ? la biblio. L'id?al ?a serait d'avoir "zera" de longueur 2346 (ici 2299)

zera<-unique(zal[,2])


##############################################################################################
##############################################################################################
# Premier probl?me: certains articles sont coll?s sur la m?me ligne et s?par?s par un point-virgule
# Ceci-?tant, on ne peut pas se baser que sur la pr?sence du point-virgule, car cela casserait
# d'autres articles. 
# Sur regex il a donc fallu faire un truc un peu plus complexe. 
# La structure ci-desssous fonctionne de la fa?on suivante: "si tu d?tecte un point-virgule,
# puis du texte (nom auteur), puis une virgule, puis du texte (prenom) puis le combo point + virgule,
# alors tu met un True (c'est en fait que deux articles sont coll?s)
#
# NB: pour v?rifier la validit? de la syntaxe, on peut utiliser https://spannbaueradam.shinyapps.io/r_regex_tester/
##############################################################################################
##############################################################################################


zal$Cace<-str_detect (string =zal$V1, pattern = "(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")


### Lorsque c'est le cas, nouvelle colonne dans laquele on met le nouvel article

zal$troupinar<-"ABC"

for(i in 1:nrow(zal)){if(zal[i,3]==T){zal[i,4]<-str_extract(string = zal[i,1], pattern ="(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")
}else{zal[i,4]<-"AB"}}



###### On a donc un truc avec cette t?te |(article 1 + 1 bis) coll?s|ID1|article 1 bis|
######                                   |article 2                 |ID2|"AB"         |

###### Ce qu'on veut
######                                   |article 1                 |ID1|"AB"         |
######                                   |article 1bis              |ID1|"AB"         |
######                                   |article 2                 |ID2|"AB"         |

###### Du coup on utilise la fonction de replication lorsqu'on est dans la situation o? 
###### deux articles ?taient coll?s: d?s que c'est le cas repetition de la ligne 2 fois et 
###### sinon on garde l'exemplaire unique. 

zal <- zal[rep(row.names(zal), ifelse(zal$troupinar!="AB",2,1)),]

for(i in 1:(nrow(zal)-1)){if(zal[i,3]==T&zal[i+1,3]==T){
  zal[i+1,3]<-0
}else{zal[i+1,3]<-zal[i+1,3]}}



###### On supprime le "1 bis coll?s" de "article 1 + 1 bis coll?s" dans la premi?re colonne 

zal$V1<-gsub("(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)", "",zal$V1)

###### On transfere les infos dans les nouvelles lignes cr??es

for(i in 1:(nrow(zal)-1)){if(zal[i,3]==T&zal[i+1,3]==F){
  zal[i+1,1]<-zal[i+1,4]
}else{zal[i+1,1]<-zal[i+1,1]}}


###### On garde que les colonnes qui nous interesse

zal<-zal[,c(1,2)]

###### On nettoie les point-virgule


for(i in 1:nrow(zal)){
  if(str_sub (zal[i,1], 1,1)==";"){str_sub (zal[i,1], 1,1)<-""}else{zal[i,1]<-zal[i,1]}
}

#####################################################################################
#####################################################################################
# Comme certaines refs ?taient coll?es en bloc, on r?it?re la boucle pour les s?parer 
# au fur et ? mesure
#####################################################################################
#####################################################################################


zal$Cace<-str_detect (string =zal$V1, pattern = "(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")
zal$troupinar<-"ABC"

for(i in 1:nrow(zal)){if(zal[i,3]==T){zal[i,4]<-str_extract(string = zal[i,1], pattern ="(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")
}else{zal[i,4]<-"AB"}}

zal <- zal[rep(row.names(zal), ifelse(zal$troupinar!="AB",2,1)),]

for(i in 1:(nrow(zal)-1)){if(zal[i,3]==T&zal[i+1,3]==T){
  zal[i+1,3]<-0
}else{zal[i+1,3]<-zal[i+1,3]}}

zal$V1<-gsub("(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)", "",zal$V1)


for(i in 1:(nrow(zal)-1)){if(zal[i,3]==T&zal[i+1,3]==F){
  zal[i+1,1]<-zal[i+1,4]
}else{zal[i+1,1]<-zal[i+1,1]}}


zal<-zal[,c(1,2)]

for(i in 1:nrow(zal)){
  if(str_sub (zal[i,1], 1,1)==";"){str_sub (zal[i,1], 1,1)<-""}else{zal[i,1]<-zal[i,1]}
}

zal$Cace<-str_detect (string =zal$V1, pattern = "(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")
zal$troupinar<-"ABC"

for(i in 1:nrow(zal)){if(zal[i,3]==T){zal[i,4]<-str_extract(string = zal[i,1], pattern ="(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")
}else{zal[i,4]<-"AB"}}


zal <- zal[rep(row.names(zal), ifelse(zal$troupinar!="AB",2,1)),]

for(i in 1:(nrow(zal)-1)){if(zal[i,3]==T&zal[i+1,3]==T){
  zal[i+1,3]<-0
}else{zal[i+1,3]<-zal[i+1,3]}}

zal$V1<-gsub("(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)", "",zal$V1)


for(i in 1:(nrow(zal)-1)){if(zal[i,3]==T & zal[i+1,3]==F){
  zal[i+1,1]<-zal[i+1,4]
}else{zal[i+1,1]<-zal[i+1,1]}}


zal<-zal[,c(1,2)]

for(i in 1:nrow(zal)){
  if(str_sub (zal[i,1], 1,1)==";"){str_sub (zal[i,1], 1,1)<-""}else{zal[i,1]<-zal[i,1]}
}


zal$Cace<-str_detect (string =zal$V1, pattern = "(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")
zal$troupinar<-"ABC"

for(i in 1:nrow(zal)){if(zal[i,3]==T){zal[i,4]<-str_extract(string = zal[i,1], pattern ="(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")
}else{zal[i,4]<-"AB"}}


zal <- zal[rep(row.names(zal), ifelse(zal$troupinar!="AB",2,1)),]

for(i in 1:(nrow(zal)-1)){if(zal[i,3]==T&zal[i+1,3]==T){
  zal[i+1,3]<-0
}else{zal[i+1,3]<-zal[i+1,3]}}

zal$V1<-gsub("(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)", "",zal$V1)


for(i in 1:(nrow(zal)-1)){if(zal[i,3]==T & zal[i+1,3]==F){
  zal[i+1,1]<-zal[i+1,4]
}else{zal[i+1,1]<-zal[i+1,1]}}


zal<-zal[,c(1,2)]

for(i in 1:nrow(zal)){
  if(str_sub (zal[i,1], 1,1)==";"){str_sub (zal[i,1], 1,1)<-""}else{zal[i,1]<-zal[i,1]}
}

zal$Cace<-str_detect (string =zal$V1, pattern = "(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")
zal$troupinar<-"ABC"

for(i in 1:nrow(zal)){if(zal[i,3]==T){zal[i,4]<-str_extract(string = zal[i,1], pattern ="(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")
}else{zal[i,4]<-"AB"}}


zal <- zal[rep(row.names(zal), ifelse(zal$troupinar!="AB",2,1)),]

for(i in 1:(nrow(zal)-1)){if(zal[i,3]==T&zal[i+1,3]==T){
  zal[i+1,3]<-0
}else{zal[i+1,3]<-zal[i+1,3]}}

zal$V1<-gsub("(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)", "",zal$V1)


for(i in 1:(nrow(zal)-1)){if(zal[i,3]==T & zal[i+1,3]==F){
  zal[i+1,1]<-zal[i+1,4]
}else{zal[i+1,1]<-zal[i+1,1]}}


zal<-zal[,c(1,2)]

for(i in 1:nrow(zal)){
  if(str_sub (zal[i,1], 1,1)==";"){str_sub (zal[i,1], 1,1)<-""}else{zal[i,1]<-zal[i,1]}
}

zal$Cace<-str_detect (string =zal$V1, pattern = "(\\;)( [A-z0-9].*, [A-z0-9].*\\.\\,.*)")



####### Maintenant on cr?? eoeoe, sur laquelle on va travailler par la suite

eoeoe<-zal[,c(1,2,3)]

###### On change le nom d'une colonne

names(eoeoe)[names(eoeoe) == "...5"] <- "V2"


###### Pour d?terminer le premier auteur, on enl?ve tout ce qu'il y a apr?s la premiere ","

eoeoe<-eoeoe[,c(1,2)]
eoeoe$Firstauthor<-gsub("[A-z0-9]\\.\\,.*", "",eoeoe$V1)



##### On cr??e une colonne qui dit si la nouvelle colonne cr??e contient la m?me info que la pr?c?dente

eoeoe$comp<-eoeoe$Firstauthor==eoeoe$V1


###### On enl?ve aussi la balise de r?f?rence
eoeoe$Firstauthor<-str_replace(string = eoeoe$Firstauthor, pattern = "REFERENCES: ", replacement = "")


#### Certaines refs sont construites diff?rement du coup on adpate le REGEX sur les infos pas trait?es avec
#### le REGEX pr?c?dent.

eoeoe$stabilo<-"AB"
eoeoe$stabilo[eoeoe$comp==T]<-str_detect (string =eoeoe$V1[eoeoe$comp==T], pattern = "(?:^)(\\w+)\\,")


eoeoe$Firstauthor[eoeoe$stabilo==T]<- str_extract_all(eoeoe$V1[eoeoe$stabilo==T], "(?:^)(\\w+)\\,")



#############################################################################################
#############################################################################################
# Attention: Certaines refs construite diff?rement peuvent ?tre coll?es ? d'autres, du coup 
# j'en ai pas tenu compte
#############################################################################################
#############################################################################################



######################################################################################
# Enorme souci: je pensais pouvoir d?tecter les auteurs tranquilement en mode:
# Hendel, L., Lizzeri, A., Interfering with secondary markets (1999)
# on r?cup?re ce qu'il y a avant la premi?re virgule
# seulement, si les auteurs ont ?crit plusieurs articles et que les articles sont ?crits
# ensemble, alors on a quelque chose comme ?a: 
#Hendel, L., Lizzeri, A., Interfering with secondary markets (1999) Rand Journal of Economics, 30, pp. 1-21; 
#Adverse selection in durable goods markets (1999) American Economic Review, 89, pp. 1097-1115; 
#
# Du coup, n?cessit? de trouver une solution.
######################################################################################

######################################################################################
# Ce que j'ai fait: Les articles qui foirent ?chouent ?galement ? r?cup?rer le nom
# du premier auteur, et r?cup?rent en g?n?ral tout le titre. Du coup j'ai cr?? une colonne
# dans laquelle on compte le nombee de caract?res de l'identifiant du premier auteur. 
# Ensuite, on filtre les identifiants ? plus de 30 caract?res, et on classe la premi?re colonne
# par ordre alphab?tique, ce qui permet de voir les doublons. Des doublons indiquent qu'on perd au
# moins deux fois les m?mes infos, ce qui est dommage. Du coup pour ces r?patitions on met l'info 
# manuellement. 
# 
######################################################################################


##### On compte les caract?res de Firstauthor

eoeoe$crapaud<-0
for(i in 1:nrow(eoeoe)){eoeoe$crapaud[i]<-nchar(eoeoe$Firstauthor[i])}

##### Si 0 ou 1 caract?res ne sert ? rien

eoeoe<-eoeoe[eoeoe$crapaud!=1 & eoeoe$crapaud!=0,]


###### Si dessous modification manuelle de certaines infos apr?s un examen personnel

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Adverse selection in durable goods markets")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Hendel"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Adverse selection with competitive inspection")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Biglaiser"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Advertising and pricing to deter or accommodate entry when demand is unknown")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Bagwell"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Advertising as information")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Nelson"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "An economic theory of planned obsolescence")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Bulow"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Competition in markets for credence goods")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Wolinsky"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Competition in Markets for Credence Goods")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Wolinsky"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Credence goods monopolists")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Emons" 

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Eliminating the market for secondhand goods: An alternative explanation for leasing")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Waldman"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Entry \\(and exit\\) in a differentiated industry")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Gabszewicz"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Information and consumer behavior \\(1970\\)")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Nelson"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Information transmission in regulated markets \\(1993\\) Canadian Journal of Economics")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Pitchik"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Information Transmission in Regulated Markets \\(1993\\) Canadian Journal of Economics")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Pitchik"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Interfering with secondary markets \\(1999\\) Rand Journal of Economics")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Hendel"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Interfering with secondary markets \\(1999\\) RAND Journal of Economics")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Hendel"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Interfering with secondary markets \\(1999\\) RAND Journal of Economics")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Hendel"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Natural oligopolies \\(1983\\) Econometrica")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Shaked"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Premium for high quality products as returns to reputations \\(1983\\) Quarterly Journal of Economics")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Shapiro"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Premiums for High Quality Products as Returns to Reputations \\(1983\\) Quarterly Journal of Economics")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Shapiro"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Price and advertising as signals of product quality \\(1986\\)")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Milgrom"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Price and advertising signals by product quality \\(1986\\) Journal of Political Economy")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Milgrom"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Price and advertising signals of product quality \\(1986\\) Journal of Political Economy")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Milgrom"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Product introduction with network externalities \\(1992\\) J. Indust. Econom.")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Katz"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Product introduction with network externalities \\(1992\\) Journal of Industrial Economics")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Katz"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "SERVQUAL: A Multiple-Item Scale for Measuring Consumer Perceptions of Service Quality \\(1988\\) Journal of Retailing")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Parasuraman"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The nature of equilibrium in markets with adverse selection")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Wilson"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The Nature of Equilibrium in Markets with Adverse Selection")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Wilson"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Relaxing price competition through product differentiation")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Shaked"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The role of leasing under adverse selection \\(2002\\) Journal of Political Economy")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Hendel"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The role of leasing under adverse selection \\(2002\\) Journal of Political Economy")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Hendel"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "\\(1974\\) Market Signaling")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Spence"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "theory of predation")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Fudenberg"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "\\(1979\\) Variety, Equity and Efficiency")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Lancaster"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "\\(1976\\) The Economic Approach to Human Behavior")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Becker"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "\\(1975\\) Human Capital: A Theoretical and Empirical Analysis")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Becker"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "A dynamic model of the duration of the customer")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Bolton"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Accuracy in the assessment of damages \\(1996\\)")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Kaplow"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Acquisition and Disclosure of Information Prior to Sale \\(1994\\)")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Shavell"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Advertising Effects on Prices Paid and Liking for Brands Selected \\(1996\\)")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Mitra"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Advertising repetition as a signal of quality")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Kirmani"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Issues for Consumer Researchers")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Anderson"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Quality assurance systems in Israeli industries")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Barad"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = " Optimal product quality and advertising")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Tapiero"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Disclosing horizontal match information")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Anderson"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Competing technologies, increasing returns")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Arthur"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Competing compatibility standards and network externalities in the PC software market")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Gandal"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Competition in salaries, credentials, and signaling prerequisites for jobs")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Spence"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Competitive and optimal responses to signals")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Spence"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Consumer misperceptions, product failure and producer liability")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Spence"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "improving research and practice through theory development")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Dean"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Diffusion with bilateral learning")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Vettas"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Differentiated products, consumer search, and locational oligopoly")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Stahl"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Disappointment in decision making under uncertainty")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Bell"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Durable goods pricing when quality matters")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Waldman"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Durable goods theory for real world markets")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Waldman"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Dynamic auctions \\(1990\\)")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Vincent"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Dynamic nonlinear pricing in markets with interdependent demand")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Oren"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Equilibrium effects of potential entry when prices signal quality")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Overgaard"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Equilibrium in competitive insurance markets")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Rothschild"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Equilibrium Incentives in Oligopoly")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Fershtman"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Products liability, punitive damages and competition")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Doughety"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Exploratory evidence on the behavior of quality costs")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Ittner"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Quality improvement and learning in productive systems")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Fine"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Patent Statistics as Economic Indicators")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Griliches"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "A National Customer Satisfaction Barometer")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Fornell"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "If Homo Economicus Could Choose His Own")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Frank"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Quality and quantity competition")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Gal-Or"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "First Mover Disadvantages with Private Information")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Gal-Or"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Cournot and Bertrand Equilibrium")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Gal-Or"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Warranties as a Signal of Quality")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Gal-Or"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Gift exchange and efficiency-wage theory")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Akerlof"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The true motives behind ISO")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Gotzamani"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "principle of efficient choice")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Hjorth-Andersen"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "More on multidimensional quality")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Hjorth-Andersen"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Price and quality of industrial product")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Hjorth-Andersen"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = " Price as a Risk Indicato")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Hjorth-Andersen"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The concept of quality and the efficiency of markets for consumer products")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Hjorth-Andersen"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "But I play one on TV")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Hertzendorf"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Ignorance is bliss as trade policy")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Creane"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Incentives and risk sharing in sharecropping")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Stiglitz"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Information alliances")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Baron"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Information in the labor market")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Stigler"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Information Revelation and Certification Intermediaries")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Lizzeri"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Informational Product Differentiation as a Barrier to Entry")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Bagwell"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "International rivalry in advancing products")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Glass"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Investment in human capital")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Becker"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Investor Sophistication and Voluntary Disclosures")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Dye"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Heuristics and biases")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Kahneman"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Labor contracts as partial gift exchange")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Akerlof"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Monopoly with Asymmetric Information about Quality")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Laffont"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "League composition effect in tournaments with heterogeneous players")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Levy"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Learning from others")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Vives"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Liability and the incentives to obtain information about risk")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Shavell"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Limit qualities and entry deterrence")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Donnenfeld"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Long term vertical relationships and the study of industrial organization and government regulation")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Joskow"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Market diffusion with two-sided learning")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Bergemann"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Measurement and Evaluation of Satisfaction Process in Retail Settings")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Oliver"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Measurement Cost and the Organization of Markets")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Barzel"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Adaptive and Sophisticated Learning in Normal Form Games")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Milgrom"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Mitigating the effect of service encounters")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Bolton"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Model for customer complaint management")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Fornell"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Modifying customer expectations of price decreases for a durable product")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Balachander"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Monopoly provision of product quality with un-informed buyers")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Cooper"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Moral hazard in teams")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Holmstrom"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Models for measuring and accounting for costs of conformance quality")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Nandakumar"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Occupational segregation, wages and profits when employers discriminate by sex")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Bergmann"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "On Sympathy and Games")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Sally"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Path dependence, lock-in, and history")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Liebowitz"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Planned obsolescence and the")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Waldman"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Prices as signals of product quality")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Wolinsky"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Pricing with endogenous direct advertising in a monopoly")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Esteban"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "A Theory of the Consumer Product Warranty")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Priest"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Product introduction with network externalities")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Katz"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Inadequacy of information")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Keeton"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Proprietary and Nonproprietary Disclosures")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Dye"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "An analysis of decision under risk")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Kahneman"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Rational expectations and durable goods pricing")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Stokey"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Reassessment of Expectations as a Comparison Standard in Measuring Service Quality")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Parasuraman"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Dynamic pricing of new experience goods")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Bergemann"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Market Structure and the Durability of Goods")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Abel"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "On Being Honest and Behaving Honestly")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Adler"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Stardom and talent")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Adler"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Qualitative uncertainty and the market mechanism")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Akerlof"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Quality uncertainty and the market mechanism")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Akerlof"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Quality Uncertainty and the Market Mechanism")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Akerlof"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Quality uncertainty and the market mechanism")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Akerlof"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Quality uncertainty and the market mechanism")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Akerlof"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Reputation and product quality")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Allen"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Reputation and product quality")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Allen"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Product Quality and Price Regulation")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Anderson"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Product Quality and Price Regulation")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Anderson"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Behavior of the firm under regulatory constraint")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Averch"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Introductory price as a signal of cost in a model of repeat business")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Bagwell"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Optimal Export Policy for a New-Product Monopol")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Bagwell"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Monopoly and quality distortion")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Besanko"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "trust and the quality of physician")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"McGuire"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Tournaments versus fixed performance standards")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Tsoulouhas"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Making service quality financially accountable")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Rust"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Revisiting the lemons market")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Kessler"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Framework for a global quality evaluation of a website")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Rocha"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The economics of superstars")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Rosen"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "When is inducing self-selection suboptimal for a monopolist")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Salant"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Incentives in Principal-Agent Relationships")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Sappington"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Product differentiation advantages of pioneering brands")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Schmalensee"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Proposals for Products Liability Reform")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Schwartz"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The Case Against Strict Liability")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Schwartz"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Imperfect Information in the Market for Contract Terms")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Schwartz"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Searching for the lowest price when the distribution of prices is unknown")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Rothschild"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "An Empirical Study Of The Indian Banking Sector")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Choudhury"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "isk Sharing and Incentives in the Principal and Agent Relationship")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Shavell"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Consumer Misperceptions, Product Failure, and Producer Liability")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Spence"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = " New Evidence on Price and Product Quality")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Sproles"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Oligopolistic pricing with sequential consumer search")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Stahl"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Standardization and variety")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Farrell"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Systems competition and network effects")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Katz"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Technology adoption in the presence of network externalities")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Katz"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Test of the lemons model")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Bond"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The computation of opportunity costs in polychotomous choice models with selectivity")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Lee"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The economics of caste and of the rat race and other woeful tales")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Akerlof"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The economics of rumours")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Banerjee"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The effect of credit market concentration on firm creditor relationships")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Petersen"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The effect of price, brand name, and store name on buyers")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Rao"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The effect of the timing of consumption decisions and the resolution of uncertainty on the choice of lotterie")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Spence"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The Effectiveness of Quality Assurance")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Donabedian"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The Impact of Product Recalls on the Wealth of Sellers")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Jarrell"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The Mandatory Disclosure of Trades and Market Liquidity")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Fishman"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The optimal structure of incentives with authority within an organization")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Mirrlees"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The problem of social cost")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Coase"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = ", education, and the distribution of income")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Stiglitz"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The value of ignorance")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Kessler"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The Value of Information in a Sealed-Bid Auction")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Milgrom"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "The vertical integration of production")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Williamson"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Time and communication in economic and social interaction")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Spence"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Time and communication in economics and social interaction")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Spence"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Umbrella branding as a signal of new product quality")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Wernerfelt"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Up-or-out contracts")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Waldman"

eoeoe$Firstauthor6<- str_detect (string =eoeoe$Firstauthor, pattern = "Deregulation and Oligopolistic Price-Quality Rivalry")
eoeoe$Firstauthor[eoeoe$Firstauthor6==T]<-"Vander Weide"



##### Detecte toutes les refs qui commencent par une ann?e, qui correspond aux articles de 
##### journaux et aux rapports. 

eoeoe$annee<-4

for(i in 1:nrow(eoeoe)){if(grepl("^\\([[:digit:]][[:digit:]][[:digit:]][[:digit:]]\\)",str_sub (eoeoe$Firstauthor[i], 1,7))==T){eoeoe$annee[i]<-1}else{eoeoe$annee[i]<-0}}

##### On les supprime car ils ne pourront pas ?tre rematch?s sur SCOPUS

eoeoe<-eoeoe[eoeoe$annee==0,]

eoeoe$Firstauthor<-as.character(eoeoe$Firstauthor)


##### Maintenant, l'id?e est de cr?er un identifiant unique pour les refs biblios, 
##### bas? sur le premier auteur, sur la date de publication et sur la premi?re page de la publi.
##### Cette d?marche permettra aussi de pr?parer les requetes pour construire le "coeur".


##### Les auteurs avec des dates sont des erreur, on supprime. Pareil s'ils contiennent des parenth?ses

eoeoe$Firstauthor2<- str_detect (string =eoeoe$Firstauthor, pattern = "20")
eoeoe<-eoeoe[eoeoe$Firstauthor2==F,]
eoeoe$Firstauthor2<- str_detect (string =eoeoe$Firstauthor, pattern = "19")
eoeoe<-eoeoe[eoeoe$Firstauthor2==F,]
eoeoe$Firstauthor2<- str_detect (string =eoeoe$Firstauthor, pattern = "\\(")
eoeoe<-eoeoe[eoeoe$Firstauthor2==F,]
eoeoe$Firstauthor2<- str_detect (string =eoeoe$Firstauthor, pattern = "\\)")
eoeoe<-eoeoe[eoeoe$Firstauthor2==F,]

###### On nettoie les infos sur les premiers auteurs

eoeoe$Firstauthor<-gsub("\\,.*", "",eoeoe$Firstauthor)


##### On extrait les infos sur les volumes de publication et on nettoie

eoeoe$Issue<- str_extract_all(eoeoe$V1, "(, \\d{1,9})(,| \\(.*\\),)")
eoeoe$Issue[eoeoe$Issue == "character(0)"] <- "AB"
eoeoe$Issue<-as.character(eoeoe$Issue)

##### Si des ann?es se glissent, erreur, donc on enl?ve

eoeoe$Issue<-str_replace_all(string = eoeoe$Issue, pattern = "(19|20)\\d{2}", replacement = "")
eoeoe$Issue<-str_replace_all(string = eoeoe$Issue, pattern = "c\\(", replacement = "")
eoeoe$Issue<-str_replace_all(string = eoeoe$Issue, pattern = "\\(.*\\)", replacement = "")
eoeoe$Issue<-str_replace_all(string = eoeoe$Issue, pattern = "\\,", replacement = "")

###### On extrait l'ann?e en d?tectant les parenth?ses qui contiennent quatre chiffres dedans


eoeoe$Datepubli<- str_extract_all(eoeoe$V1, "\\((17|18|19|20)\\d{2}\\)")
eoeoe$Datepubli<-as.character(eoeoe$Datepubli)

###### Si plusieurs ann?es extraites, on ne garde que si les deux ann?es sont identiques. 
###### Du coup on splite dans plusieurs colonnes et on compare les diff?rentes ann?es

eoeoe$Datepubli2<-gsub("\\,.*", "",eoeoe$Datepubli)
eoeoe$Datepubli3<- str_extract_all(eoeoe$Datepubli, '\\,.*')
eoeoe$Datepubli2[eoeoe$Datepubli2 == "character(0)"] <- "AB"
eoeoe$Datepubli3[eoeoe$Datepubli3 == "character(0)"] <- "AB"

eoeoe$Datepubli2<-str_replace_all(string = eoeoe$Datepubli2, pattern = "\\(", replacement = "")
eoeoe$Datepubli2<-str_replace_all(string = eoeoe$Datepubli2, pattern = "\\)", replacement = "")
eoeoe$Datepubli2<-str_replace_all(string = eoeoe$Datepubli2, pattern = "c", replacement = "")
eoeoe$Datepubli2<-str_replace_all(string = eoeoe$Datepubli2, pattern = '\\"', replacement = "")

eoeoe$Datepubli3<-str_replace_all(string = eoeoe$Datepubli3, pattern = "\\(", replacement = "")
eoeoe$Datepubli3<-str_replace_all(string = eoeoe$Datepubli3, pattern = "\\)", replacement = "")
eoeoe$Datepubli3<-str_replace_all(string = eoeoe$Datepubli3, pattern = "c", replacement = "")
eoeoe$Datepubli3<-str_replace_all(string = eoeoe$Datepubli3, pattern = '\\"', replacement = "")
eoeoe$Datepubli3<-str_replace_all(string = eoeoe$Datepubli3, pattern = ',', replacement = "")


eoeoe$Datepubli3<-trimws(eoeoe$Datepubli3)
eoeoe$Datepubli2<-trimws(eoeoe$Datepubli2)

#### Si ce sont les m?mes ann?es, alors on remplace les ann?es multiples par une unique ann?e

for(i in 1:nrow(eoeoe)){
  if (eoeoe$Datepubli3[i]==eoeoe$Datepubli2[i] & eoeoe$Datepubli3[i]!="AB" & eoeoe$Datepubli2[i]!= "AB")
  {eoeoe$Datepubli[i]<-eoeoe$Datepubli3[i]}
  else{eoeoe$Datepubli[i]<-eoeoe$Datepubli[i]}}


#################################################################################################
# Je supprime les observations avec des dates mutliples restantes (pas optimal, sinon 138 ?
# modifier ? la main)
#################################################################################################

eoeoe$Datepubli<-gsub("c\\(.*", "AB",eoeoe$Datepubli)


###### On d?tecte les pages, soit au format "pp." ou au format "p.", et on garde ce qu'il y a apres.
###### "\\d" correspond ? des informations num?riques
###### Les doubles virgules, c'est pour les bouquins ou les chapitres de bouquins

eoeoe$Page<- str_extract_all(eoeoe$V1, " pp.+\\d")
eoeoe$Page2<- str_extract_all(eoeoe$V1, ", p\\..*")
eoeoe$Page3<-str_detect (string = eoeoe$V1, pattern = ", , ")


#### L'existence de cha?ne de caract?res nulles pose des pbs, du coup on les remplace
eoeoe$Page<-as.character(eoeoe$Page)
eoeoe$Page[eoeoe$Page == "character(0)"] <- "AB"
eoeoe$Page2[eoeoe$Page2 == "character(0)"] <- "AB"
eoeoe$Datepubli[eoeoe$Datepubli == "character(0)"] <- "AB"



###### On fusionne les deux formats de pages (pp. et p.) stock?es dans deux colonnes
###### au sein d'une m?me colonne


for(i in 1:nrow(eoeoe)){
  if(str_sub (eoeoe$Page2[i], 1,4)==", p."){eoeoe$Page[i]<-eoeoe$Page2[i]}else{eoeoe$Page[i]<-eoeoe$Page[i]}
}


##### On ne garde que l'information sur la premi?re page en supprimant ce qu'il y a apr?s

eoeoe$Page<-gsub("-.*", "",eoeoe$Page)
eoeoe$Page<-gsub(".,.*", "",eoeoe$Page)


##### On supprime aussi ce qu'il y a avant

eoeoe$Page<-str_replace(string = eoeoe$Page, pattern = "pp. ", replacement = "")
eoeoe$Page<-str_replace(string = eoeoe$Page, pattern = ", p. ", replacement = "")



#### trimws supprime les espace au d?but et ? la fin des chaines de caract?res
eoeoe$Firstauthor<-trimws(eoeoe$Firstauthor)
eoeoe$Datepubli<-trimws(eoeoe$Datepubli)
eoeoe$Issue<-trimws(eoeoe$Issue)
eoeoe$Page<-trimws(eoeoe$Page)



##### On garde uniquement les colonnes d'int?r?t

eoeoe<-eoeoe[,c(2,3,10,11,14)]


eoeoe$Page<-str_replace(string = eoeoe$Page, pattern = ";", replacement = "")


eoeoe$Firstauthor<-trimws(eoeoe$Firstauthor)
eoeoe$Datepubli<-trimws(eoeoe$Datepubli)
eoeoe$Issue<-trimws(eoeoe$Issue)
eoeoe$Page<-trimws(eoeoe$Page)



##### On cr?er une colonne qui fusionne les diff?rentes informations, permettant de cr?er 
##### un identifiant unique pour les refs biblio

eoeoe$identifiant<-"AB"
for(i in 1:nrow(eoeoe)){
  eoeoe$identifiant[i]<-paste(eoeoe$Firstauthor[i],eoeoe$Datepubli[i],eoeoe$Issue[i],eoeoe$Page[i])}


##### On nettoie les points virgules restants

eoeoe$identifiant<-str_replace(string = eoeoe$identifiant, pattern = ";", replacement = "")

###### On classe alphab?tiquement par rapport aux identifiants

eoeoe2<-eoeoe[order(eoeoe$identifiant),]

eoeoe2<-eoeoe2[eoeoe2$Datepubli!="AB",]


##### On regarde quelles sont les r?f?rences les plus cit?es en stockant la distribution


freq<-table(eoeoe2$identifiant)
freq<-as.data.frame(freq)

##### On ne garde que les refs cit?es au moins 3 fois, ce qui pr?pare le travail sur le coeur

freq<-freq[freq[,2]>=3,]


##### Si l'identifiant de la r?f?rence est dans le tableau des r?f?rences cit?es au moins 3 fois, 
##### Alors on la note dans la base de donn?es globale (fonction %in%)

eoeoe2$yolo<-"Yolo"
for(i in 1:nrow(eoeoe2)){
  if(eoeoe2$identifiant[i] %in% freq$Var1){eoeoe2$yolo[i]<-T}else{eoeoe2$yolo[i]<-F}
}

##### On ne garde que les refs cit?e au moins trois fois dans la base globale
##### Attention: laiss?es avec leurs r?p?titions

eoeoe2<-eoeoe2[eoeoe2$yolo==T,]


##### On place les balises pour faire les requ?tes du "coeur" du SCOPUS
##### M?me logique que dans la partie 1, donc s'y r?f?rer pour comprendre les commandes

eoeoe2$Firstauthor<-paste("AUTH(",eoeoe2$Firstauthor,")") 
eoeoe2$Datepubli<-str_replace(string = eoeoe2$Datepubli, pattern = "\\(", replacement = "")
eoeoe2$Datepubli<-str_replace(string = eoeoe2$Datepubli, pattern = "\\)", replacement = "")
#eoeoe[eoeoe$V2==2,1]<-paste("SRCTITLE(",eoeoe[eoeoe$V2==2,1],")") 
eoeoe2$Datepubli<-paste("PUBYEAR =",eoeoe2$Datepubli) 
eoeoe2$Issue<-paste("VOLUME(",eoeoe2$Issue,")") 
eoeoe2$Page<-paste("PAGEFIRST(",eoeoe2$Page,")")

eoeoe2$requete<-paste("(",eoeoe2$Firstauthor,"AND",eoeoe2$Datepubli,"AND",eoeoe2$Page,"AND",eoeoe2$Issue,")")


###### Pour ?viter les doublons, on garde les chaines de caract?res uniques

Rideo<-unique(eoeoe2$requete)
Rideo<-as.data.frame(Rideo)

#### Boucle pour cr?er la chaine de caract?res

Rideo[,2]<-Rideo[,1]
for(i in 1:(nrow(Rideo)-1)){
  Rideo[i+1,2]<- paste(Rideo[i,2],"OR", Rideo[i+1,2])
}

mot<-Rideo[nrow(Rideo),2]


#Enregistrer la requ?te sur mon PC



write.table(mot,file="Corerequest.txt", sep = ";", row.names=FALSE) 




##########################################################################################
##########################################################################################
# M?me commandes mais avec 4 occurences
##########################################################################################
##########################################################################################


#eoeoe22<-eoeoe2[order(eoeoe2$identifiant),]



#eoeoe22$kalro<-0
#for(i in 2:(nrow(eoeoe22)-1)){if((eoeoe22$Firstauthor[i]==eoeoe22$Firstauthor[i+1] & eoeoe22$Datepubli[i]==eoeoe22$Datepubli & ((eoeoe22$Issue[i]==eoeoe22$Issue[i+1] & eoeoe22$Page[i]!=eoeoe22$Page[i+1])|(eoeoe22$Issue[i]==eoeoe22$Issue[i+1] & eoeoe22$Page[i]!=eoeoe22$Page[i+1])))|(eoeoe22$Firstauthor[i]==eoeoe22$Firstauthor[i+1] & eoeoe22$Datepubli[i]==eoeoe22$Datepubli & ((eoeoe22$Issue[i]==eoeoe22$Issue[i+1] & eoeoe22$Page[i]!=eoeoe22$Page[i+1])|(eoeoe22$Issue[i]==eoeoe22$Issue[i+1] & eoeoe22$Page[i]!=eoeoe22$Page[i+1])))){eoeoe22$kalro[i]<-1}else{eoeoe22$kalro[i]<-0}}


#eoeoe22<-eoeoe22[eoeoe22$Datepubli!="AB",]



#freq22<-table(eoeoe22$identifiant)
#freq22<-as.data.frame(freq22)

##### On ne garde que les refs cit?es au moins 5 fois, ce qui pr?pare le travail sur le coeur

#freq22<-freq22[freq22[,2]>=4,]



##### Si l'identifiant de la r?f?rence est dans le tableau des r?f?rences cit?es au moins 5 fois, 
##### Alors on la note dans la base de donn?es globale (fonction %in%)

#eoeoe22$yolo<-"Yolo"
#for(i in 1:nrow(eoeoe22)){
#  if(eoeoe22$identifiant[i] %in% freq22$Var1){eoeoe22$yolo[i]<-T}else{eoeoe22$yolo[i]<-F}
#}

##### On ne garde que les refs cit?e au moins cinq fois dans la base globale
##### Attention: laiss?es avec leurs r?p?titions

#eoeoe22<-eoeoe22[eoeoe22$yolo==T,]






##### On place les balises pour faire les requ?tes du "coeur" du SCOPUS
##### M?me logique que dans la partie 1, donc s'y r?f?rer pour comprendre les commandes

#eoeoe22$Firstauthor<-paste("AUTH(",eoeoe22$Firstauthor,")") 
#eoeoe22$Datepubli<-str_replace(string = eoeoe22$Datepubli, pattern = "\\(", replacement = "")
#eoeoe22$Datepubli<-str_replace(string = eoeoe22$Datepubli, pattern = "\\)", replacement = "")
#eoeoe22$Datepubli<-paste("PUBYEAR =",eoeoe22$Datepubli) 
#eoeoe22$Issue<-paste("VOLUME(",eoeoe22$Issue,")") 
#eoeoe22$Page<-paste("PAGEFIRST(",eoeoe22$Page,")")

#eoeoe22$requete<-paste("(",eoeoe22$Firstauthor,"AND",eoeoe22$Datepubli,"AND",eoeoe22$Page,"AND",eoeoe22$Issue,")")


###### Pour ?viter les doublons, on garde les chaines de caract?res uniques
################################################################################################
# L'utilisation de "unique" m'inqui?te ici, il faudrait v?rifier que ?a ne pose pas de pbs,
# dans le sens o? l'on perdrait des possibilit?s de matching
################################################################################################

#Rideo22<-unique(eoeoe22$requete)
#Rideo22<-as.data.frame(Rideo22)




#### Boucle pour cr?er la chaine de caract?res

#Rideo22[,2]<-Rideo22[,1]
#for(i in 1:(nrow(Rideo22)-1)){
#  Rideo22[i+1,2]<- paste(Rideo22[i,2],"OR", Rideo22[i+1,2])
#}

#mot<-Rideo22[nrow(Rideo22),2]


#Enregistrer la requ?te sur mon PC


#setwd("C:/Users/jgrad/Desktop/R?seau de citations bibliographiques")
#write.table(mot,file="3-TraitementScopus/Coeurarticles222.txt", sep = ";", row.names=FALSE) 





