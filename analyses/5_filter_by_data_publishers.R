#' @title
#' Filter by data publishers
#' @description 
#' We screened all data publishers of invasive data - and selected only research works or observations by confirmed naturalists
#' 
#' @author Cybill Staentzel \cybill.staentzel@engees.unistra.fr
#' 
#' @date 2023/01/24

#Filter by data publishers for both fish and macroinvertebrates----
prov <- read.csv("Supplemental File3_FISH, sep=";") 
prov <- read.csv("Supplemental File3_INVERTEBRATES.csv", sep=";") 

prov$confidence_level<-as.factor(prov$confidence_level)

prov1_2 <- prov[prov$confidence_level %in% c("1", "2"), ]
length(prov1_2$datasetkey)

tot_xeno1_2<- tot_xeno[tot_xeno$"datasetKey" %in% unique(prov1_2$"datasetkey"), ]
length(unique(tot_xeno1_2$datasetKey))
