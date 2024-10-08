#' @title
#' Keep invasive species data only
#' @description 
#' Keep invasive species data only (fish and macroinvertebrates)
#' 
#' @author Cybill Staentzel \cybill.staentzel@engees.unistra.fr
#' 
#' @date 2023/01/24
#' 

## Import filtered GBIF data ---
dat    <- read.csv("to define by user.csv")

## Import filtered GBIF spatial data ---
dat_sf <- sf::st_read("to define by user.gpkg")

#Select only SpeciesxCountry keys from predefined lists
##Invertebrates
Supplemental File1_Invertebrates <- read.csv("...Supplemental File1_Invertebrates", sep=";")
##Fish
Supplemental File1_Fish <- read.csv("...Supplemental File1_Fish", sep=";")

#An example with one taxonomic group for macroinvertebrates : Bivalvia
bivalvia_keys_final<- bivalvia_TRUE[bivalvia_TRUE$"concat" %in% unique(Supplemental File1_Invertebrates$"Keys_CS"), ]
names(bivalvia_keys_final)<-c("download_key","datasetKey","occurrenceID","year", "eventDate", "class", "order", "family", "genus", 
                              "species", "period", "sovereignt", "concat","geom")
View(bivalvia_keys_final)
sf::st_write(bivalvia_keys_final, "gbif_bivalvia_data_final.gpkg",delete_dsn = TRUE) 

#Macroinvertebrates - All were merged
tot_xeno<-rbind(clitellata_keys_final, polychaeta_keys_final,culicidae_keys_final,brachyceridae_keys_final,Coenagrionidae_keys_final,
                amphipoda_keys_final,decapoda_keys_final,isopoda_keys_final,mysidae_keys_final,Pectinatellidae_keys_final,Cordylophoridae_keys_final,
                on_keys_final,bivalvia_keys_final,gastropoda_keys_final,tricladida_keys_final)

#Here the final list for macroinvertebrates! -----
sf::st_write(tot_xeno, "gbif_data_final.gpkg",delete_dsn = TRUE)
tot_xeno <- sf::st_read(dsn = "gbif_data_final.gpkg", stringsAsFactors = FALSE) 

#for futher analyses, add a column = 1 -----
tot_xeno$PA<-c("1")
tot_xeno$PA<-as.numeric(tot_xeno$PA)
str(tot_xeno)
