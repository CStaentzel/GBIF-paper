#' @title
#' Keep invasive species data only
#' @description 
#' Keep exotic species data only
#' 
#' @author Cybill Staentzel \cybill.staentzel@engees.unistra.fr
#' 
#' @date 2023/01/24
#' 

#Join observations by catchment (level 6 to 12)----
library(sf)
library(s2)
library(dplyr)
library(tidyr)

##############################
#An example with level 12 (done from level 12 to 6) ----------
level12 <- 
  read_sf("hybas_eu_lev12_v1c.shp") %>% 
  select(polygon_id = HYBAS_ID)

sf_use_s2(FALSE)

spatialjoin_12_xeno1_2 <- sf::st_join(tot_xeno1_2, level12["polygon_id"])
sf::st_write(spatialjoin_12_xeno1_2, "gbif_data_level12_xeno1_2.gpkg",delete_dsn = TRUE)
write.csv(spatialjoin_12_xeno1_2, "gbif_data_level12_xeno1_2.csv", row.names = FALSE)
level12_gbif_xeno1_2 <- sf::st_read(dsn = "gbif_data_level12_xeno1_2.gpkg", stringsAsFactors = FALSE) 

#Obtain occurrence for each subcatchment ----
agglevel12_xeno1_2 <- aggregate(level12_gbif_xeno1_2$PA, by=list(level12_gbif_xeno1_2$species,level12_gbif_xeno1_2$polygon_id), FUN=sum)

#Put lines in row by catchment
xeno1_2_level12<-agglevel12_xeno1_2 %>% 
  group_by(Group.2) %>% 
  mutate(row_id = (Group.1)) %>% 
  pivot_wider(id_cols = "Group.2",  names_from = row_id,
              values_from = x, values_fill=0,
              names_glue = "{.value}{row_id}") %>% 
  ungroup()

xeno1_2_level12<-as.data.frame(xeno1_2_level12)

library(vegan)
xeno1_2_level12$S=specnumber(xeno1_2_level12[,-1])

#Global----
agglevel12_xeno1_2 <- aggregate(level12_gbif_xeno1_2$PA, by=list(level12_gbif_xeno1_2$polygon_id), FUN=sum)
gbif_dataxeno1_2_level12 <- merge(x=xeno1_2_level12,y=agglevel12_xeno1_2, by.x=c("Group.2"), 
                                  by.y=c("Group.1"))

#Two metrics----
colnames(gbif_dataxeno1_2_level12)[colnames(gbif_dataxeno1_2_level12) == 'x'] <- 'Occ_exotic'
colnames(gbif_dataxeno1_2_level12)[colnames(gbif_dataxeno1_2_level12) == 'S'] <- 'SR_exotic'
colnames(gbif_dataxeno1_2_level12)[colnames(gbif_dataxeno1_2_level12) == 'Group.2'] <- 'Catchment_ID'

#You can here reorder before and export
write.csv(gbif_dataxeno1_2_level12,"gbif_dataxeno1_2_level12.csv",row.names = FALSE)

##For rarefaction-----
#We filtered by data publishers and calculate occurrences by subcatchment as such (level 6 to 12)-----