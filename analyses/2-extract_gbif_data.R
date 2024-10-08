#' @title
#' Extract GBIF data
#' @description 
#' Select and extract GBIF Data for European Macroinvertebrates & fish species
#' 
#' @author Cybill Staentzel \cybill.staentzel@engees.unistra.fr
#' 
#' @date 2023/01/24
#' 

# Invertebrates----
## Get GBIF taxonKey for each---
taxon_name <- "Clitellata"
taxon_name <- "Polychaeta"
taxon_name <- "Culicidae"
taxon_name <- "Brachyceridae"
taxon_name <- "Hydrophilidae" 
taxon_name <- "Ametropodidae"
taxon_name <- "Crambidae"
taxon_name <- "Coenagrionidae"
taxon_name <- "Pseudoneureclipsidae"
taxon_name <- "Amphipoda"
taxon_name <- "Decapoda"
taxon_name <- "Isopoda"
taxon_name <- "Mysidae"
taxon_name <- "Pectinatellidae"
taxon_name <- "Cordylophoridae"
taxon_name <- "Olindiidae"
taxon_name <- "Bivalvia"
taxon_name <- "Gastropoda"
taxon_name <- "Tricladida"
taxon_name <- "Planorbidae"

# Fish----
## Get GBIF taxonKey---
library(rgbif)
fish <- name_backbone(name = "Chordata", kingdom = "Animalia")
fish_taxonkey <- fish$usageKey #44

## Get the spatial extent --
study_area_wkt <-readLines("study_area_wkt.txt")

## Set temporal extent ---
periods <- list()
periods[[1]] <- c("1972", "1982")
periods[[2]] <- c("1982", "1992") 
periods[[3]] <- c("1992", "2002")
periods[[4]] <- c("2002", "2012")
periods[[5]] <- c("2012", "2022")

#Extract from GBIF website----

downloads_info <- data.frame()
info_requests  <- list()
k              <- 1

user= "to define by user"
pwd= "to define by user"
email= "to define by user"

folder<- "to define by user"

for (i in 1:length(periods)) {

  cat("Requesting chunk", i, "on", length(periods), "\n")
  
  if (k <= 3) {
    
    ## Prepare ZIP file on GBIF server ---
    
    info_requests[[k]] <- rgbif::occ_download(
      rgbif::pred("taxonKey", "to define by user"),
      rgbif::pred("hasCoordinate", TRUE),
      rgbif::pred("hasGeospatialIssue", FALSE),
      rgbif::pred("geometry", study_area_wkt),
      rgbif::pred_gte("year", min(periods[[i]])),
      rgbif::pred_lte("year", max(periods[[i]])),
      format = "SIMPLE_CSV",
      user   = user,
      pwd    = pwd,
      email  = email)
    
    ## Save request info ---
    
    info_requests[[k]] <- data.frame("download_key" = info_requests[[k]][1], 
                                     do.call(cbind.data.frame, 
                                             attributes(info_requests[[k]])))
    
    downloads_info <- rbind(downloads_info, info_requests[[k]])
    
    k <- k + 1
  }
  
  if (k > 3) {
    
    
    ## Wait until ZIP files are done on GBIF servers ---
    
    for (n in seq_len(k - 1)) {
      rgbif::occ_download_wait(info_requests[[n]]$"download_key", 
                               status_ping = 60) 
    }
    
    
    ## Downloads ZIP files ---
    
    for (n in seq_len(k - 1)) {
      rgbif::occ_download_get(key       = info_requests[[n]]$"download_key",
                              path      = #to define by user,
                              overwrite = TRUE)
    }
    
    
    ## Reset objects ---
    
    k             <- 1
    info_requests <- list()
  }
}


#Export zip.
write.csv(downloads_info, "to define by user", row.names = FALSE)

##For rarefaction
#All macroinvertebrates were extracted from GBIF : PORIFERA, CNIDARIA, BRYOZOA, PLATYHELMINTHES, NEMATODA, ANNELIDA, MOLLUSCA, ARTHOPODA
