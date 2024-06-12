#' @title
#' Clean GBIF data
#' @description 
#' Clean GBIF Data for European Macroinvertebrates and fish species
#' 
#' @author Cybill Staentzel \cybill.staentzel@engees.unistra.fr
#' 
#' @date 2023/01/24
#' 

## Filters ---
basis_of_record <- c("HUMAN_OBSERVATION", "OCCURRENCE", "OBSERVATION")
occurrence_status <- "PRESENT"

## Columns to keep ----

col_names <- c("datasetKey","occurrenceID","year", "eventDate", "class", "order", "family", "genus", 
               "species", "decimalLongitude", "decimalLatitude","download_key")

## Read GBIF downloads info ---
downloads_info <- read.csv("to define by user", header = TRUE)
downloads_info$download_key <- gsub(",", "", downloads_info$download_key.)
gbif_data <- data.frame()

#Extract and read zip.

for (i in seq_len(nrow(downloads_info))) {
  
  cat("Processing file", i, "on", nrow(downloads_info), "\n")
  
  
  ## Import GBIF raw data du dossier de téléchargement ---
  
  tab <- rgbif::occ_download_import(key   = downloads_info[i, "download_key"], 
                                    path  = "to define by user", 
                                    quote = "")
  
  tab <- as.data.frame(tab)
  tab$download_key<-downloads_info[i, "download_key"]
  
  ## Filter BASIS OF RECORDS ---
  tab <- tab[which(tab$"basisOfRecord" %in% basis_of_record), ]
  
  ## Filter OCCURRENCE STATUS ---
  tab <- tab[which(tab$"occurrenceStatus" %in% occurrence_status), ]

  ## Keep only NON NA SPECIES ---
  tab <- tab[which(!is.na(tab$"species")), ]
  tab <- tab[which(tab$"species" != ""), ]
  
  ## Select columns ---
  tab <- tab[ , col_names]
  
  ## Append to final data frame ---
  gbif_data <- rbind(gbif_data, tab)
}

##Convert dates ---
gbif_data$"eventDate" <- as.character(as.Date(gbif_data$"eventDate"))
str(gbif_data)

## Remove duplicates ---
keys <- paste(gsub("\\s", "_", gbif_data$"species"), gbif_data$"eventDate",gbif_data$"period", gbif_data$"datasetKey",
              gbif_data$"decimalLongitude", gbif_data$"decimalLatitude", 
              sep = "__")


pos <- which(duplicated(keys))
if (length(pos)) gbif_data <- gbif_data[-pos, ]

## Reorder rows ---
gbif_data <- gbif_data[with(gbif_data, order(species, eventDate)), ]
rownames(gbif_data) <- NULL

## Convert to spatial object ---
gbif_data_sf <- sf::st_as_sf(gbif_data, coords = c("decimalLongitude", "decimalLatitude"),
                             crs = 4326)

#We removed all observations from Russia

## Export layers ---
sf::st_write(gbif_data_sf, delete_dsn = TRUE,"to define by user".gpkg")

##For rarefaction
#same was done