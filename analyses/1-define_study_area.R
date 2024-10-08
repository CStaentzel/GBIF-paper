#' @title
#' Define the study area 
#' @description 
#' Define the study area to extract GBIF Data for European Macroinvertebrates and fish species
#' 
#' @author Cybill Staentzel \cybill.staentzel@engees.unistra.fr
#' 
#' @date 2023/01/24
#' 

## Define MIN latitude ---
malta <- rnaturalearth::ne_states(country = "malta", returnclass = "sf")
plot(malta)
ymin <- sf::st_bbox(malta)$"ymin"
ymin=32

## Define MIN longitude ---
iceland <- rnaturalearth::ne_states(country = "iceland", returnclass = "sf")
plot(iceland)
xmin <- sf::st_bbox(iceland)$"xmin"

## Define MAX latitude ---
norway <- rnaturalearth::ne_states(country = "norway", returnclass = "sf")
norway <- norway[norway$type_en == "County", ]
ymax <- sf::st_bbox(norway)$"ymax"

## Define MAX longitude ---
ukraine <- rnaturalearth::ne_states(country = "ukraine", returnclass = "sf")
xmax <- sf::st_bbox(ukraine)$"xmax"

## Add space around box ---
xmin <- xmin - 0.50
xmax <- xmax + 1.50
ymin <- ymin - 0.25
ymax <- ymax + 0.35

## Create area box ---
study_bbox <- sf::st_polygon(list(cbind(c(xmin, xmax, xmax, xmin, xmin), 
                                        c(ymin, ymin, ymax, ymax, ymin))))

## Extract WKT expression ---
study_wkt <- sf::st_as_text(study_bbox)

## Convert to real spatial object ---
study_bbox <- sf::st_sfc(study_bbox)
study_bbox <- sf::st_as_sf(study_bbox, crs = 4326)

## Get Europe layer ---
europe <- rnaturalearth::ne_countries(scale = "large", continent = "europe", 
                                      returnclass = "sf")
## Rewrite CRS ---
sf::st_crs(europe) <- 4326

## Crop Europe layer ---
europe <- sf::st_crop(europe, study_bbox)

## Map study area ---
ggplot2::ggplot() + 
  ggplot2::geom_sf(data = europe, ggplot2::aes(fill = sovereignt), 
                   col = "white", size = 0.1) + 
  ggplot2::theme_light() +
  ggplot2::theme(legend.position = "none")

## Export layers ---
sf::st_write(study_bbox, delete_dsn = TRUE,"study_area_bbox.gpkg")
sf::st_write(europe, delete_dsn = TRUE,"study_area_basemap.gpkg")
cat(paste0(study_wkt, "\n"), 
    file = "study_area_wkt.txt")
