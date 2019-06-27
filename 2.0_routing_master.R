# SET UP
rm(list = ls())
library(foreign)
library(tidyverse)
library(rgdal)
library(raster)
library(stplanr)
library(SDraw)
library(geojsonio)
library(raster)

source("00_Functions/leg_graphhopper.R")
proj_4326 <- CRS("+proj=longlat +init=epsg:4326")   # global projection - lat/long.
proj_27700 <- CRS("+init=epsg:27700")               # UK easting/northing projection - 'projected' (need if working in metres)

####################
# PART 0 : SET UP
####################

# Define input params [NB for now routing buses as trucks]
inputdf <- data.frame(
  mode = c(1:4),
  modename = c("bike", "foot", "car", "truck")  
)

# Define LA list
lad14 <- read.csv("01_DataInput/lad14cd.csv")
lahomelist <- lad14[lad14$lahome==1,]
latravellist <- lad14[lad14$latravel==1,]

####################
# PART 1: MAKE LINES SPATIAL (by LA)
####################
# Load large files
lines_all <- read.csv("02_DataCreated/1_sampleroutes_small.csv")
cents_lsoa <- readOGR(file.path("01_DataInput/lsoa_cents/lsoa_cents_mod.geojson"))
cents_pcd <- read.csv(file.path("01_DataInput/pcd_cents/postcodes_england_latlong.csv"))
coordinates(cents_pcd) <- ~longitude + latitude
proj4string(cents_pcd) <- proj_4326
cents_pcd <- spTransform(cents_pcd, proj_4326)

# Run by LA
for(j in 1:length(lahomelist$lad14cd)){
  lahome <- as.character(lahomelist$lad14cd[j])
  source("2.1_define_lines_by_LA.R")
}

####################
# PART 2: DO ROUTING AND MATRICES (by LA by mode)
####################
# Load and transform large spatial data files
lad14shape <- readOGR(file.path("01_DataInput/lad14_boundaries/Local_Authority_Districts_December_2015_Ultra_Generalised_Clipped_Boundaries_in_Great_Britain.shp")) # [NB LAD15CD always same as LAD14CD]
lad14shape <- spTransform(lad14shape, proj_27700)
# builtup <- readOGR("01_DataInput/built_up_areas/Builtup_Areas_December_2011_Boundaries_V2.shp")
# builtup <- builtup[,names(builtup@data) %in% c("urban_bua")]
# builtup <- spTransform(builtup, proj_27700)
# lad14builtup <- intersect(lad14shape, builtup)
# lad14builtup <- spTransform(lad14builtup, proj_27700)
# geojson_write(lad14builtup, file = ("01_DataInput/lad14_by_builtup/lad14builtup.geojson"))
lad14builtup <- readOGR("01_DataInput/lad14_by_builtup/lad14builtup.geojson")

# Run by LA and mode
for(j in 1:length(lahomelist$lad14cd)){
  lahome <- as.character(lahomelist$lad14cd[j])
  
  # Load lines files
  lines <- readRDS(file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/lines.Rds")))
  lines_toroute <- readRDS(file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/lines_toroute.Rds")))

  # Create matrices by mode
  for(k in (1:4)){
    mode <- as.numeric(k)
    source("2.2_graphhopper_by_LA_mode.R")
    source("2.3_LA_matrices_by_LA_mode.R")
    source("2.4_road_class_matrices_by_LA_mode.R")
  }
}

####################
# PART 3: REJOIN MATRICES
####################
# Join LA matrices to single list by mode
for(k in 1:4) {
  mode <- as.numeric(k)
  # Add files together in a list
  lahome <- as.character(lahomelist$lad14cd[1])
  listla <- read_csv(file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/matla_mode", mode, ".csv")))
  for(j in 2:length(lahomelist$lad14cd)){
    lahome <- as.character(lahomelist$lad14cd[j])
    nextfilela <- read_csv(file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/matla_mode", mode, ".csv")))
    listla <- full_join(listla, nextfilela)
  }
  # Reshape long to wide
    matla_all <- reshape2::dcast(listla, lahome~latravel, value.var="plength")
    matla_all[is.na(matla_all)] <- 0  
  # Save
    write_csv(matla_all, file.path(paste0("02_DataCreated/2_matla_mode", mode, ".csv")))
}

# Join RC matrices to single list by mode
for(k in 1:4) {
  mode <- as.numeric(k)
  # Add files together in a list
  lahome <- as.character(lahomelist$lad14cd[1])
  listrc <- read_csv(file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/matrc_mode", mode, ".csv")))
  for(j in 2:length(lahomelist$lad14cd)){
    lahome <- as.character(lahomelist$lad14cd[j])
    nextfilela <- read_csv(file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/matrc_mode", mode, ".csv")))
    listrc <- full_join(listrc, nextfilela)
  }
  # Modify road types to 6-way type
  listrc$road_class <- "motorway"
  listrc$road_class[listrc$road_classcat==2 & listrc$urban_rural=="urban"] <- "urban_primary"
  listrc$road_class[listrc$road_classcat==2 & listrc$urban_rural=="rural"] <- "rural_primary"
  listrc$road_class[listrc$road_classcat==3 & listrc$urban_rural=="urban"] <- "urban_other"
  listrc$road_class[listrc$road_classcat==3 & listrc$urban_rural=="rural"] <- "rural_other"
  listrc$road_class[listrc$road_classcat==4 ] <- "out_of_scope"
  
  # Multiply up by weights & sum weighted lengths across LAs
  listrc$weightlength <- listrc$lahome_weight * listrc$length
  listrc <- listrc[,c("latravel", "road_class", "weightlength")]
  listrc <- aggregate(. ~latravel+road_class, data=listrc, sum, na.rm=TRUE)

  # Create a percentage
  listrc2 <- listrc[,c("latravel", "weightlength")]
  listrc2 <- dplyr::rename(listrc2, laweightlength = weightlength)
  listrc2 <- aggregate(. ~latravel, data=listrc2, sum, na.rm=TRUE)
  listrc <- left_join(listrc, listrc2, by = "latravel")
  listrc$plength <- listrc$weightlength / listrc$laweightlength
  listrc <- listrc[,c("latravel", "road_class", "plength")]

  # Reshape long to wide
  matrc_all <- reshape2::dcast(listrc, latravel~road_class, value.var="plength")
  matrc_all[is.na(matrc_all)] <- 0 
  # Save
  write_csv(matrc_all, file.path(paste0("02_DataCreated/2_matrc_mode", mode, ".csv")))
}
##TO DO: change matrices to save to mh-execute/input
