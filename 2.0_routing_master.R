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

####################
# PART 0 : SET UP
####################

# Define input params
# Question: what for bus, mode 4? same as car, or truck/small truck? For now not doing it
inputdf <- data.frame(
  mode = c(1:4),
  modename = c("bike", "foot", "car", "truck")  
)
lsoa_within_dist <- 0.5

# Define LA list
lad14 <- read.csv("01_DataInput/lad14cd.csv")
lahomelist <- lad14[lad14$lahome==1,]
latravellist <- lad14[lad14$latravel==1,]


####################
# PART 1: MAKE LINES SPATIAL (by LA)
####################
# Load large files
lines_all <- read.csv("02_DataCreated/1_sampleroutes_small.csv")
cents_all <- readOGR(file.path("01_DataInput/lsoa_cents/lsoa_cents_mod.geojson"))

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
lad14shape <- spTransform(lad14shape, proj_4326)
builtup <- readOGR("01_DataInput/built_up_areas/Builtup_Areas_December_2011_Boundaries_V2.shp")
builtup <- builtup[,names(builtup@data) %in% c("urban_bua")]
builtup <- spTransform(builtup, proj_4326)

# Run by LA and mode
for(j in 1:length(lahomelist$lad14cd)){
  lahome <- as.character(lahomelist$lad14cd[j])

  # Load lines files
  lines <- readRDS(file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/lines.Rds")))
  lines_toroute <- readRDS(file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/lines_toroute.Rds")))

  # Create matrices by mode
  for(k in 1:3) {
    mode <- as.numeric(k)
    source("2.2_graphhopper_by_LA_mode.R")
   # source("2.3_matrices_by_LA_mode.R")
  }
}

####################
# PART 3: REJOIN MATRICES
####################


