# SET UP
rm(list = ls())
library(foreign)
library(tidyverse)
library(rgdal)
library(rmapshaper)
library(rgeos)
library(geojsonio)
proj_4326 <- CRS("+proj=longlat +init=epsg:4326")   # global projection - lat/long.
proj_27700 <- CRS("+init=epsg:27700")               # UK easting/northing projection - 'projected' (need if working in metres)


# ###########
# # CHECK FOR A SAMPLE: DISTANCES FROM ROADS, TO CHECK STATS19 OK
# ###########
# # Load road type
#   osm <- readOGR("01_DataInput/osm/gis_osm_roads_free_1.shp")
#   osm_motorway <- osm[(osm@data$fclass %in% c("motorway", "motorway_link")),]
#   osm_motorway <- gLineMerge(osm_motorway)
#   osm_motorway <- spTransform(osm_motorway, proj_27700)
# 
#   osm_primary <- osm[(osm@data$fclass %in% c("primary", "primary_link", "trunk", "trunk_link")),]
#   osm_primary <- gLineMerge(osm_primary)
#   osm_primary <- spTransform(osm_primary, proj_27700)
# 
#   # Load injuries and make an sp file
#   injuries <- read.csv("01_DataInput/stats19/stats19_KSI_2005-17_sample.csv")
#   injuries <- injuries[,names(injuries) %in% c("accident_index", "location_easting_osgr", "location_northing_osgr", "longitude", "latitude", "road_class")]
#   injuries  <- unique(injuries)
#   coordinates(injuries) <- ~longitude + latitude
#   proj4string(injuries) <- proj_4326
#   injuries <- spTransform(injuries, proj_4326)
# 
# # Find distance to motorway and primary
#   osm_motorwaybf <- gBuffer(osm_motorway, byid=F, width = 10)
#   injuries@data$within10mot <- as.numeric( gIntersects(injuries, osm_motorwaybf, byid = T ))
#   osm_motorwaybf <- gBuffer(osm_motorway, byid=F, width = 50)
#   injuries@data$within50mot <- as.numeric( gIntersects(injuries, osm_motorwaybf, byid = T ))
#   osm_motorwaybf <- gBuffer(osm_motorway, byid=F, width = 250)
#   injuries@data$within250mot <- as.numeric( gIntersects(injuries, osm_motorwaybf, byid = T ))
# 
#   osm_primarybf <- gBuffer(osm_primary, byid=F, width = 10)
#   injuries@data$within10pri <- as.numeric( gIntersects(injuries, osm_primarybf, byid = T ))
#   osm_primarybf <- gBuffer(osm_primary, byid=F, width = 50)
#   injuries@data$within50pri <- as.numeric( gIntersects(injuries, osm_primarybf, byid = T ))
#   osm_primarybf <- gBuffer(osm_primary, byid=F, width = 250)
#   injuries@data$within250pri <- as.numeric( gIntersects(injuries, osm_primarybf, byid = T ))
#   
#   write.csv(injuries@data, file = file.path("02_DataCreated/extras/stats19_sample_roaddist.csv"))

###########
# ASSIGN FOR ALL: ASSIGN WHETHER URBAN OR RURAL
###########
# Load built-up areas files
  builtup <- readOGR("01_DataInput/rural_urban/Builtup_Areas_December_2011_Boundaries_V2.shp")
  builtup <- builtup[,names(builtup@data) %in% c("urban_bua")]
  builtup <- spTransform(builtup, proj_4326)
  
# Load injuries
  injuries <- read.csv("01_DataInput/stats19/stats19_KSI_2005-17.csv")
  injuries <- injuries[,names(injuries) %in% c("accident_index", "lanum_stats19", "longitude", "latitude")]
  injuries  <- unique(injuries)
  
  injuriesmissing <- injuries[is.na(injuries$latitude),]
  injuriesmissing <- injuriesmissing[,names(injuriesmissing) %in% c("accident_index", "lanum_stats19")]
  injuriesmissing$urban_bua <- as.numeric(NA)

# Make injuries spatial
  injuries <- injuries[!is.na(injuries$latitude),]
  coordinates(injuries) <- ~longitude + latitude
  proj4string(injuries) <- proj_4326
  injuries <- spTransform(injuries, proj_4326)
  
# Do an overlap for area type
  proj4string(injuries) <- proj4string(builtup)
  injuries@data <- cbind(injuries@data, over(injuries, builtup))
  
# Recode NA to not urban
  injuries@data$urban_bua <- ifelse(injuries@data$urban_bua=="Yes",1,0)
  injuries@data$urban_bua[is.na(injuries@data$urban_bua)] <- 0
  
# Make data frame, bind back in missing
  injuriesdf <- full_join(injuries@data, injuriesmissing)
  
# Recode inner London to urban - injuries on bridges
  injuriesdf$urban_bua[(injuriesdf$lanum_stats19 %in% 1:12)] <- 1
  
# Save
  write.csv(injuries@data, file = file.path("02_DataCreated/3_stats19_bua.csv"))
  # NB for 2017 the overlap is close to perfect; prior to that not, because using old 2001 definition
  
