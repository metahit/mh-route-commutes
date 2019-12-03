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

# Define input params [NB for now routing motorbikes as cars, buses as trucks]
inputdf <- data.frame(
  mode = c(1:5),
  modename = c("bike", "foot", "car", "car", "truck")  
)

# Define LA list
lad14 <- read.csv("01_DataInput/lad14cd.csv")
lahomelist <- lad14
cityregion <- lad14[,c("lad14cd", "cityregion")]

# ALL ROUTING NATIONALLY: WHAT % IN/OUT OF CITY REGION
for(k in 1:5) {
  mode <- as.numeric(k)
  for (routetype in c("all")) {
    # Add files together in a list
    lahome <- as.character(lahomelist$lad14cd[1])
    listla <- read_csv(file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/matla_mode", mode, "_", routetype, ".csv")))
    for(j in 2:length(lahomelist$lad14cd)){
      lahome <- as.character(lahomelist$lad14cd[j])
      nextfilela <- read_csv(file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/matla_mode", mode, "_", routetype, ".csv")))
      listla <- full_join(listla, nextfilela)
    }
    listla <- listla[listla$latravel!="x",]
    listla <- left_join(listla, cityregion, by = c("lahome" = "lad14cd"))
    listla <- dplyr::rename(listla, crhome = cityregion)
    listla <- left_join(listla, cityregion, by = c("latravel" = "lad14cd"))
    listla <- dplyr::rename(listla, crtravel = cityregion)
    listla$crtravel[listla$latravel=="E06000053"] <- "otherengland" # Isles of scilly
    listla$crtravel[listla$latravel=="E09000001"] <- "london"       # london

    if(nrow(listla)!=0) {
      
      # Reshape long to wide
      matla_all <- reshape2::dcast(listla, crhome~crtravel, fun.aggregate = sum, value.var="length")
      matla_all[is.na(matla_all)] <- 0
      
      write_csv(matla_all, file.path(paste0("02_DataCreated/city_region_inout/mode", mode, "_", routetype, "_matla.csv")))
    }
  }
}