# SET UP
rm(list = ls())
library(foreign)
library(tidyverse)
library(rgdal)
library(raster)
library(stplanr)
library(SDraw)

####################
# SET UP
####################

proj_4326 <- CRS("+proj=longlat +init=epsg:4326")   # global projection - lat/long.

# Read route_graphhopper2 function
source("route_graphhopper2.R")

# Define mode & mode input parameters
mode <- 1
maxdist_mode <- 15
lsoa_within_dist <- 0.5

# Define LA
lad14 <- read.csv("0_DataInput/lad14cd.csv")
lahome_lad14 <- lad14[lad14$lahome==1,]
latravel_lad14 <- lad14[lad14$latravel==1,]

# Load and subset census lines to be routed
lines <- read.csv("1_DataCreated/1_sampleroutes.csv")
lines <- lines[lines$mode4==mode,]
lines <- lines[lines$home_lad14cd %in% lahome_lad14$lad14cd,]

####################
# Do Routing
####################

# Load geo data, merge centroids, calc distance
cents_all <- readOGR(file.path("0_DataInput/lsoa_cents/lsoa_cents_mod.geojson"))
match1 <- match(lines$geo_code_o, cents_all$lsoa11cd) # generates a number - where in cents_all is found each $home in lines
match2 <- match(lines$geo_code_d, cents_all$lsoa11cd)
lines <- lines[!is.na(match1) & !is.na(match2),] # remove line outside the required build region, or no geographical origin/dest
coords1 <- cents_all@coords[match(lines$geo_code_o, cents_all$lsoa11cd),] # gets the coords from 'match1' position of cents
coords2 <- cents_all@coords[match(lines$geo_code_d, cents_all$lsoa11cd),]

routes <- list()
for (i in 1:10){
  start_point <- coords1[i,] %>% as.numeric()
  end_point <- coords2[i,] %>% as.numeric()
  print(i)
  
  routes[[i]] <- route_graphhopper2(start_point, end_point, vehicle = "car", return_JSON_obj = T)
  
}

# https://graphhopper.com/api/1/route?point=49.932707,11.588051&point=50.3404,11.64705&vehicle=car&debug=true&type=json&details=road_class&[YOUR-KEY]