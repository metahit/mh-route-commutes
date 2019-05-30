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

####################
# SET UP
####################

proj_4326 <- CRS("+proj=longlat +init=epsg:4326")   # global projection - lat/long.

# Define mode & mode input parameters
mode <- 1
vehicle <- "bike"  # change this and mode together
maxdist_mode <- 15
lsoa_within_dist <- 0.5

# Define LA
lad14 <- read.csv("01_DataInput/lad14cd.csv")
lahome_lad14 <- lad14[lad14$lahome==1,]
latravel_lad14 <- lad14[lad14$latravel==1,]

# Load and subset census lines to be routed
lines <- read.csv("02_DataCreated/1_sampleroutes_small.csv")
lines <- lines[lines$mode4==mode,]
lines <- lines[lines$home_lad14cd %in% lahome_lad14$lad14cd,]


####################
# Make road class matrix
####################
legs <- readRDS(file.path(paste0("02_DataCreated/temp/legs_mode",mode,".Rds"))) ##currently a bodge

# Recode road class
legs@data$road_classcat <- 3
legs@data$road_classcat[legs@data$road_class %in% c("motorway", "motorroad")] <- 1
legs@data$road_classcat[legs@data$road_class %in% c("trunk", "primary")] <- 2
legs@data$road_classcat[legs@data$road_class %in% c("forestry", "path", "steps", "track")] <- 4

# Load builtup data
builtup <- readOGR("01_DataInput/rural_urban/Builtup_Areas_December_2011_Boundaries_V2.shp")
builtup <- builtup[,names(builtup@data) %in% c("urban_bua")]
builtup <- spTransform(builtup, proj_4326)

# Create and populate matrix for road class types
rc_mat <- data.frame("lahome"= character(0), "road_classcat"=numeric(0), "length"=numeric(0), "length_bua"=numeric(0), stringsAsFactors=FALSE)
for(j in 1:length(lahome_lad14$lad14cd)){
  for(k in 1:4){
    lahome <- as.character(lahome_lad14$lad14cd[j])
    if(nrow(legs[legs@data$home_lad14cd==lahome & legs@data$road_classcat==k,])==0) {
    } else {
      print(paste0("home LA ",j, " and road class ", k))
      rc_mat[nrow(rc_mat) + 1,] = list(lahome,k,NA,NA)
      rc_mat$length[(rc_mat$lahome==lahome & rc_mat$road_classcat==k)] <- lineLength(legs[legs@data$home_lad14cd==lahome & legs@data$road_classcat==k,])
      rc_mat$length_bua[(rc_mat$lahome==lahome & rc_mat$road_classcat==k)] <- lineLength(intersect(legs[legs@data$home_lad14cd==lahome & legs@data$road_classcat==k,], builtup), byid = FALSE)
    }
  }
}

# Add in a bit extra to capture within-LSOA routes - assume all on non-motorway, non-primary
lines_within <- lines[(lines$e_dist_km==0),]
for(j in 1:length(lahome_lad14$lad14cd)){
  lahome <- as.character(lahome_lad14$lad14cd[j])
  latravel <- lahome
  rc_mat$length[(rc_mat$lahome==lahome & rc_mat$road_classcat==3)] <-  as.numeric(rc_mat$length[(rc_mat$lahome==lahome & rc_mat$road_classcat==3)] 
                                                                                    + (lsoa_within_dist*as.numeric(sum(lines_within$home_lad14cd==lahome))))
}

# Save road class matrix
write_csv(rc_mat, file.path(paste0("02_DataCreated/2_rcmatrix_mode", mode, ".csv")))



