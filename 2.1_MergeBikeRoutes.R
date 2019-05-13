# SET UP
rm(list = ls())
library(foreign)
library(tidyverse)
library(rgdal)
#library(raster)
library(stplanr)
#library(SDraw)
# # Source parallel version of line2route (depreciated in stplanr 0.1.9) - to make it faster
# source("https://github.com/ropensci/stplanr/raw/18a598674bb378d5577050178da1561489496157/R/od-funs.R")

memory.limit(size=1000000)

proj_4326 <- CRS("+proj=longlat +init=epsg:4326")   # global projection - lat/long.

# Define LA and mode
  region_lad14 <- c("E06000022", "E06000023", "E06000024", "E06000025")
  commutemode <- 1
  maxdist_mode <- 20

# Load and subset census lines to be routed
  lines <- read.csv("1_DataCreated/1_sampleroutes_oneach.csv")
  lines <- lines[lines$mode4==commutemode,]
  lines <- lines[lines$home_lad14cd %in% region_lad14,]

# Load geo data, merge centroids, calc distance
  cents_all <- readOGR(file.path("0_DataInput/lsoa_cents/lsoa_cents_mod.geojson"))
  match1 <- match(lines$geo_code1, cents_all$lsoa11cd) # generates a number - where in cents_all is found each $home in lines
  match2 <- match(lines$geo_code2, cents_all$lsoa11cd)
  lines <- lines[!is.na(match1) & !is.na(match2),] # remove line outside the required build region, or no geographical origin/dest
  coords1 <- cents_all@coords[match(lines$geo_code1, cents_all$lsoa11cd),] # gets the coords from 'match1' position of cents
  coords2 <- cents_all@coords[match(lines$geo_code2, cents_all$lsoa11cd),]
  lines$e_dist_km <- geosphere::distHaversine(p1 = coords1, p2 = coords2) / 1000 # assign euclidean dist

# Restrict to selected lines
 lines_toroute_data <- lines[(lines$e_dist_km < maxdist_mode) & !is.na(lines$e_dist_km) & lines$e_dist_km!=0,]
 lines_toroute_data <- lines_toroute_data[,c("geo_code1", "geo_code2", "id", "home_lad14cd", "e_dist_km")]
 lines_toroute_data <- lines_toroute_data[order(lines_toroute_data$id),]
 saveRDS(lines_toroute_data, (file.path("1_DataCreated/lines_toroute_data.Rds")))
 
# MAKE A SPATIAL OBJECT OF CS LINES
 lines_toroute_lines <- od2line(flow = lines_toroute_data, zones = cents_all, destinations = cents_all) # slower implementation for where o and d have different geography
 rownames(lines_toroute_data) <- sapply(1:length(lines_toroute_lines), function(j) lines_toroute_lines@lines[[j]]@ID) # FORCE DATA ROW NAMES TO BE SAME AS ID IN LINES (in case don't start from '1')
 lines_toroute <- SpatialLinesDataFrame(sl = lines_toroute_lines, data = lines_toroute_data)
 lines_toroute <- spTransform(lines_toroute, proj_4326)
 
 route_type <- "fastest" # run for fastest then quietest
 routes <- line2route(lines_toroute, route_fun = route_cyclestreet, plan = route_type, n_processes = 10, base_url = "http://pct.cyclestreets.net/api/")
 


# # Load LA data [NB LAD15CD always same as LAD14CD]
# lad14 <- readOGR(file.path("0_DataInput/lad14_boundaries/Local_Authority_Districts_December_2015_Ultra_Generalised_Clipped_Boundaries_in_Great_Britain.shp"))
# lad14 <- spTransform(lad14, proj_4326)
# 
# # Create and populate matrix
# mat <- data.frame("lahome"= character(0), "latravel"=character(0), "length"=numeric(0), stringsAsFactors=FALSE)
# for(j in 1:length(region_lad14)){
# for(k in 1:length(region_lad14)){
# lahome <- region_lad14[j]
# latravel <- region_lad14[k]
# mat[nrow(mat) + 1,] = list(lahome,latravel,NA)
# mat$length[(mat$lahome==lahome & mat$latravel==latravel)] <- lineLength(intersect(routes[routes@data$home_lad14cd==lahome,], lad14[lad14@data$lad15cd==latravel,]), byid = FALSE)
# }
# }
