# SET UP
rm(list = ls())
library(foreign)
library(tidyverse)
library(rgdal)
library(stplanr)
# # Source parallel version of line2route (depreciated in stplanr 0.1.9) - to make it faster
# source("https://github.com/ropensci/stplanr/raw/18a598674bb378d5577050178da1561489496157/R/od-funs.R")

memory.limit(size=1000000)

# Define cents and lines
cents_all <- readOGR(file.path("0_DataInput/lsoa_cents/lsoa_cents_mod.geojson"))
# https://github.com/npct/pct-inputs/blob/master/02_intermediate/01_geographies/lsoa_cents_mod.geojson")
lines_toroute_data <- readRDS(file.path("1_DataCreated/lines_toroute_data.Rds"))
# https://github.com/metahit/mh-route-commutes/blob/master/1_DataCreated/lines_toroute_data.Rds


# MAKE A SPATIAL OBJECT OF LINES
lines_toroute_lines <- od2line(flow = lines_toroute_data, zones = cents_all, destinations = cents_all) 
rownames(lines_toroute_data) <- sapply(1:length(lines_toroute_lines), function(j) lines_toroute_lines@lines[[j]]@ID) # FORCE DATA ROW NAMES TO BE SAME AS ID IN LINES (in case don't start from '1')
lines_toroute <- SpatialLinesDataFrame(sl = lines_toroute_lines, data = lines_toroute_data)
proj_4326 <- CRS("+proj=longlat +init=epsg:4326")   # global projection - lat/long.
lines_toroute <- spTransform(lines_toroute, proj_4326)

# TRY TO ROUTE IN CYCLE STREETS
route_type <- "fastest" # run for fastest then quietest
routes <- line2route(lines_toroute, route_fun = route_cyclestreet, plan = route_type, n_processes = 10, base_url = "http://pct.cyclestreets.net/api/")
