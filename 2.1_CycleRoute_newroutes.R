# SET UP
rm(list = ls())
library(foreign)
library(tidyverse)
library(rgdal)
library(raster)
library(stplanr)
library(SDraw)

memory.limit(size=1000000)

proj_4326 <- CRS("+proj=longlat +init=epsg:4326")   # global projection - lat/long.

# Define LA and mode
  lad14 <- read.csv("0_DataInput/lad14cd.csv")
  lahome_lad14 <- lad14[lad14$lahome==1,]
  latravel_lad14 <- lad14[lad14$latravel==1,]
  commutemode <- 1
  maxdist_mode <- 15
  lsoa_within_dist <- 0.5

# Load and subset census lines to be routed
  lines <- read.csv("1_DataCreated/1_sampleroutes.csv")
  lines <- lines[lines$mode4==commutemode,]
  lines <- lines[lines$home_lad14cd %in% lahome_lad14$lad14cd,]

# Load geo data, merge centroids, calc distance
  cents_all <- readOGR(file.path("0_DataInput/lsoa_cents/lsoa_cents_mod.geojson"))
  match1 <- match(lines$geo_code_o, cents_all$lsoa11cd) # generates a number - where in cents_all is found each $home in lines
  match2 <- match(lines$geo_code_d, cents_all$lsoa11cd)
  lines <- lines[!is.na(match1) & !is.na(match2),] # remove line outside the required build region, or no geographical origin/dest
  coords1 <- cents_all@coords[match(lines$geo_code_o, cents_all$lsoa11cd),] # gets the coords from 'match1' position of cents
  coords2 <- cents_all@coords[match(lines$geo_code_d, cents_all$lsoa11cd),]
  lines$e_dist_km <- geosphere::distHaversine(p1 = coords1, p2 = coords2) / 1000 # assign euclidean dist

# Restrict to selected between-zone lines
  lines_toroute_data <- lines[(lines$e_dist_km < maxdist_mode) & !is.na(lines$e_dist_km) & lines$e_dist_km!=0,]
  lines_toroute_data <- lines_toroute_data[,c("geo_code_o", "geo_code_d", "id", "home_lad14cd", "e_dist_km")]
  lines_toroute_data <- lines_toroute_data[order(lines_toroute_data$id),]

# MAKE A SPATIAL OBJECT OF CS LINES
  lines_toroute_lines <- od2line(flow = lines_toroute_data, zones = cents_all, destinations = cents_all)
  rownames(lines_toroute_data) <- sapply(1:length(lines_toroute_lines), function(j) lines_toroute_lines@lines[[j]]@ID) # FORCE DATA ROW NAMES TO BE SAME AS ID IN LINES (in case don't start from '1')
  lines_toroute <- SpatialLinesDataFrame(sl = lines_toroute_lines, data = lines_toroute_data)
  lines_toroute <- spTransform(lines_toroute, proj_4326)
  #saveRDS(lines_toroute, (file.path("1_DataCreated/temp/lines_toroute.Rds")))
 
# ROUTE LINES FROM SCRATCH
  route_type <- "fastest" 
  routes <- line2route(lines_toroute, route_fun = route_cyclestreet, plan = route_type,  base_url = "http://pct.cyclestreets.net/api/")
  # to do: batch this section like in pct
  routes@data <- left_join(routes@data, unique(lines_toroute@data), by="id")
  rownames(routes@data) <- sapply(1:length(routes), function(j) routes@lines[[j]]@ID) # FORCE DATA ROW NAMES TO BE SAME AS ID IN LINES (in case don't start from '1')
  
# Load LA data [NB LAD15CD always same as LAD14CD]
  lad14 <- readOGR(file.path("0_DataInput/lad14_boundaries/Local_Authority_Districts_December_2015_Ultra_Generalised_Clipped_Boundaries_in_Great_Britain.shp"))
  lad14 <- spTransform(lad14, proj_4326)

# Create and populate matrix for between-routes
  la_mat <- data.frame("lahome"= character(0), "latravel"=character(0), "length"=numeric(0), "percent"=numeric(0), stringsAsFactors=FALSE)
  for(j in 1:length(lahome_lad14$lad14cd)){
  for(k in 1:length(latravel_lad14$lad14cd)){
  lahome <- as.character(lahome_lad14$lad14cd[j])
  latravel <- as.character(latravel_lad14$lad14cd[k])
  if(is.null(intersect(routes[routes@data$home_lad14cd==lahome,], lad14[lad14@data$lad15cd==latravel,]))) {
  #  la_mat$length[(la_mat$lahome==lahome & la_mat$latravel==latravel)] <- 0
  } else {
    la_mat[nrow(la_mat) + 1,] = list(lahome,latravel,NA,NA)
    la_mat$length[(la_mat$lahome==lahome & la_mat$latravel==latravel)] <- lineLength(intersect(routes[routes@data$home_lad14cd==lahome,], lad14[lad14@data$lad15cd==latravel,]), byid = FALSE)
  }
  }
  }

# Add in a bit extra to capture within-LSOA routes, then turn to percentages
  lines_within <- lines[(lines$e_dist_km==0),]
  for(j in 1:length(lahome_lad14$lad14cd)){
   lahome <- as.character(lahome_lad14$lad14cd[j])
   latravel <- lahome
   la_mat$length[(la_mat$lahome==lahome & la_mat$latravel==latravel)] <-  as.numeric(la_mat$length[(la_mat$lahome==lahome & la_mat$latravel==latravel)] 
                                                                + (lsoa_within_dist*as.numeric(sum(lines_within$home_lad14cd==lahome))))
  }
  
# Save matrix
  write_csv(la_mat, file.path(paste0("1_DataCreated/2_lamatrix_mode", commutemode, ".csv")))
