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
lahomelist <- lad14[lad14$lahome==1,]
latravellist <- lad14[lad14$latravel==1,]

####################
# PART 1: MAKE LINES SPATIAL (by LA)
####################
# Load large files
lines_all <- read.csv("02_DataCreated/1_sampleroutes.csv")
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

for(j in 1:length(lahomelist$lad14cd)){
  lahome <- as.character(lahomelist$lad14cd[j])
  
  for(k in (1:5)){
    mode <- as.numeric(k)
    
# Route lines by LA and mode
     lines_toroute <- readRDS(file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/lines_toroute.Rds")))
     modename <- inputdf$modename[inputdf$mode== mode]
     lines_toroute_mode <- lines_toroute[lines_toroute@data$mode5==mode,]
     # Remove some bad points that graph hopper doesn't recognise
     if(k==3 & j==107){
       lines_toroute_mode <- lines_toroute_mode[lines_toroute_mode@data$id!="SS39XT E01015893",]
     }
     if(k==5 & j==107){
       lines_toroute_mode <- lines_toroute_mode[lines_toroute_mode@data$id!="SS39XG E01015860",]
       lines_toroute_mode <- lines_toroute_mode[lines_toroute_mode@data$id!="SS39XQ E01015852",]
       lines_toroute_mode <- lines_toroute_mode[lines_toroute_mode@data$id!="SS39XR E01015879",]
     }

     lines_toroute_mode_vars <- unique(lines_toroute_mode@data[,names(lines_toroute_mode@data) %in% c("id","home_lad14cd","work_lad14cd","urbanmatch","lahome_weight")])
     
     source("2.2a_graphhopper_route.R")
     source("2.2b_graphhopper_prepare.R")

# Create matrices by LA and mode
    # Recode road class
    legs <- readRDS(file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/legs_mode",mode,".Rds")))
    legs@data$road_classcat <- 3
    legs@data$road_classcat[legs@data$road_class %in% c("motorway")] <- 1
    legs@data$road_classcat[legs@data$road_class %in% c("trunk", "primary", "motorroad")] <- 2
    legs@data$road_classcat[legs@data$road_class %in% c("path", "steps", "forestry")] <- 4 # judge these are out of stats19 scope, although NB path might be in scope if a footway next a road. See https://github.com/metahit/mh-route-commutes/tree/master/02_DataCreated/national_data_test
    # https://wiki.openstreetmap.org/wiki/Key:highway
    # https://github.com/graphhopper/graphhopper/blob/35b58ddbe8e3aeecd310cf83f637211a6f784093/core/src/main/java/com/graphhopper/routing/profiles/RoadClass.java#L4-L9 
    
    # Define route type
    legs@data$routedistcat <-as.numeric(cut(legs@data$routedist, c(0,5,15,40,100), labels=c(1:4)))
    if (mode==2){
      legs@data$routedistcat <- 1  # walking always lowest cat
    } 
    if (mode %in% c(1, 5)) {
      legs@data$routedistcat[legs@data$routedistcat==4] <- 3 # bike + bus at most level 3
    }
    legs@data$routetype <- "u0d1"
    legs@data$routetype[legs@data$urbanmatch==0 & legs@data$routedistcat==2] <- as.character("u0d2")
    legs@data$routetype[legs@data$urbanmatch==0 & legs@data$routedistcat==3] <- as.character("u0d3")
    legs@data$routetype[legs@data$urbanmatch==0 & legs@data$routedistcat==4] <- as.character("u0d4")
    legs@data$routetype[legs@data$urbanmatch==1 & legs@data$routedistcat==1] <- as.character("u1d1")
    legs@data$routetype[legs@data$urbanmatch==1 & legs@data$routedistcat==2] <- as.character("u1d2")
    legs@data$routetype[legs@data$urbanmatch==1 & legs@data$routedistcat==3] <- as.character("u1d3")
    legs@data$routetype[legs@data$urbanmatch==1 & legs@data$routedistcat==4] <- as.character("u1d4")
    
    # Run all together [comment out if needed]
    legs@data$routetype <- as.character("all")
    routetypelist <- c("all")
    source("2.3_LA_matrices_by_LA_mode.R")
    source("2.4_road_class_matrices_by_LA_mode.R")
    
    # Run by route type [comment out if needed]
    routetypelist <- c("u0d1", "u0d2", "u0d3", "u0d4", "u1d1", "u1d2", "u1d3", "u1d4")
    source("2.3_LA_matrices_by_LA_mode.R")
    source("2.4_road_class_matrices_by_LA_mode.R")
    
  }
}

####################
# PART 3: REJOIN MATRICES
####################

# Join LA matrices to single list by mode
for(k in 1:5) {
  mode <- as.numeric(k)
  for (routetype in c("u0d1", "u0d2", "u0d3", "u0d4", "u1d1", "u1d2", "u1d3", "u1d4")) {
  # Add files together in a list
    lahome <- as.character(lahomelist$lad14cd[1])
    listla <- read_csv(file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/matla_mode", mode, "_", routetype, ".csv")))
    for(j in 2:length(lahomelist$lad14cd)){
      lahome <- as.character(lahomelist$lad14cd[j])
      nextfilela <- read_csv(file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/matla_mode", mode, "_", routetype, ".csv")))
      listla <- full_join(listla, nextfilela)
    }
    listla <- listla[listla$latravel!="x",]
    if(nrow(listla)!=0) {
    
    # Reshape long to wide
      matla_all <- reshape2::dcast(listla, lahome~latravel, value.var="plength")
      matla_all[is.na(matla_all)] <- 0  
    
    write_csv(matla_all, file.path(paste0("../mh-execute/inputs/travel-matrices/mode", mode, "_", routetype, "_matla.csv")))
    }
  }
}

# Join RC matrices to single list by mode [do first just for all, then part 4, then for the other modes]
for(k in 1:5) {
  mode <- as.numeric(k)
  for (routetype in c("all", "u0d1", "u0d2", "u0d3", "u0d4", "u1d1", "u1d2", "u1d3", "u1d4")) {
  
  # Add files together in a list
  lahome <- as.character(lahomelist$lad14cd[1])
  listrc <- read_csv(file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/matrc_mode", mode, "_", routetype,".csv")))
  for(j in 2:length(lahomelist$lad14cd)){
    lahome <- as.character(lahomelist$lad14cd[j])
    nextfilela <- read_csv(file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/matrc_mode", mode, "_", routetype,".csv")))
    listrc <- full_join(listrc, nextfilela)
  }
  listrc  <- listrc[listrc$latravel!="x",] 
  if(nrow(listrc)!=0) {
    # Modify road types to 6-way type
    listrc$road_class <- "motorway"
    listrc$road_class[listrc$road_classcat==2 & listrc$urban_rural=="urban"] <- "urban_primary"
    listrc$road_class[listrc$road_classcat==2 & listrc$urban_rural=="rural"] <- "rural_primary"
    listrc$road_class[listrc$road_classcat==3 & listrc$urban_rural=="urban"] <- "urban_other"
    listrc$road_class[listrc$road_classcat==3 & listrc$urban_rural=="rural"] <- "rural_other"
    listrc$road_class[listrc$road_classcat==4 ] <- "off_public_highway"
    
    # Merge in Motorway/A road scaling weight (circular, as this needs to be created after 'all' has been done)
    listrc$rts_weight <- 1
    if(routetype!= "all") {
      rtsscaling <- read_csv(file.path(paste0("02_DataCreated/rts_ORscaling/allmode_alltrips_ORscaling.csv")))
      rtsscaling <- rtsscaling[rtsscaling$mode5==mode,]
      listrc <- left_join(listrc, rtsscaling)
      listrc$rts_weight[!is.na(listrc$rts_ORscaling)] <- listrc$rts_ORscaling[!is.na(listrc$rts_ORscaling)]
    }
    
    # Multiply up by weights & sum weighted lengths across LAs
    listrc$weightlength <- listrc$lahome_weight * listrc$length * listrc$rts_weight
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
    
    if(routetype== "all") {
      write_csv(matrc_all, file.path(paste0("02_DataCreated/rts_ORscaling/bymode_all_matrc/mode", mode, "_", routetype,"_matrc.csv")))
    } else {
      write_csv(matrc_all, file.path(paste0("../mh-execute/inputs/travel-matrices/mode", mode, "_", routetype,"_matrc.csv")))
    }
  }
  }
}

####################
# PART 4: OR SCALING FOR MOTORWAY AND PRIMARY (do before run part 3b the second time)
####################
## PREPARE GRAPHHOPPER
  # Add graphhopper files together
  graphhopper_matrc <- read_csv(file.path(paste0("02_DataCreated/rts_ORscaling/bymode_all_matrc/mode1_all_matrc.csv")))
  graphhopper_matrc$mode5 <-as.numeric(1) 
  for(k in 2:5){
   mode <- as.numeric(k)
   nextfilemode <- read_csv(file.path(paste0("02_DataCreated/rts_ORscaling/bymode_all_matrc/mode", mode, "_all_matrc.csv")))
   nextfilemode$mode5 <-as.numeric(k) 
   graphhopper_matrc <- full_join(graphhopper_matrc, nextfilemode)
  }
  # Set NA to zero
    graphhopper_matrc$motorway[is.na(graphhopper_matrc$motorway)] <- 0
    graphhopper_matrc$rural_primary[is.na(graphhopper_matrc$rural_primary)] <- 0
    graphhopper_matrc$urban_primary[is.na(graphhopper_matrc$urban_primary)] <- 0
    graphhopper_matrc$urban_other[is.na(graphhopper_matrc$urban_other)] <- 0
    graphhopper_matrc$rural_other[is.na(graphhopper_matrc$rural_other)] <- 0
    graphhopper_matrc$off_public_highway[is.na(graphhopper_matrc$off_public_highway)] <- 0
    
  # Recalculate percentages
    graphhopper_matrc$g_all <- graphhopper_matrc$motorway + graphhopper_matrc$rural_primary + graphhopper_matrc$urban_primary + graphhopper_matrc$rural_other + graphhopper_matrc$urban_other + graphhopper_matrc$off_public_highway
    graphhopper_matrc$g_allam <- graphhopper_matrc$motorway + graphhopper_matrc$rural_primary + graphhopper_matrc$urban_primary
    graphhopper_matrc$g_am <- graphhopper_matrc$g_allam / graphhopper_matrc$g_all
    graphhopper_matrc$g_motorway <- graphhopper_matrc$motorway / graphhopper_matrc$g_allam
    graphhopper_matrc$g_rural_primary <- graphhopper_matrc$rural_primary / graphhopper_matrc$g_allam
    graphhopper_matrc$g_urban_primary <- graphhopper_matrc$urban_primary / graphhopper_matrc$g_allam
    
  # Limit columns
    graphhopper_matrc <- graphhopper_matrc[,c("latravel", "mode5", "g_am", "g_motorway", "g_rural_primary", "g_urban_primary")]
    
## MERGE IN RTS DATA FROM ROB
  # Merge in data
  rts_matrc <- read_csv(file.path(paste0("01_DataInput/RTS/rts_motorway_primary.csv")))
  graphhopper_matrc <- left_join(graphhopper_matrc, rts_matrc)
  
  # Limit to LA of travel (plausibly 100%) and LA done by Rob
  print(summary({sel_latravel <- (graphhopper_matrc$latravel %in% latravellist$lad14cd)}))
  graphhopper_matrc <- graphhopper_matrc[sel_latravel,]  
  lalist_rts <- unique(rts_matrc$latravel)
  print(summary({sel_rts <- (graphhopper_matrc$latravel %in% lalist_rts)}))
  graphhopper_matrc <- graphhopper_matrc[sel_rts,]
  graphhopper_lalist <- unique(graphhopper_matrc$latravel)
  
## GENERATE SCALING OR
  # OR
  graphhopper_matrc$am <- 1 # comment out next line to skip the motorway/primary : minor ratio bit
  graphhopper_matrc$am=(graphhopper_matrc$rts_am / (1 - graphhopper_matrc$rts_am)) / (graphhopper_matrc$g_am / (1 - graphhopper_matrc$g_am))
  
  graphhopper_matrc$motorway=graphhopper_matrc$am*((graphhopper_matrc$rts_motorway / (1 - graphhopper_matrc$rts_motorway)) / (graphhopper_matrc$g_motorway / (1 - graphhopper_matrc$g_motorway)))
  graphhopper_matrc$rural_primary=graphhopper_matrc$am*((graphhopper_matrc$rts_rural_primary / (1 - graphhopper_matrc$rts_rural_primary)) / (graphhopper_matrc$g_rural_primary / (1 - graphhopper_matrc$g_rural_primary)))
  graphhopper_matrc$urban_primary=graphhopper_matrc$am*((graphhopper_matrc$rts_urban_primary / (1 - graphhopper_matrc$rts_urban_primary)) / (graphhopper_matrc$g_urban_primary / (1 - graphhopper_matrc$g_urban_primary)))
  graphhopper_matrc$urban_other <- 1 / (graphhopper_matrc$am)
  graphhopper_matrc$rural_other <- 1 / (graphhopper_matrc$am)
  graphhopper_matrc$off_public_highway <- 1 / (graphhopper_matrc$am)
  
  graphhopper_matrc <- graphhopper_matrc[,c("latravel", "mode5", "motorway", "rural_primary", "urban_primary", "urban_other", "rural_other", "off_public_highway")]

  # Reshape & make NAN = 1
  graphhopper_matrc <- reshape2::melt(graphhopper_matrc, id.vars=c("latravel", "mode5"), 
                                      measure.vars=c("motorway", "rural_primary", "urban_primary", "urban_other", "rural_other", "off_public_highway" ),variable.name="road_class",value.name="rts_ORscaling")
  graphhopper_matrc$rts_ORscaling[is.nan(graphhopper_matrc$rts_ORscaling)] <- 1
  
  # Make walking same as cycling
  for (la in graphhopper_lalist) {
  for (rc in c("motorway", "rural_primary", "urban_primary", "urban_other", "rural_other", "off_public_highway")) {
    graphhopper_matrc$rts_ORscaling[graphhopper_matrc$latravel==la & graphhopper_matrc$road_class==rc & graphhopper_matrc$mode5==2] <-   graphhopper_matrc$rts_ORscaling[graphhopper_matrc$latravel==la & graphhopper_matrc$road_class==rc & graphhopper_matrc$mode5==1]
  }
  }
  
  # Save 
  write_csv(graphhopper_matrc, file.path(paste0("02_DataCreated/rts_ORscaling/allmode_alltrips_ORscaling.csv")))
  