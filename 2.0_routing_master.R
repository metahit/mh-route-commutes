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
#lahomelist <- lad14[lad14$lahome==1,]
lahomelist <- lad14 # do nationally
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
    legs@data$road_classcat <- 2
    legs@data$road_classcat[legs@data$road_class %in% c("motorway")] <- 1
    # https://wiki.openstreetmap.org/wiki/Key:highway
    # https://github.com/graphhopper/graphhopper/blob/35b58ddbe8e3aeecd310cf83f637211a6f784093/core/src/main/java/com/graphhopper/routing/profiles/RoadClass.java#L4-L9 
    
    # Define route type (see '09x-drafts\191128_CodeAllLANAt' for 6 road types; add back in urban etc - but then OR scaling not work)
    legs@data$routedistcat <-as.numeric(cut(legs@data$routedist, c(0,5,15,40,100), labels=c(1:4)))
    if (mode==2){
      legs@data$routedistcat <- 1  # walking always lowest cat
    } 
    if (mode %in% c(1, 5)) {
      legs@data$routedistcat[legs@data$routedistcat==4] <- 3 # bike + bus at most level 3
    }
    
    # Run all together [comment out if needed]
    legs@data$routetype <- as.character("all")
    routetypelist <- c("all")
    source("2.3_LA_matrices_by_LA_mode.R")
    source("2.4_road_class_matrices_by_LA_mode.R")
    
    # Run by route type [comment out if needed]
    legs@data$routetype <- "u0d1"
    legs@data$routetype[legs@data$urbanmatch==0 & legs@data$routedistcat==2] <- as.character("u0d2")
    legs@data$routetype[legs@data$urbanmatch==0 & legs@data$routedistcat==3] <- as.character("u0d3")
    legs@data$routetype[legs@data$urbanmatch==0 & legs@data$routedistcat==4] <- as.character("u0d4")
    legs@data$routetype[legs@data$urbanmatch==1 & legs@data$routedistcat==1] <- as.character("u1d1")
    legs@data$routetype[legs@data$urbanmatch==1 & legs@data$routedistcat==2] <- as.character("u1d2")
    legs@data$routetype[legs@data$urbanmatch==1 & legs@data$routedistcat==3] <- as.character("u1d3")
    legs@data$routetype[legs@data$urbanmatch==1 & legs@data$routedistcat==4] <- as.character("u1d4")
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
    # Modify road types to motorway/other
    listrc$road_class <- "motorway"
    listrc$road_class[listrc$road_classcat==2 ] <- "other"
    
    # Rename City of London and Isles Scilly
    listrc$latravel[listrc$latravel=="E06000053"] <- "E06000052" # Isles of scilly
    listrc$latravel[listrc$latravel=="E09000001"] <- "E09000033" # City of London
    
    # Merge in motorway scaling weight (circular, as this needs to be created after 'all' has been done - and can only be done for areas with RTS decided)
    listrc$rts_weight <- 1
    if(routetype!= "all" & mode %in% (3:5)) {
      rtsscaling <- read_csv(file.path(paste0("02_DataCreated/rts_ORscaling/alltrips_ORscaling.csv")))
      rtsscaling <- rtsscaling[rtsscaling$mode5==mode,]
      listrc <- left_join(listrc, rtsscaling, by = c("latravel", "road_class"))
      listrc$rts_weight[!is.na(listrc$rts_ORscaling)] <- listrc$rts_ORscaling[!is.na(listrc$rts_ORscaling)]
    }
    
    # Multiply up by weights & sum weighted lengths across LAs (NB weightlenght weights only b lahome weight, not RTS scaling)
    listrc_rtswt <- unique(listrc[,c("latravel", "road_class", "rts_weight")])
    listrc$weightlength <- listrc$lahome_weight * listrc$length
    listrc <- listrc[,c("latravel", "road_class", "weightlength")]
    listrc <- aggregate(. ~latravel+road_class, data=listrc, sum, na.rm=TRUE)
    listrc <- left_join(listrc, listrc_rtswt, by = c("latravel", "road_class"))
    
    # Create a percentage
    listrc2 <- listrc[,c("latravel", "weightlength")]
    listrc2 <- dplyr::rename(listrc2, laweightlength = weightlength)
    listrc2 <- aggregate(. ~latravel, data=listrc2, sum, na.rm=TRUE)
    listrc <- left_join(listrc, listrc2, by = "latravel")
    listrc$plength_raw <- listrc$weightlength / listrc$laweightlength
    
    # Convert percentage to an OR, multiply by OR scaling weight, and convert back to percent. Make nan = 1 (when only 'other' roads)
    listrc$newor <- (listrc$plength_raw / (1 - listrc$plength_raw)) * listrc$rts_weight
    listrc$plength <- listrc$newor / (1 + listrc$newor)
    listrc$plength[is.nan(listrc$plength)] <- 1
    listrc <- listrc[,c("latravel", "road_class", "weightlength", "plength")]
    
    if(routetype== "all") {
      matrc_all <- reshape2::dcast(listrc, latravel~road_class, value.var="weightlength")
      matrc_all[is.na(matrc_all)] <- 0 
      write_csv(matrc_all, file.path(paste0("02_DataCreated/rts_ORscaling/bymode_all_matrc/mode", mode, "_", routetype,"_matrc.csv")))
    } else {
      # Reshape long to wide
      matrc_all <- reshape2::dcast(listrc, latravel~road_class, value.var="plength")
      matrc_all[is.na(matrc_all)] <- 0
      if(mode %in% (1:2)) {
        matrc_all$motorway <- 0
        matrc_all <- matrc_all[,c(1,3,2)]
      }      
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
  graphhopper_matrc <- read_csv(file.path(paste0("02_DataCreated/rts_ORscaling/bymode_all_matrc/mode3_all_matrc.csv")))
  graphhopper_matrc$mode5 <-as.numeric(3) 
  for(k in 4:5){
   mode <- as.numeric(k)
   nextfilemode <- read_csv(file.path(paste0("02_DataCreated/rts_ORscaling/bymode_all_matrc/mode", mode, "_all_matrc.csv")))
   nextfilemode$mode5 <-as.numeric(k) 
   graphhopper_matrc <- full_join(graphhopper_matrc, nextfilemode)
  }
  # Get total
    graphhopper_matrc$all <- graphhopper_matrc$motorway + graphhopper_matrc$other
    
  # Merge in city region
    cityregion <- lad14[,c("lad14cd", "cityregion")]
    graphhopper_matrc <- left_join(graphhopper_matrc, cityregion, by = c("latravel" = "lad14cd"))
    
  # Create graphhopper city region level of motorway
    graphhopper_matrc2 <- graphhopper_matrc[,c("cityregion", "mode5", "motorway", "all")]
    graphhopper_matrc2 <- aggregate(. ~cityregion+mode5, data=graphhopper_matrc2, sum, na.rm=TRUE)
    graphhopper_matrc2$g_motorway <- graphhopper_matrc2$motorway / graphhopper_matrc2$all
    graphhopper_matrc2 <- graphhopper_matrc2[,c("cityregion", "mode5", "g_motorway")]
    graphhopper_matrc <- left_join(graphhopper_matrc, graphhopper_matrc2, by = c("mode5", "cityregion"))

  # Limit columns
    graphhopper_matrc <- graphhopper_matrc[,c("latravel", "cityregion", "mode5", "g_motorway")]
    
## MERGE IN RTS DATA FROM ROB [NB some areas like Westminster have zero motorway according to RTS, some on graphhopper, as graphhopper counts westway flyover as motorway. so some discrepancy is different definitionns in this way]
  # Merge in data
  rts_matrc <- read_csv(file.path(paste0("01_DataInput/RTS/rts_motorway.csv")))
  graphhopper_matrc <- left_join(graphhopper_matrc, rts_matrc, by = c("mode5", "cityregion"))
  
  # Limit to LA of travel (plausibly 100%)
  print(summary({sel_latravel <- (graphhopper_matrc$latravel %in% latravellist$lad14cd)}))
  graphhopper_matrc <- graphhopper_matrc[sel_latravel,]  
  # lalist_rts <- unique(rts_matrc$latravel)
  # print(summary({sel_rts <- (graphhopper_matrc$latravel %in% lalist_rts)}))
  # graphhopper_matrc <- graphhopper_matrc[sel_rts,]
  # graphhopper_lalist <- unique(graphhopper_matrc$latravel)
  
## GENERATE SCALING ratios
  # SCALE
  graphhopper_matrc$m_infinite <- graphhopper_matrc$rts_motorway==0 | graphhopper_matrc$g_motorway==0
  graphhopper_matrc$motorway <- (graphhopper_matrc$rts_motorway / (1 - graphhopper_matrc$rts_motorway)) / (graphhopper_matrc$g_motorway / (1 - graphhopper_matrc$g_motorway))
  graphhopper_matrc$motorway[graphhopper_matrc$m_infinite==TRUE] <- 1
  graphhopper_matrc$other <- 1 / graphhopper_matrc$motorway

  graphhopper_matrc <- graphhopper_matrc[,c("latravel", "mode5", "cityregion", "motorway", "other")]

  # Reshape & make NA and NAN = 1
  graphhopper_matrc <- reshape2::melt(graphhopper_matrc, id.vars=c("latravel", "mode5", "cityregion"), 
                                      measure.vars=c("motorway", "other" ),variable.name="road_class",value.name="rts_ORscaling")
  graphhopper_matrc$rts_ORscaling[is.na(graphhopper_matrc$rts_ORscaling)] <- 1
  graphhopper_matrc$rts_ORscaling[is.nan(graphhopper_matrc$rts_ORscaling)] <- 1

  # Save 
  write_csv(graphhopper_matrc, file.path(paste0("02_DataCreated/rts_ORscaling/alltrips_ORscaling.csv")))

  
####################
# PART 5: SCALE DISTANCE MATRICES TO DURATION
####################
  motorwayscaledf <- data.frame(
    mode = c(1:5),
    speedscale = c(1,1,4.5, 4.3, 8.0) # numbers from Dropbox\1 - Phys Act_1-PA main\2017_MetaHIT_analysis\2_program\0.2_NTSprepare_191210.do
  )
  for(k in 1:5) {
    mode <- as.numeric(k)
    motorwayscale <- motorwayscaledf$speedscale[motorwayscaledf$mode==mode]
    for (routetype in c("u0d1", "u0d2", "u0d3", "u0d4", "u1d1", "u1d2", "u1d3", "u1d4")) {
    if (file.exists(file.path(paste0("../mh-execute/inputs/travel-matrices/mode", mode, "_", routetype,"_matrc.csv")))) {
      matrc_dist <- read_csv(file.path(paste0("../mh-execute/inputs/travel-matrices/mode", mode, "_", routetype,"_matrc.csv")))
      # Scale down motorway distance
      matrc_dist$motorway2 <- (matrc_dist$motorway / motorwayscale)
      matrc_dist$motorway2 <- matrc_dist$motorway2 / (matrc_dist$motorway2 + matrc_dist$other)
      matrc_dist$motorway <- matrc_dist$motorway2
      matrc_dist$other <- (1 - matrc_dist$motorway)
      # Limit columns and save
      matrc_dist <- matrc_dist[,c("latravel", "motorway", "other")]
      write_csv(matrc_dist, file.path(paste0("../mh-execute/inputs/travel-matrices/mode", mode, "_", routetype,"_matrc_durn.csv")))
    }
    }
  }
  