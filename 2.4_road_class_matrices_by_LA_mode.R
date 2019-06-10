legs <- readRDS(file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/legs_mode",mode,".Rds")))

####################
## Make road class matrix
####################
# Recode road class
legs@data$road_classcat <- 3
legs@data$road_classcat[legs@data$road_class %in% c("motorway")] <- 1
legs@data$road_classcat[legs@data$road_class %in% c("trunk", "primary", "motorroad")] <- 2
legs@data$road_classcat[legs@data$road_class %in% c("path", "steps", "forestry")] <- 4 # judge these are out of stats19 scope, although NB path might be in scope if a footway next a road
# https://wiki.openstreetmap.org/wiki/Key:highway
# https://github.com/graphhopper/graphhopper/blob/35b58ddbe8e3aeecd310cf83f637211a6f784093/core/src/main/java/com/graphhopper/routing/profiles/RoadClass.java#L4-L9 

# Create and populate matrix for road class types
matrc <- data.frame("lahome"= character(0), "lahome_weight"= numeric(0), "latravel"= character(0), "road_classcat"=numeric(0), "length"=numeric(0), "urban"=numeric(0), stringsAsFactors=FALSE)
lahome_weight <- as.numeric(mean(legs@data$lahome_weight)) # same value for all routes in same LA by same mode

for(rc in 1:4){
  if(nrow(legs[legs@data$home_lad14cd==lahome & legs@data$road_classcat==rc,])==0) {
  } else {
    for(i in 1:length(latravellist$lad14cd)){
      latravel <- as.character(latravellist$lad14cd[i])
      if(is.null(intersect(legs[legs@data$road_classcat==rc,], lad14shape[lad14shape@data$lad15cd==latravel,]))) {
      } else {
        matrc[nrow(matrc) + 1,] = list(lahome,lahome_weight,latravel,rc,NA,NA)
        matrc$length[(matrc$lahome==lahome & matrc$latravel==latravel & matrc$road_classcat==rc)] <- (lineLength(intersect(legs[legs@data$road_classcat==rc,], lad14shape[lad14shape@data$lad15cd==latravel,]), byid = FALSE)) / 1000
      }
      if(is.null(intersect(legs[legs@data$road_classcat==rc,], lad14builtup[lad14builtup@data$lad15cd==latravel,]))) {
        matrc$urban[(matrc$lahome==lahome & matrc$latravel==latravel & matrc$road_classcat==rc)]  <- 0
      } else {
        matrc$urban[(matrc$lahome==lahome & matrc$latravel==latravel & matrc$road_classcat==rc)]  <- (lineLength(intersect(legs[legs@data$road_classcat==rc,], lad14builtup[lad14builtup@data$lad15cd==latravel,]), byid = FALSE)) / 1000
      }      
    }
  }
}

# # Rejig to divide by type
matrc$rural <- matrc$length - matrc$urban
matrc$length <- NULL
matrc <- reshape2::melt(matrc, id.vars=c("lahome", "lahome_weight", "latravel", "road_classcat"), 
                         measure.vars=c("urban", "rural" ),variable.name="urban_rural",value.name="length")

#Save
write_csv(matrc, file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/matrc_mode", mode, ".csv")))
print(paste0("Road class matrix done for home LA ",lahome, " and mode ", mode, " at ",Sys.time()))
