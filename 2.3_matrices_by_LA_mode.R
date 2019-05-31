legs <- readRDS(file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/legs_mode",mode,".Rds")))

####################
## Make LA matrix##
####################
# Create and populate matrix for between-LA travel
la_mat <- data.frame("lahome"= character(0), "latravel"=character(0), "length"=numeric(0),stringsAsFactors=FALSE)
for(i in 1:length(latravellist$lad14cd)){
  latravel <- as.character(latravellist$lad14cd[i])
  if(is.null(intersect(legs[legs@data$home_lad14cd==lahome,], lad14shape[lad14shape@data$lad15cd==latravel,]))) {
  } else {
    la_mat[nrow(la_mat) + 1,] = list(lahome,latravel,NA)
    la_mat$length[(la_mat$lahome==lahome & la_mat$latravel==latravel)] <- lineLength(intersect(legs[legs@data$home_lad14cd==lahome,], lad14shape[lad14shape@data$lad15cd==latravel,]), byid = FALSE)
  }
}

# Add in a bit extra to capture within-LSOA routes
lines_within <- lines[(lines$e_dist_km==0 & lines$home_lad14cd==lahome),]
latravel <- lahome
la_mat$length[(la_mat$lahome==lahome & la_mat$latravel==lahome)] <-  as.numeric(la_mat$length[(la_mat$lahome==lahome & la_mat$latravel==lahome)] 
                                                                                + (lsoa_within_dist*as.numeric(sum(lines_within$home_lad14cd==lahome))))

#Save
write_csv(la_mat, file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/matla_mode", mode, ".csv")))


####################
## Make road class matrix
####################
# Recode road class
legs@data$road_classcat <- 3
legs@data$road_classcat[legs@data$road_class %in% c("motorway")] <- 1
legs@data$road_classcat[legs@data$road_class %in% c("trunk", "primary", "motorroad")] <- 2
legs@data$road_classcat[legs@data$road_class %in% c("path", "steps", "forestry")] <- 4 # judge these are out of stats19 scope
# https://wiki.openstreetmap.org/wiki/Key:highway
# https://github.com/graphhopper/graphhopper/blob/35b58ddbe8e3aeecd310cf83f637211a6f784093/core/src/main/java/com/graphhopper/routing/profiles/RoadClass.java#L4-L9 

# Create and populate matrix for road class types
rc_mat <- data.frame("lahome"= character(0), "road_classcat"=numeric(0), "length"=numeric(0), "length_bua"=numeric(0), stringsAsFactors=FALSE)
for(i in 1:4){
  if(nrow(legs[legs@data$home_lad14cd==lahome & legs@data$road_classcat==i,])==0) {
  } else {
    rc_mat[nrow(rc_mat) + 1,] = list(lahome,i,NA,NA)
    rc_mat$length[(rc_mat$lahome==lahome & rc_mat$road_classcat==i)] <- lineLength(legs[legs@data$home_lad14cd==lahome & legs@data$road_classcat==i,])
    if(is.null(intersect(legs[legs@data$home_lad14cd==lahome & legs@data$road_classcat==i,], builtup))) {
      rc_mat$length_bua[(rc_mat$lahome==lahome & rc_mat$road_classcat==i)] <- 0
    } else {
      rc_mat$length_bua[(rc_mat$lahome==lahome & rc_mat$road_classcat==i)] <- lineLength(intersect(legs[legs@data$home_lad14cd==lahome & legs@data$road_classcat==i,], builtup), byid = FALSE)
    }
  }
}

# Add in a bit extra to capture within-LSOA routes - assume all on non-motorway, non-primary
rc_mat$length[(rc_mat$lahome==lahome & rc_mat$road_classcat==3)] <-  as.numeric(rc_mat$length[(rc_mat$lahome==lahome & rc_mat$road_classcat==3)] 
                                                                                + (lsoa_within_dist*as.numeric(sum(lines_within$home_lad14cd==lahome))))

# Add in a bit extra to capture URBAN within-LSOA routes
lines_within_urban <- lines[(lines$e_dist_km==0 & lines$home_lad14cd==lahome & lines$urban_lsoa==1),]
rc_mat$length_bua[(rc_mat$lahome==lahome & rc_mat$road_classcat==3)] <-  as.numeric(rc_mat$length_bua[(rc_mat$lahome==lahome & rc_mat$road_classcat==3)] 
                                                                                + (lsoa_within_dist*as.numeric(sum(lines_within_urban$home_lad14cd==lahome))))

#Save
write_csv(rc_mat, file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/matrc_mode", mode, ".csv")))
print(paste0("Matrices done for home LA ",lahome, " and mode ", mode, " at ",Sys.time()))
