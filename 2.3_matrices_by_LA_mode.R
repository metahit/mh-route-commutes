legs <- readRDS(file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/legs_mode",mode,".Rds")))

####################
## Make LA matrix##
####################
# Create and populate matrix for between-LA travel
matla <- data.frame("lahome"= character(0), "latravel"=character(0),"length"=numeric(0),"plength"=numeric(0),stringsAsFactors=FALSE)
for(i in 1:length(latravellist$lad14cd)){
  latravel <- as.character(latravellist$lad14cd[i])
  if(is.null(intersect(legs[legs@data$home_lad14cd==lahome,], lad14shape[lad14shape@data$lad15cd==latravel,]))) {
  } else {
    matla[nrow(matla) + 1,] = list(lahome,latravel,NA,NA)
    matla$length[(matla$lahome==lahome & matla$latravel==latravel)] <- (lineLength(intersect(legs[legs@data$home_lad14cd==lahome,], lad14shape[lad14shape@data$lad15cd==latravel,]), byid = FALSE)) / 1000
  }
}

# Add in a bit extra to capture within-LSOA routes
lines_within <- lines[(lines$e_dist_km==0 & lines$home_lad14cd==lahome),]
latravel <- lahome
matla$length[(matla$lahome==lahome & matla$latravel==lahome)] <-  as.numeric(matla$length[(matla$lahome==lahome & matla$latravel==lahome)] 
                                                                                + (as.numeric(sum(lines_within$within_lsoa_dist[lines_within$home_lad14cd==lahome]))))
# Do percentages
matla$plength <- matla$length/sum(matla$length)

#Save
write_csv(matla, file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/matla_mode", mode, ".csv")))


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
matrc1 <- data.frame("lahome"= character(0), "road_classcat"=numeric(0), "length"=numeric(0), "length_bua"=numeric(0), stringsAsFactors=FALSE)
for(i in 1:4){
  if(nrow(legs[legs@data$home_lad14cd==lahome & legs@data$road_classcat==i,])==0) {
#    matrc1$length[(matrc1$lahome==lahome & matrc1$road_classcat==i)] <- 0
#    matrc1$length_bua[(matrc1$lahome==lahome & matrc1$road_classcat==i)] <- 0
  } else {
    matrc1[nrow(matrc1) + 1,] = list(lahome,i,NA,NA)
    matrc1$length[(matrc1$lahome==lahome & matrc1$road_classcat==i)] <- (lineLength(legs[legs@data$home_lad14cd==lahome & legs@data$road_classcat==i,])) / 1000
    if(is.null(intersect(legs[legs@data$home_lad14cd==lahome & legs@data$road_classcat==i,], builtup))) {
      matrc1$length_bua[(matrc1$lahome==lahome & matrc1$road_classcat==i)] <- 0
    } else {
      matrc1$length_bua[(matrc1$lahome==lahome & matrc1$road_classcat==i)] <- (lineLength(intersect(legs[legs@data$home_lad14cd==lahome & legs@data$road_classcat==i,], builtup), byid = FALSE)) / 1000
    }
  }
}

# Add in a bit extra to capture within-LSOA routes - assume all on non-motorway, non-primary [?ok]
matrc1$length[(matrc1$lahome==lahome & matrc1$road_classcat==3)] <-  as.numeric(matrc1$length[(matrc1$lahome==lahome & matrc1$road_classcat==3)] 
                                                                                + (as.numeric(sum(lines_within$within_lsoa_dist[lines_within$home_lad14cd==lahome]))))

# Add in a bit extra to capture URBAN within-LSOA routes
lines_within_urban <- lines[(lines$e_dist_km==0 & lines$home_lad14cd==lahome & lines$urban_lsoa==1),]
matrc1$length_bua[(matrc1$lahome==lahome & matrc1$road_classcat==3)] <-  as.numeric(matrc1$length_bua[(matrc1$lahome==lahome & matrc1$road_classcat==3)] 
                                                                                    + (as.numeric(sum(lines_within_urban$within_lsoa_dist[lines_within_urban$home_lad14cd==lahome]))))

# Rejig to divide by type
matrc1$length_rural <- matrc1$length - matrc1$length_bua
matrc2 <- data.frame("lahome"= c(rep(lahome,6)),
                     "road_class"=c("motorway", "urban_primary", "rural_primary", "urban_other", "rural_other", "out_of_scope"),
                     "length"=as.numeric(c(rep(0,6))),
                     "plength"=as.numeric(c(rep(0,6)))
                     )

if(nrow(matrc1[matrc1$road_classcat==1,])==1){
  matrc2$length[matrc2$road_class=="motorway"] <- matrc1$length[matrc1$road_classcat==1]
} 
if(nrow(matrc1[matrc1$road_classcat==2,])==1){
  matrc2$length[matrc2$road_class=="urban_primary"] <- matrc1$length_bua[matrc1$road_classcat==2]
  matrc2$length[matrc2$road_class=="rural_primary"] <- matrc1$length_rural[matrc1$road_classcat==2]
}
if(nrow(matrc1[matrc1$road_classcat==3,])==1){
  matrc2$length[matrc2$road_class=="urban_other"] <- matrc1$length_bua[matrc1$road_classcat==3]
  matrc2$length[matrc2$road_class=="rural_other"] <- matrc1$length_rural[matrc1$road_classcat==3]
}
if(nrow(matrc1[matrc1$road_classcat==4,])==1){
  matrc2$length[matrc2$road_class=="out_of_scope"] <- matrc1$length[matrc1$road_classcat==4]
} 

# Do percentages
matrc2$plength <- matrc2$length/sum(matrc2$length)

#Save
write_csv(matrc2, file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/matrc_mode", mode, ".csv")))
print(paste0("Matrices done for home LA ",lahome, " and mode ", mode, " at ",Sys.time()))
