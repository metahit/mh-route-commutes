####################
## Make road class matrix
####################
for (routetype in routetypelist) {
  legsuse <- legs[legs@data$routetype == routetype,]
  nroute <- nrow(legsuse@data[legsuse@data$start==0,])
  laworklist <- unique(legs@data$work_lad14cd) # assume any significant LA of travel has at least one person working there
  
  if (nroute==0) {
    print(paste0("Empty for ", routetype))
    matrc <- data.frame("lahome"= character(0), "lahome_weight"= numeric(0), "latravel"= character(0), "road_classcat"=numeric(0), "urban_rural"= character(0), "length"=numeric(0), stringsAsFactors=FALSE)
    matrc[nrow(matrc) + 1,] = list(lahome,0,"x",0,"x",0)
    
  } else {
    print(paste0("RC mat for ", routetype))
    matrc <- data.frame("lahome"= character(0), "lahome_weight"= numeric(0), "latravel"= character(0), "road_classcat"=numeric(0), "length"=numeric(0), "urban"=numeric(0), stringsAsFactors=FALSE)
    lahome_weight <- as.numeric(mean(legsuse@data$lahome_weight)) # same value for all routes in same LA by same mode
    for(rc in 1:4){
      if(nrow(legsuse[legsuse@data$home_lad14cd==lahome & legsuse@data$road_classcat==rc,])==0) {
      } else {
        for(i in 1:length(laworklist)){
          latravel <- as.character(laworklist[i])
          matrc[nrow(matrc) + 1,] = list(lahome,lahome_weight,latravel, rc,NA,NA)
          if(is.null(intersect(legsuse[legsuse@data$road_classcat==rc,], lad14shape[lad14shape@data$lad15cd==latravel,]))) {
            matrc$length[(matrc$lahome==lahome & matrc$latravel==latravel & matrc$road_classcat==rc)] <- 0
          } else {
            matrc$length[(matrc$lahome==lahome & matrc$latravel==latravel & matrc$road_classcat==rc)] <- (lineLength(intersect(legsuse[legsuse@data$road_classcat==rc,], lad14shape[lad14shape@data$lad15cd==latravel,]), byid = FALSE)) / 1000
          }
          if(is.null(intersect(legsuse[legsuse@data$road_classcat==rc,], lad14builtup[lad14builtup@data$lad15cd==latravel,]))) {
            matrc$urban[(matrc$lahome==lahome & matrc$latravel==latravel & matrc$road_classcat==rc)]  <- 0
          } else {
            matrc$urban[(matrc$lahome==lahome & matrc$latravel==latravel & matrc$road_classcat==rc)]  <- (lineLength(intersect(legsuse[legsuse@data$road_classcat==rc,], lad14builtup[lad14builtup@data$lad15cd==latravel,]), byid = FALSE)) / 1000
          }      
        }
      }
    }
    # Remove if length is zero
    matrc <- matrc[matrc$length!=0,]
    
    # # Rejig to divide by type
    matrc$rural <- matrc$length - matrc$urban
    matrc$length <- NULL
    matrc <- reshape2::melt(matrc, id.vars=c("lahome", "lahome_weight", "latravel", "road_classcat"), 
                             measure.vars=c("urban", "rural" ),variable.name="urban_rural",value.name="length")
  }
    #Save
    write_csv(matrc, file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/matrc_mode", mode, "_", routetype, ".csv")))
}
print(paste0("Road class matrices done for home LA ",lahome, " and mode ", mode, " at ",Sys.time()))
