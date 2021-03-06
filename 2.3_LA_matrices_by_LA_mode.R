####################
## Make LA matrix##
####################
# Create and populate matrix for between-LA travel
for (routetype in routetypelist) {
  legsuse <- legs[legs@data$routetype == routetype,]
  nroute <- nrow(legsuse@data[legsuse@data$start==0,])
  laworklist <- unique(legs@data$work_lad14cd) # assume any significant LA of travel has at least one person working there

  matla <- data.frame("lahome"= character(0), "latravel"=character(0),"nroute"=numeric(0), "length"=numeric(0), "plength"=numeric(0),stringsAsFactors=FALSE)
  
  if (nroute==0) {
    matla[nrow(matla) + 1,] = list(lahome,"x", nroute ,0, 0)
    print(paste0("Empty for ", routetype))
    
  } else {
    print(paste0("LA mat for ", routetype))
    for(i in 1:length(laworklist)){
      latravel <- as.character(laworklist[i])
      matla[nrow(matla) + 1,] = list(lahome,latravel, nroute ,NA)
      if(is.null(intersect(legsuse, lad14shape[lad14shape@data$lad15cd==latravel,]))) {
        matla$length[(matla$lahome==lahome & matla$latravel==latravel)] <- 0
      } else {
         matla$length[(matla$lahome==lahome & matla$latravel==latravel)] <- (lineLength(intersect(legsuse, lad14shape[lad14shape@data$lad15cd==latravel,]), byid = FALSE)) / 1000
      }
    }
    # Remove if length is zero
    matla <- matla[matla$length!=0,]
    # Do percentages
    matla$plength <- matla$length/sum(matla$length)
  }
  
    #Save
    write_csv(matla, file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/matla_mode", mode, "_", routetype, ".csv")))
}
print(paste0("LA matrices done for home LA ",lahome, " (",j,") and mode ", mode, " at ",Sys.time()))
