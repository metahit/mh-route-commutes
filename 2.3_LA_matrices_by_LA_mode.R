legs <- readRDS(file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/legs_mode",mode,".Rds")))

####################
## Make LA matrix##
####################
# Create and populate matrix for between-LA travel
matla <- data.frame("lahome"= character(0), "latravel"=character(0),"length"=numeric(0),stringsAsFactors=FALSE)
for(i in 1:length(latravellist$lad14cd)){
  latravel <- as.character(latravellist$lad14cd[i])
  if(is.null(intersect(legs, lad14shape[lad14shape@data$lad15cd==latravel,]))) {
  } else {
    matla[nrow(matla) + 1,] = list(lahome,latravel,NA)
    matla$length[(matla$lahome==lahome & matla$latravel==latravel)] <- (lineLength(intersect(legs, lad14shape[lad14shape@data$lad15cd==latravel,]), byid = FALSE)) / 1000
  }
}

# Do percentages
matla$plength <- matla$length/sum(matla$length)

#Save
write_csv(matla, file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/matla_mode", mode, ".csv")))
print(paste0("LA matrix done for home LA ",lahome, " and mode ", mode, " at ",Sys.time()))
