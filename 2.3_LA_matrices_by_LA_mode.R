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

# Add in a bit extra to capture within-LSOA routes
lines_within <- lines[(lines$e_dist_km==0 & lines$home_lad14cd==lahome),]
latravel <- lahome
matla$length[(matla$lahome==lahome & matla$latravel==lahome)] <-  as.numeric(matla$length[(matla$lahome==lahome & matla$latravel==lahome)] 
                                                                                + (as.numeric(sum(lines_within$within_lsoa_dist[lines_within$home_lad14cd==lahome]))))
# Do percentages
matla$plength <- matla$length/sum(matla$length)

#Save
write_csv(matla, file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/matla_mode", mode, ".csv")))
print(paste0("LA matrix done for home LA ",lahome, " and mode ", mode, " at ",Sys.time()))
