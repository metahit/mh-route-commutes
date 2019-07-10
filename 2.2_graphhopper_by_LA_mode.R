# Define mode
modename <- inputdf$modename[inputdf$mode== mode]
lines_toroute_mode <- lines_toroute[lines_toroute@data$mode4==mode,]

## ROUTING ##
leglist <- list()
for (i in 1:nrow(lines_toroute_mode)){
  start_point <- coordinates(lines_toroute_mode[i,])[[1]][[1]][1,] %>% as.numeric()
  end_point <- coordinates(lines_toroute_mode[i,])[[1]][[1]][2,] %>% as.numeric()
  routeid <- as.character(lines_toroute_mode@data$id[i])
  lawork <- as.character(lines_toroute_mode@data$work_lad14cd[i])
  urbanmatch <- as.character(lines_toroute_mode@data$urbanmatch[i])
  lahome_weight <- as.numeric(lines_toroute_mode@data$lahome_weight[i])

    if (i %in% c(1,1000,2000,3000,4000,5000)) {
      print(i)
  }

  leglist[[i]] <- leg_graphhopper(start_point, end_point, homearea = lahome, workarea = lawork, home_urbanmatch = urbanmatch, routeid = routeid, 
                                  weight = lahome_weight, vehicle = modename , legs = T)
}

# Combine all routes/legs into a single spatialdataframe
legs <- do.call(bind, leglist) 
legs <- legs[legs@data$legid != 0,] # remove routes where start & end same place
legs <- spTransform(legs, proj_27700)
#mapview::mapview(legs)
#(summary(legs@data$road_class))
saveRDS(legs, (file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/legs_mode",mode,".Rds"))), version = 2)
print(paste0("Routing done for home LA ",lahome, " and mode ", mode, " at ",Sys.time()))
