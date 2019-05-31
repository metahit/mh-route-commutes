# Define mode
modename <- inputdf$modename[inputdf$mode== mode]
lines_toroute_mode <- lines_toroute[lines_toroute@data$mode4==mode,]

## ROUTING ##
leglist <- list()
for (i in 1:nrow(lines_toroute_mode)){
  start_point <- coordinates(lines_toroute_mode[i,])[[1]][[1]][1,] %>% as.numeric()
  end_point <- coordinates(lines_toroute_mode[i,])[[1]][[1]][2,] %>% as.numeric()
  
  #print(i)
  leglist[[i]] <- leg_graphhopper(start_point, end_point, homearea = lahome, vehicle = modename , legs = T)
}

# Combine all routes/legs into a single spatialdataframe
legs <- do.call(bind, leglist) 
legs <- spTransform(legs, proj_27700)
#mapview::mapview(legs)
#(summary(legs@data$road_class))
saveRDS(legs, (file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/legs_mode",mode,".Rds"))))
print(paste0("Routing done for home LA ",lahome, " and mode ", mode, " at ",Sys.time()))
