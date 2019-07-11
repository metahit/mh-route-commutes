# Combine all routes/legs into a single spatialdataframe
leglist <- readRDS(file.path(paste0("02_DataCreated/temp/graphhopper_leglists_1907nat/",lahome,"_mode",mode,".Rds")))
legs <- do.call(bind, leglist) 
legs@data <- left_join(legs@data,lines_toroute_mode_vars, by = c("routeid" = "id")) # merge in selected route variables
legs <- legs[legs@data$legid != 0,] # remove routes where start & end same place
legs <- spTransform(legs, proj_27700)
#mapview::mapview(legs)
#(summary(legs@data$road_class))
saveRDS(legs, (file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/legs_mode",mode,".Rds"))), version = 2)
print(paste0("Routing done for home LA ",lahome, " and mode ", mode, " at ",Sys.time()))