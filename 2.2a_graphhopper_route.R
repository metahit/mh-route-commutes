## ROUTING ##
leglist <- list()
for (i in 1:nrow(lines_toroute_mode)){
  start_point <- coordinates(lines_toroute_mode[i,])[[1]][[1]][1,] %>% as.numeric()
  end_point <- coordinates(lines_toroute_mode[i,])[[1]][[1]][2,] %>% as.numeric()
  routeid <- as.character(lines_toroute_mode@data$id[i])

  if (i %in% c(500,1000,5000)) {
      print(i)
  }
  leglist[[i]] <- leg_graphhopper(start_point, end_point,  routeid = routeid, vehicle = modename , legs = T)
}
saveRDS(leglist, (file.path(paste0("02_DataCreated/temp/graphhopper_leglists_1907nat/",lahome,"_mode",mode,".Rds"))), version = 2)
print(paste0("Routing part A done for home LA ",lahome, " (",j,") and mode ", mode, " at ",Sys.time()))