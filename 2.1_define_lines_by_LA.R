####################
# PART 1: MAKE LINES SPATIAL (by LA)
####################

# Subset census lines to be routed
lines <- lines_all[lines_all$home_lad14cd == lahome,]

# Load geo data, merge centroids, calc distance
match1 <- match(lines$geo_code_o, cents_all$lsoa11cd) # generates a number - where in cents_all is found each $home in lines
match2 <- match(lines$geo_code_d, cents_all$lsoa11cd)
lines <- lines[!is.na(match1) & !is.na(match2),] # remove line outside the required build region, or no geographical origin/dest
coords1 <- cents_all@coords[match(lines$geo_code_o, cents_all$lsoa11cd),] # gets the coords from 'match1' position of cents
coords2 <- cents_all@coords[match(lines$geo_code_d, cents_all$lsoa11cd),]
lines$e_dist_km <- geosphere::distHaversine(p1 = coords1, p2 = coords2) / 1000 # assign euclidean dist
if(!dir.exists(file.path(paste0("02_DataCreated/temp_matrix/",lahome)))) { dir.create(file.path(paste0("02_DataCreated/temp_matrix/",lahome))) }
saveRDS(lines, (file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/lines.Rds"))))

# Restrict to selected between-zone lines
lines_toroute_data <- lines[(lines$e_dist_km < lines$maxdist_mode) & !is.na(lines$e_dist_km) & lines$e_dist_km!=0,]
lines_toroute_data <- lines_toroute_data[,c("geo_code_o", "geo_code_d", "id", "home_lad14cd", "mode4", "lahome_weight", "e_dist_km")]
lines_toroute_data <- lines_toroute_data[order(lines_toroute_data$id),]

# Make a spatial object of lines
lines_toroute_lines <- od2line(flow = lines_toroute_data, zones = cents_all, destinations = cents_all)
rownames(lines_toroute_data) <- sapply(1:length(lines_toroute_lines), function(j) lines_toroute_lines@lines[[j]]@ID) # FORCE DATA ROW NAMES TO BE SAME AS ID IN LINES (in case don't start from '1')
lines_toroute <- SpatialLinesDataFrame(sl = lines_toroute_lines, data = lines_toroute_data)
lines_toroute <- spTransform(lines_toroute, proj_4326)
saveRDS(lines_toroute, (file.path(paste0("02_DataCreated/temp_matrix/",lahome,"/lines_toroute.Rds"))))
print(paste0("Lines saved for home LA ",lahome, " at ",Sys.time()))
