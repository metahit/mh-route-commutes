rm(list = ls())

#legs <- readRDS("02_DataCreated/national_data_test/legs_mode1.Rds")  # CYCLING
legs <- readRDS("02_DataCreated/national_data_test/legs_mode2.Rds")  # WALKING

(summary(legs@data$road_class))
legs@data$legdist <- (SDraw::lineLength(legs, byid = TRUE)) / 1000

rc <- "path"  # SET ROAD CLASS OF INTEREST!
legs <- legs[legs@data$road_class==rc,]

legs1 <- legs[legs@data$legdist>0.5 & legs@data$legdist<=1 ,]  ## SET DISTANCE OF INTEREST!
mapview::mapview(legs1)
