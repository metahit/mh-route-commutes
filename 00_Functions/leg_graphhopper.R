# Copy of stplanr's route_graphhopper function
# Modified it to add road_class and return the values by leg, using the info from the API
# https://graphhopper.com/api/1/route?point=49.932707,11.588051&point=50.3404,11.64705&vehicle=car&debug=true&type=json&details=road_class&[YOUR-KEY]

leg_graphhopper <- function(from, to, l = NULL, vehicle = "bike", homearea = NULL, routeid = NULL, weight = NULL, silent = TRUE, pat = NULL, base_url = "https://graphhopper.com", legs = F  ) {
  
  # Convert character strings to lon/lat if needs be
  coords <- od_coords(from, to, l)
  
  if (is.null(pat)) {
    pat <- api_pat("graphhopper")
  }
  
  httrmsg <- httr::modify_url(
    base_url,
    path = "/api/1/route",
    query = list(
      point = paste0(coords[1, c("fy", "fx")], collapse = ","),
      point = paste0(coords[1, c("ty", "tx")], collapse = ","),
      vehicle = vehicle,
      details = "road_class", 
      #details = "road_environment",
      locale = "en-US",
      debug = "true",
      points_encoded = "false",
      key = pat
    )
  )
  if (silent == FALSE) {
    print(paste0("The request sent was: ", httrmsg))
  }
  httrreq <- httr::GET(httrmsg)
  httr::stop_for_status(httrreq)
  
  if (grepl("application/json", httrreq$headers$`content-type`) == FALSE) {
    stop("Error: Graphhopper did not return a valid result")
  }
  
  txt <- httr::content(httrreq, as = "text", encoding = "UTF-8")
  if (txt == "") {
    stop("Error: Graphhopper did not return a valid result")
  }
  
  obj <- jsonlite::fromJSON(txt)
  
  if (is.element("message", names(obj))) {
    if (grepl("Wrong credentials", obj$message) == TRUE) {
      stop("Invalid API key")
    }
  }
  
  if(legs == F ) {
    route <- sp::SpatialLines(list(sp::Lines(list(sp::Line(obj$paths$points[[2]][[1]][, 1:2])), ID = "1")))
    
    climb <- NA # to set elev variable up
    
    # get elevation data if it was a bike trip
    if (vehicle == "bike") {
      change_elev <- obj$path$descend + obj$paths$ascend
    } else {
      change_elev <- NA
    }
    
    # Attribute data for the route
    df <- data.frame(
      home_lad14cd = homearea,
      routeid = routeid,
      lahome_weight = weight,
      time = obj$paths$time / (1000 * 60),
      dist = obj$paths$distance,
      change_elev = change_elev
    )

    route <- sp::SpatialLinesDataFrame(route, df)
    
  } else {
    # CREATE SEPARATE ENTRY FOR EACH LEG
    # Create data frame of road class information
    numlegs <- as.numeric(nrow(obj$paths$details$road_class[[1]]))
    detailsdf <- data.frame(
      legid = (1:numlegs),
      routeid = routeid,
      lahome_weight = weight,
      home_lad14cd = homearea,
      start = as.numeric(obj$paths$details$road_class[[1]][,1]), 
      end = as.numeric(obj$paths$details$road_class[[1]][,2]), 
      road_class = obj$paths$details$road_class[[1]][,3]
    )
    
    # Extract the list of coordinates corresponding to each leg that has a road class
    l <- list()
    for(legno in (1:numlegs)){
      legstart <- (detailsdf$start[detailsdf$legid==legno])+1 # plus 1 as graphhopper starts with 0
      legend <- (detailsdf$end[detailsdf$legid==legno])+1
      l[[legno]] <- sp::Lines(list(sp::Line(obj$paths$points[[2]][[1]][legstart:legend, 1:2])), ID=legno)
    }
    
    # Turn into a spatial object
    route <- sp::SpatialLines(l)
    route <- sp::SpatialLinesDataFrame(route, detailsdf)
    
    # # reproject to easting/northing and calculate distance of each leg
    # sp::proj4string(route) <- sp::CRS("+proj=longlat +init=epsg:4326")
    # route <- spTransform(route, proj_27700)
    # route@data$length <-lineLength(route, byid = TRUE)
  }
 
  sp::proj4string(route) <- sp::CRS("+proj=longlat +init=epsg:4326")
  return(route)
  
}