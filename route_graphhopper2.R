# Copy of stplanr's route_graphhopper function
# Modified it to add route_class and route_environment
# Also allow to return JSON obj with all the info from the API

route_graphhopper2 <- function(from, to, l = NULL, vehicle = "bike", silent = TRUE, pat = NULL, base_url = "https://graphhopper.com",
                               return_JSON_obj = FALSE) {
  
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
    time = obj$paths$time / (1000 * 60),
    dist = obj$paths$distance,
    change_elev = change_elev
  )
  
  route <- sp::SpatialLinesDataFrame(route, df)
  sp::proj4string(route) <- sp::CRS("+proj=longlat +init=epsg:4326")
  
  if(return_JSON_obj)
    return (list(route, obj))
  else
    return(route)
  
}