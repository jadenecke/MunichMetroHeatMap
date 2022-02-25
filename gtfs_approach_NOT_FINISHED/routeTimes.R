#read and combine data:
mvv_routes <- read.csv(file.path("mvv", "routes.txt"))
mvg_routes <- read.csv(file.path("mvg", "routes.txt"))
mvv_routes$source <- "mvv"
mvg_routes$source <- "mvg"
routes_bothNames <- intersect(names(mvv_routes), names(mvg_routes))
routes <- rbind.data.frame(mvv_routes[, routes_bothNames],
                          mvg_routes[, routes_bothNames])
rm("mvv_routes", "mvg_routes")

mvv_trips <- read.csv(file.path("mvv", "trips.txt"))
mvg_trips <- read.csv(file.path("mvg", "trips.txt"))
mvv_trips$source <- "mvv"
mvg_trips$source <- "mvg"
trips_bothNames <- intersect(names(mvv_trips), names(mvg_trips))
trips <- rbind.data.frame(mvv_trips[, trips_bothNames],
                           mvg_trips[, trips_bothNames])
rm("mvv_trips", "mvg_trips")

mvv_stopTimes <- read.csv(file.path("mvv", "stop_times.txt"))
mvg_stopTimes <- read.csv(file.path("mvg", "stop_times.txt"))
mvv_stopTimes$source <- "mvv"
mvg_stopTimes$source <- "mvg"
stopTimes_bothNames <- intersect(names(mvv_stopTimes), names(mvg_stopTimes))
stopTimes <- rbind.data.frame(mvv_stopTimes[, stopTimes_bothNames],
                              mvg_stopTimes[, stopTimes_bothNames])
rm("mvv_stopTimes", "mvg_stopTimes")

routeTimes_getStopsInRoute <- function(route_ids){
  trip_ids <- trips$trip_id[trips$route_id %in% route_ids]
  stop_ids <- unique(stopTimes$stop_id[stopTimes$trip_id %in% trip_ids])
  return(unique(stops_IDtoParentID(stop_ids)))
}


routeTimes_getConnectedStops <- function(stop_id){
  tripIDS <- stopTimes$trip_id[stops_IDtoParentID(stopTimes$stop_id) == stops_IDtoParentID(stop_id)]
  tripIDS <- unique(tripIDS)
  #uniqueRoutes <- unique(trips$route_id[trips$trip_id %in% trip_ids])
  connectedStops <- list()
  for(tripID in tripIDS){
    stopEvent <- stopTimes[stopTimes$trip_id == tripID & stopTimes$stop_id == stop_id, ]
    stopTimes_workingSet <- stopTimes[stopTimes$trip_id == tripID,]
    
  }
}




###########################
sdb_stopTimes <- read.csv(file.path("sdb", "stop_times.txt"))
