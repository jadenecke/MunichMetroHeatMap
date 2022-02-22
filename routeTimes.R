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

route_id <-  "1-U2-G-012-1"
routeTrips <- trips[trips$route_id == route_id,]


