### Network creation
library(igraph)

trips <- read.csv(file.path("sbahn_Subset", "trips.txt"))
stopTimes <- read.csv(file.path("sbahn_Subset", "stop_times.txt"))
calendarDates <- read.csv(file.path("sbahn_Subset", "calendar_dates.txt"))
stops <- read.csv(file.path("sbahn_Subset", "stops.txt"))
calendar <- read.csv(file.path("sbahn_Subset", "calendar.txt"))
routes <- read.csv(file.path("sbahn_Subset", "routes.txt"))


################ TEST only for subway ################
route_id_U <- c("1-U1-G-012-1",
                "1-U2-G-012-1",
                "1-U3-G-012-1",
                "1-U4-G-012-1",
                "1-U5-G-012-1",
                "1-U6-G-012-1",
                "1-U7-G-012-1",
                "1-U8-G-012-1")
# stopsForVertecies <- split(getStopsForNetwork(), seq(nrow(getStopsForNetwork())))
stopIds_U <- getStopsInRoute(route_id_U)
stopsForVertecies_U <- as.list(stops_getStopsDataForNetwork(stopIds_U))
#names(stopsForVertecies_U) <- unlist(lapply(stopsForVertecies_U, \(l){l[[1]][1]}))
metroGraph <- make_empty_graph()
metroGraph <- add_vertices(metroGraph,
                           nv = length(stopIds_U),
                           attr = stopsForVertecies_U)

plot(metroGraph,
     vertex.label = vertex_attr(metroGraph, "stop_name"),
     vertex.label.cex = .8,
     vertex.size = 5,
     layout = cbind(vertex_attr(metroGraph, "stop_lon"),
                    vertex_attr(metroGraph, "stop_lat")))

