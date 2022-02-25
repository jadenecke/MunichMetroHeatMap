sdb_stops <- read.csv(file.path("sdb", "stops_modded_full.txt"))
sdb_stopIDsToInclude <- sdb_stops$stop_id[!sdb_stops$largeDistance]

#make id transform function
sdbClean_EE <- new.env(hash = TRUE)
list2env(setNames(as.list(sdb_stops$stop_id_guess), sdb_stops$stop_id),
         envir = sdbClean_EE)
sdbClean_IDtoCorID <- function(stop_id){
  return(sdbClean_EE[[stop_id]])
}
sdbClean_IDtoCorID <- Vectorize(sdbClean_IDtoCorID)


sdb_stopTimes <- read.csv(file.path("sdb", "stop_times.txt"))
sdb_stopTimes <- sdb_stopTimes[sdb_stopTimes$stop_id %in% sdb_stopIDsToInclude, ]
sdb_stopTimes$stop_id <- sdbClean_IDtoCorID(as.character(sdb_stopTimes$stop_id))
write.csv(sdb_stopTimes, file.path("sdb", "stop_times_modded.txt"))
sdb_tripsToInclude <- unique(sdb_stopTimes$trip_id)


sdb_trips <- read.csv(file.path("sdb", "trips.txt"))
sdb_trips <- sdb_trips[sdb_trips$trip_id %in% sdb_tripsToInclude, ]
write.csv(sdb_trips, file.path("sdb", "trips_modded.txt"))
sdb_routesToInclude <- unique(sdb_trips$route_id)
sdb_servicesToInclude <- unique(sdb_trips$service_id)


sdb_routes <- read.csv(file.path("sdb", "routes.txt"))
sdb_routes <- sdb_routes[sdb_routes$route_id %in% sdb_routesToInclude, ]
write.csv(sdb_routes, file.path("sdb", "routes_modded.txt"))

#carefull with calendar_dates, data is not complete!
sdb_calendarDates <- read.csv(file.path("sdb", "calendar_dates.txt"))
sdb_calendarDates <- sdb_calendarDates[sdb_calendarDates$service_id %in% sdb_servicesToInclude, ]
write.csv(sdb_calendarDates, file.path("sdb", "calendar_dates_modded.txt"))


sdb_calendar <- read.csv(file.path("sdb", "calendar.txt"))
sdb_calendar <- sdb_calendar[sdb_calendar$service_id %in% sdb_servicesToInclude, ]
write.csv(sdb_routes, file.path("sdb", "calendar_modded.txt"))