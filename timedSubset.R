#### create a data set that starts at a given day and time and goes for a certain period:

timedSubset_createTimedSubset <- function(startTime, duration, weekday){
  
}



service_ids_Monday <- calendar$service_id[calendar$monday == 1]
trips_Monday <- trips[trips$service_id %in% service_ids_Monday, ]

stopTimes_Monday <- stopTimes[stopTimes$trip_id %in% trips_Monday$trip_id, ]
minTime <- strptime("08:00:00", format = "%H:%M:%S")
maxTime <- minTime + 3600
stopTimes_Monday$departure_time_formated <-  strptime(stopTimes_Monday$departure_time, format = "%H:%M:%S")

stopTimes_Monday_8to9 <- stopTimes_Monday[stopTimes_Monday$departure_time_formated >= minTime & stopTimes_Monday$departure_time_formated <= maxTime, ]
