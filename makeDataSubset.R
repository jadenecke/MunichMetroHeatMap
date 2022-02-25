routeIDS_Subset <- c("1-U1-G-012-1",
                 "1-U2-G-012-1",
                 "1-U3-G-012-1",
                 "1-U4-G-012-1",
                 "1-U5-G-012-1",
                 "1-U6-G-012-1",
                 "1-U7-G-012-1",
                 "1-U8-G-012-1")


routes_Subset <- routes[routes$route_id %in% routeIDS_Subset, ]
write.csv(routes_Subset, file.path("sbahn_Subset", "routes.txt"))

trips_subset <- trips[trips$route_id %in% routeIDS_Subset, ]
write.csv(trips_subset, file.path("sbahn_Subset", "trips.txt"))

stopTimes_subset <- stopTimes[stopTimes$trip_id %in% trips_subset$trip_id, ]
write.csv(stopTimes_subset, file.path("sbahn_Subset", "stop_times.txt"))

stops_subset <- stops[stops$stop_id %in% stopTimes_subset$stop_id | 
                        stops$stop_id %in% stops_IDtoParentID(stopTimes_subset$stop_id), ]
write.csv(stops_subset, file.path("sbahn_Subset", "stops.txt"))

calendar_subset <- calendar[calendar$service_id %in% trips_subset$service_id, ]
write.csv(calendar_subset, file.path("sbahn_Subset", "calendar.txt"))

calendarDates_subset <- calendar[calendarDates$service_id %in% trips_subset$service_id, ]
write.csv(calendarDates_subset, file.path("sbahn_Subset", "calendar_dates.txt"))