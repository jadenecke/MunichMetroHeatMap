#combine mvv & mvg data
mvv_stops <- read.csv(file.path("mvv", "stops.txt"))
mvg_stops <- read.csv(file.path("mvg", "stops.txt"))
mvv_stops$source <- "mvv"
mvg_stops$source <- "mvg"
stops_bothNames <- intersect(names(mvv_stops), names(mvg_stops))
stops <- rbind.data.frame(mvv_stops[, stops_bothNames], mvg_stops[, stops_bothNames])
rm("mvv_stops", "mvg_stops")

#make hash map for parent Stations:
stops_redirectEnv <- new.env(hash = TRUE)

stop_id_parent <- lapply(stops$stop_id, \(id){
  el <- strsplit(id, ":")
  return(paste0(el[[1]][1:3], collapse = ":"))
})

list2env(setNames(stop_id_parent, stops$stop_id),
         envir = stops_redirectEnv)


#query function:
stops_getParentStop <- function(id, env = stops_redirectEnv){
  return(stops_redirectEnv[[id]])
}





#match s-bahn stops
sdb_stops <- read.csv(file.path("sdb", "stops.txt"))


findNearestStop <- function(sourceLat, sourceLong, targetsLat, targetsLong, targetsNames){
  #check lengths to confirm format:
  if(length(sourceLat) != 1) stop("sourceLat not singular")
  if(length(sourceLong) != 1) stop("sourceLong not singular")
  if(length(targetsLat) <= 1) stop("targetsLat lenght <= 1")
  if(length(targetsLong) <= 1) stop("targetsLong lenght <= 1")
  if(length(targetsLong) != length(targetsLat)) stop("targetsLong & targetsLat not of the same length")
  if(length(targetsLong) != length(targetsNames)) stop("targetsLong & targetsNames not of the same length")
  
  x <- t(matrix(c(sourceLat, targetsLat, sourceLong, targetsLong), nrow = 2, byrow = TRUE))
  dist <- dist(x)[seq(1, length(targetsLat))]
  loc <-  which.min(dist)
  #500 meter distance:
  threshold <- 0.00678
  result <- list("dist" = dist[loc],
                 "stop_name" = targetsNames[loc],
                 "largeDistance" = ifelse(dist[loc] > threshold, TRUE, FALSE))
  return(result)
}

sdb_stops_modded <- data.frame(dist = numeric(length = nrow(sdb_stops)),
                               stop_name = character(length = nrow(sdb_stops)),
                               largeDistance = logical(length = nrow(sdb_stops)))
load("sdb_stops_modded.RData")


pb = txtProgressBar(min = 0, max = nrow(sdb_stops), initial = 0, style = 3) 
for(i in seq(1, nrow(sdb_stops))){
  setTxtProgressBar(pb,i)
  if(sdb_stops_modded[i, ] == 0){
    sdb_stops_modded[i, ] <- findNearestStop(sdb_stops$stop_lat[i], sdb_stops$stop_lon[i], stops$stop_lat, stops$stop_lon, stops$stop_name)  
  }
}
