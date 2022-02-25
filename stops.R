#combine mvv & mvg data
# mvv_stops <- read.csv(file.path("mvv", "stops.txt"))
# mvg_stops <- read.csv(file.path("mvg", "stops.txt"))
# mvv_stops$source <- "mvv"
# mvg_stops$source <- "mvg"
# stops_bothNames <- intersect(names(mvv_stops), names(mvg_stops))
# stops <- rbind.data.frame(mvv_stops[, stops_bothNames], mvg_stops[, stops_bothNames])
# rm("mvv_stops", "mvg_stops")
stops <- read.csv(file.path("combinedData", "stops_mvv_mvg.txt"), header = TRUE)


### Queries: ID to Parent_ID
stopsInternal_makeStopHashEnv_parentID <- function(){
  EE <- new.env(hash = TRUE)
  
  stop_id_parent <- lapply(stops$stop_id, \(id){
    el <- strsplit(id, ":")
    return(paste0(el[[1]][1:3], collapse = ":"))
  })
  list2env(setNames(stop_id_parent, stops$stop_id),
           envir = EE)
  return(EE)
}

stopsInternal_IDtoParentIDEnv <- stopsInternal_makeStopHashEnv_parentID()

stops_IDtoParentID <- function(id, env = stopsInternal_IDtoParentIDEnv){
  return(stopsInternal_IDtoParentIDEnv[[id]])
}
stops_IDtoParentID <- Vectorize(stops_IDtoParentID)


### Queries: ID to NameOf(Parent_ID)
stops$parentName <- unlist(lapply(stops$stop_id, \(id){stops$stop_name[stops$stop_id == stops_getParentStop(id)]}))
stopsInternal_makeStopHashEnv_IDtoName <- function(){
  EE <- new.env(hash = TRUE)
  list2env(setNames(as.list(stops$parentName), stops$stop_id),
           envir = EE)
  return(EE)
}

stopsInternal_StopNameEnv <- stopsInternal_makeStopHashEnv_IDtoName()
stops_stopIDtoParentName <- function(stop_id){
  return(stopsInternal_StopNameEnv[[stop_id]])
}
stops_stopIDtoParentName <- Vectorize(stops_stopIDtoParentName)


### External Function:
stops_getStopsDataForNetwork <- function(ids = NULL){
  if(is.null(ids)){
    return(stops[stops$stop_id == stops_getParentStop(stops$stop_id),
                 c("stop_id", "stop_name", "stop_lat", "stop_lon")])  
  } else {
    return(stops[stops$stop_id %in% ids,
                 c("stop_id", "stop_name", "stop_lat", "stop_lon")])  
  }
}


########################## Data modding section ##################
##########################  DONT SOURCE!        ##################

##### Make Perent Stop for every stop without parrent #########
stopsInternal_hasParentStop <- function(stop_id){
  parent_id <- stops_getParentStop(stop_id)[[1]]
  if(is.null(parent_id)) stop("stop_id is not known")
  if(parent_id %in% stops$stop_id) return(TRUE)
  return(FALSE)
}
stopsInternal_hasParentStop <- Vectorize(stopsInternal_hasParentStop)

pb = txtProgressBar(min = 0, max = nrow(stops), initial = 0, style = 3) 
for(i in seq(1, nrow(stops))){
  setTxtProgressBar(pb,i)
  if(!stopsInternal_hasParentStop(stops$stop_id[i])){
    newLoc <- nrow(stops) + 1
    stops[newLoc, ] <- stops[i, ]
    stops$stop_id[newLoc] <- stops_getParentStop(stops$stop_id[i])
  }
}
stopsInternal_IDtoParentIDEnv <- stopsInternal_makeStopHashEnv_parentID()
write.csv(stops, file.path("combinedData", "stops_mvv_mvg.txt"))


######### match s-bahn stops ###############
sdb_stops <- read.csv(file.path("sdb", "stops.txt"))


findNearestStop <- function(sourceLat, sourceLong, targetsLat, targetsLong, targetsNames, targetsID){
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
  #450 meter distance:
  threshold <- 0.00600
  result <- list("dist" = dist[loc],
                 "stop_name_guess" = targetsNames[loc],
                 "stop_id_guess" = targetsID[loc],
                 "largeDistance" = ifelse(dist[loc] > threshold, TRUE, FALSE))
  return(result)
}

sdb_stops_modded <- data.frame(dist = numeric(length = nrow(sdb_stops)),
                               stop_name_guess = character(length = nrow(sdb_stops)),
                               stop_id_guess = character(length = nrow(sdb_stops)),
                               largeDistance = logical(length = nrow(sdb_stops)))

pb = txtProgressBar(min = 0, max = nrow(sdb_stops), initial = 0, style = 3) 
for(i in seq(1, nrow(sdb_stops))){
  setTxtProgressBar(pb,i)
  if(sdb_stops_modded[i, 1] == 0){
    sdb_stops_modded[i, ] <- findNearestStop(sdb_stops$stop_lat[i], sdb_stops$stop_lon[i], stops$stop_lat, stops$stop_lon, stops$stop_name, stops$stop_id)  
  }
}

sdb_stops_combined <- cbind(sdb_stops, sdb_stops_modded)
head(sdb_stops_combined)
write.csv(sdb_stops_combined, file.path("sdb", "stops_modded.txt"))




############ remove duplicated stops resulting from data merge (mvv / mvg) ############
stops <- stops[!duplicated(stops$stop_id), ]
dim(stops)
write.csv(stops, file.path("combinedData", "stops_mvv_mvg.txt"))
