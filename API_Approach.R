library(tidyverse)
library(osmdata) # package for working with streets
library(showtext) # for custom fonts
library(ggmap)
library(rvest)
library(sf)
library(httr)
library(jsonlite)
source("OSM_Functions.R")


#München City
folder <- "mapDataSmall"
xRange <- c(11.3794, 11.7690)
yRange <- c(48.0473, 48.2336)

attrList <- list(
  list(name = "small_streets", key = "highway", value = c("residential", "living_street"), split = TRUE),
  list(name = "med_streets", key = "highway", value = c("secondary", "tertiary", "secondary_link", "tertiary_link"), split = TRUE),
  list(name = "big_streets", key = "highway", value = c("motorway", "primary", "motorway_link", "primary_link", "trunk", "trunk_link"), split = FALSE),
  list(name = "railway", key = "railway", value = c("rail"), split = FALSE),
  list(name = "water", key = "natural", value = c("water"), split = TRUE),
  list(name = "riverbanks", key = "waterway", value = c("riverbank"), split = TRUE),
  list(name = "riverSmall", key = "waterway", value = c("stream"), split = TRUE),
  list(name = "riverBig", key = "waterway", value = c("river"), split = FALSE),
  list(name = "verySmall_streets", key = "highway", value = c("track"), split = TRUE),
  list(name = "city_district", key = "boundary", value = c("administrative"), split = FALSE)
)

#Square Coordniates
sqLi <- ratioCoordinates(xRange = xRange, yRange = yRange, yDim = 40, xDim = 60)
xRange <- sqLi$xRange
yRange <- sqLi$yRange
coord <- matrix(c(xRange, yRange), nrow = 2, byrow = TRUE, dimnames = list(c("x", "y"), c("min", "max")))
fetchDataSplitedToDisk(coord, attrList, limit = 10, splits = 4, folder)
dl <- readDataFromDisk(folder, attrList)
size <- 7559 #do not change this, change factor! #32x32 600dpi
factor <- .2


queryRoute <- function(lat, lon, destStationID, startTime){
  baseURL <- "https://www.mvg.de/api/fahrinfo/routing/?"
  latString <- paste0("fromLatitude=", lat)
  lonString <- paste0("fromLongitude=", lon)
  toStationString <- paste0("toStation=", destStationID)
  startTime <- paste0("time=", startTime)
  req <- paste(paste0(baseURL, latString, collapse = ""),
                lonString,
                toStationString,
                startTime, 
                "max_walk_time_to_dest=360",
                "max_walk_time_to_start=360",
                sep = "&")
  
  resp <- GET(req)
  
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  
  return(
    list(
      content = parsed,
      lat = lat,
      lon = lon,
      destStationID = destStationID,
      startTime = startTime,
      response = resp
    ))
}


timeStringToAPIformat <- function(t, wd = 1){
  if(wd < 1 | wd > 7 ) stop("wd must be between 1 - 7")
  currentDay <-  as.numeric(strftime(as.POSIXct(Sys.time()), format = "%u"))
  todayTime <-  as.numeric(as.POSIXct(t, format="%H:%M:%S")) * 1000
  if(wd < currentDay){
    bonus <- (7 - currentDay) + wd
  } else if (wd > currentDay){
    bonus <- wd - currentDay
  } else {
    bonus <- 7
  }
  
  shiftTime <- todayTime + ((60*60*24) * 1000) * bonus
  print(strftime(as.POSIXct(shiftTime / 1000, origin = "1970-01-01"), format="%D - %H:%M:%S"))
  return(shiftTime)
}

timeAPIformatToString <- function(t){
  return(strftime(as.POSIXct(t / 1000, origin = "1970-01-01"), format="%H:%M:%S"))
}


rasterCoord <- function(coord, dist){
  xRange <- seq(coord["x", "min"], coord["x", "max"], by = dist)
  yRange <- seq(coord["y", "min"], coord["y", "max"], by = dist)
  df <- expand.grid(yRange, xRange)
  names(df) <- c("lat", "lon")
  return(df)
}

getShortestTime <- function(l){
  return(
    median(sapply(
      l$content$connectionList, \(el){
        (el$arrival - el$departure) / 1000
      }
    )
    , na.rm = TRUE))
}


queryRaster <- function(coord, dist, destStationID, startTime, pauseTime){
  folder <- file.path("queryRasterData", str_replace_all(paste(destStationID, timeAPIformatToString(startTime), dist, sep = "_"), ":", "_"))
  print(paste("Writing data to: ", folder))
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)
  if(file.exists(file.path(folder, "data.txt"))){
    df <- read.csv(file.path(folder, "data.txt"), header = TRUE)
  } else {
    df <- rasterCoord(coord = coord, dist = dist)
    df$travelTime <- NaN  
  }
  
  pb = txtProgressBar(min = 0, max = nrow(df), initial = 0, style = 3)
  for(i in seq(1, nrow(df))){
    setTxtProgressBar(pb,i)
    if(!is.na(df$travelTime[i])){print(paste0(i, ": travel time already known, skipping")); next}
    if(file.exists(file.path(folder, paste0(df$lat[i], "_", df$lon[i], ".Rdata")))){
      load(file.path(folder, paste0(df$lat[i], "_", df$lon[i], ".Rdata")))
      indList <- list()
      for(j in seq_along(routeList$content$connectionList)){
        if(checkIfContainsTaxi(routeList$content$connectionList[[j]])){
          indList[[length(indList) + 1 ]] <- j
        }
      }
      routeList$content$connectionList[unlist(indList)] <- NULL
      if(length(routeList[["content"]][["connectionList"]]) == 0){
        df$travelTime[i] <- NA
        print(paste0(i, ": routeList loaded but connection List is null"))
        next}
      
      df$travelTime[i] <- getShortestTime(routeList)
      print(paste0(i, ": found and used already downloaded Data"))
      next
    }
    tryCatch(
      {
        routeList <- queryRoute(lat = df$lat[i],
                                lon = df$lon[i],
                                destStationID = destStationID,
                                startTime = startTime)
        indList <- list()
        for(j in seq_along(routeList$content$connectionList)){
          if(checkIfContainsTaxi(routeList$content$connectionList[[j]])){
             indList[[length(indList) + 1 ]] <- j
          }
        }
        routeList$content$connectionList[unlist(indList)] <- NULL
        if(length(routeList[["content"]][["connectionList"]]) == 0){
          df$travelTime[i] <- NA
          print(paste0(i, ": downloaded new routeList, but connection List is null"))
          next}
        save(routeList, file = file.path(folder, paste0(df$lat[i], "_", df$lon[i], ".Rdata")))
        df$travelTime[i] <- getShortestTime(routeList)
        write.csv(df, file.path(folder, "data.txt"), row.names = FALSE)
        print(paste0(i, ": Success!"));
      },
      error=function(cond) {
        print(paste0(i, ": Some Error:"))
        message(cond)
        # Choose a return value in case of error
        
        df$travelTime[i] <- NA
      },
      warning=function(cond) {
        print(paste0(i, ": Some Warning"))
        message(cond)
        # Choose a return value in case of warning
        df$travelTime[i] <- NA
      }
    )
    print(df$travelTime[i])
    Sys.sleep(abs(rnorm(1, mean = pauseTime, sd = sqrt(pauseTime))))
  }
  write.csv(df, file.path(folder, "data.txt"), row.names = FALSE)
  return(df)
}

checkIfContainsTaxi <- function(l){
  if(length(l[["connectionPartList"]]) == 0){return(FALSE)}
  el <-  lapply(l[["connectionPartList"]], \(pl){
    return(pl[["product"]] == "TAXI")
  })
  return(sum(unlist(el)) > 0)
}


neuperlachSued <- queryRaster(coord,
                      dist = 0.0040, #distance between points (ca. 500 m)
                      destStationID = "de:09162:1010", #strings need to be queried or taken from the GTFS stops list
                      startTime = timeStringToAPIformat("08:00:00", wd = 1), #wd = weekday (1 [monday] - 7 [sunday])
                      pauseTime = 4) #pause in seconds with a random normal jitter, please be kind to the api

grosshadern <- queryRaster(coord,
                           dist = 0.0040,
                           destStationID = "de:09162:1540",
                           startTime = timeStringToAPIformat("08:00:00", wd = 1),
                           pauseTime = 4) 




neuperlachSued <- read.csv(file.path("queryRasterData", "de_09162_1010_08_00_00_0.004", "data.txt"), header = TRUE)
# neuperlachSued$travelTime_old <- neuperlachSued$travelTime
# neuperlachSued$travelTime <- pmin(neuperlachSued$travelTime, 3600) #capping travel time to 60 mins 

groshadern <- read.csv(file.path("queryRasterData", "de_09162_1540_08_00_00_0.004", "data.txt"), header = TRUE)
# groshadern$travelTime_old <- groshadern$travelTime
# groshadern$travelTime <- pmin(groshadern$travelTime, 3600) #capping travel time to 60 mins 

ng <- neuperlachSued
ng$travelTime <- rowMeans(cbind(neuperlachSued$travelTime, groshadern$travelTime))
ng$travelTime_old <- ng$travelTime
ng$travelTime <- pmin(ng$travelTime, 3600)

ggplot()+
  geom_raster(data = neuperlachSued,
                      aes(x = lon,
                          y = lat,
                          fill = travelTime/60),
              alpha = 1,
              interpolate = TRUE
  )+
  scale_fill_distiller(palette = "Spectral")


plotMap(li = dl,
        heatMapData = ng,
        xRange = xRange,
        yRange = yRange,
        cityName = "München",
        colorList = colorList,
        white = FALSE,
        size = size,
        yDim = 40,
        xDim = 60,
        degStep = .02,
        natureReserve = FALSE,
        factor = factor,
        folder = "maps")
