library(tidyverse)
library(osmdata) # package for working with streets
library(showtext) # for custom fonts
library(ggmap)
library(rvest)
library(sf)

################# MAPPING STUFF #####################

### higher Saturation:
colorList <- list(
  #waterColor = "#3a7878",
  waterColor = "#437070", #jenny's choice
  streetColor = "#513a54",
  ferryColor = "#efba71",
  railwayColor = "#d2af79",
  #railwayColor = "#863e2d", #paris
  natureColor = "#77c5b0",
  buildingColor = "#FF9705",
  backgroundColor = "#333333",
  textColor = "#333333",
  woodColor = "#57a145",
  parkColor = "#a4de9c",
  wiesenColor = "#cae892",
  felderColor = "#e8dc92",
  grenzeColor = "#FF0000"
)


plotMap <- function(li, heatMapData, xRange, yRange, cityName, colorList, degStep, size, factor, yDim, xDim, white = FALSE, natureReserve = TRUE, folder = "maps"){
  if(!dir.exists(folder)){
    dir.create(folder)
  }
  fivePercYHight <- ((yRange[2] - yRange[1]) * 0.07) * (xDim / yDim) 
  fivePercXWidth <- ((xRange[2] - xRange[1]) * 0.07) 
  yBreaks <- seq(yRange[1],yRange[2], degStep)
  xBreaks <- seq(xRange[1],xRange[2], degStep)
  
  g <- ggplot() +
    theme_classic() + 
    labs(x=NULL, y=NULL) + 
    theme(panel.background = element_rect(fill = "white",
                                          color = "white"),
          panel.border = element_blank(),
          plot.margin = unit(c(.2,0,0,.0), "cm"),
          legend.spacing=unit(0, "mm"),
          axis.text = element_blank(),
          legend.background = element_rect(fill = "white"),
          legend.box = NULL
    )+
    {if(!white){
      geom_rect(data=data.frame(), #Backgroud box to avoid small lines in the white border sections of the plot
                aes(xmin=xRange[1], xmax=xRange[2],
                    ymin=yRange[1], ymax=yRange[2]),
                color=colorList$backgroundColor, fill=colorList$backgroundColor)}}+ 
    geom_raster(data = heatMapData,
                aes(x = lon,
                    y = lat,
                    fill = travelTime / 60),
                alpha = 1,
                interpolate = TRUE
    )+
    scale_fill_distiller(palette = "Spectral") +
    # {if(natureReserve){
    #   geom_sf(data = li$nature_reserve$osm_polygons,
    #           fill = colorList$natureColor,
    #           colour = NA,
    #           alpha = .4)}}+ 
    geom_sf(data = li$water$osm_polygons,
            fill = colorList$waterColor,
            colour = NA ) +
    geom_sf(data = li$water$osm_multipolygons,
            fill = colorList$waterColor,
            colour = NA ) +
    geom_sf(data = li$riverbanks$osm_polygons,
            fill = colorList$waterColor,
            colour = NA ) +
    geom_sf(data = li$riverbanks$osm_multipolygons,
            fill = colorList$waterColor,
            colour = NA ) +
    geom_sf(data = li$riverBig$osm_lines,
            inherit.aes = FALSE,
            color = colorList$waterColor,
            size = 2 * factor,
            alpha = .8) +
    geom_sf(data = li$riverSmall$osm_lines,
            inherit.aes = FALSE,
            color = colorList$waterColor,
            size = 2 * factor,
            alpha = .8) +
    # geom_sf(data = li$wood$osm_polygons,
    #         fill = colorList$woodColor,
    #         colour = NA,
    #         alpha = .15) +
    # geom_sf(data = li$parks$osm_polygons,
    #         fill = colorList$parkColor,
    #         colour = NA,
    #         alpha = .15) +
    # geom_sf(data = li$wiesen$osm_polygons,
    #         fill = colorList$wiesenColor,
    #         colour = NA,
    #         alpha = .15) +
    # geom_sf(data = li$felder$osm_polygons,
    #         fill = colorList$felderColor,
    #         colour = NA,
    #         alpha = .15) +
    geom_sf(data = li$railway$osm_lines,
            inherit.aes = FALSE,
            color = colorList$railwayColor,
            size = 1.5 * factor,
            linetype="twodash",
            alpha = .8) +
    geom_sf(data = li$big_streets$osm_lines,
            inherit.aes = FALSE,
            color = colorList$streetColor,
            size = .9 * factor,
            alpha = .8)+
    geom_sf(data = li$med_streets$osm_lines,
            inherit.aes = FALSE,
            color = colorList$streetColor,
            size = .8 * factor,
            alpha = .8) +
    geom_sf(data = li$small_streets$osm_lines,
            inherit.aes = FALSE,
            color = colorList$streetColor,
            size = .6 * factor,
            alpha = .7) +
    geom_sf(data = li$verySmall_streets$osm_lines,
            inherit.aes = FALSE,
            color = colorList$streetColor,
            size = .4 * factor,
            alpha = .6) +
    # geom_sf(data = li$buildings$osm_polygons,
    #         inherit.aes = FALSE,
    #         fill = colorList$buildingColor,
    #         colour = NA,
    #         #size = .1 * factor,
    #         alpha = .8) +
    # geom_sf(data = li$ferry$osm_lines,
    #         inherit.aes = FALSE,
    #         color = colorList$ferryColor,
    #         size = 1.5 * factor,
    #         alpha = .7,
  #         linetype = "longdash") +
  geom_sf(data = li$grenze$osm_lines,
          inherit.aes = FALSE,
          color = colorList$grenzeColor,
          size = 1.5 * factor,
          linetype="dotdash",
          alpha = .6) +
  coord_sf(xlim = c(xRange[1] - fivePercXWidth, xRange[2]),
           ylim = c(yRange[1] - fivePercYHight, yRange[2]),
           expand = FALSE)+
    geom_rect(data=data.frame(), #left bar (y bar)
              aes(xmin=xRange[1] - fivePercXWidth, xmax=xRange[1] + .001,
                  ymin=yRange[1] - fivePercYHight, ymax=yRange[2] + .001),
              color="white", fill="white")+
    geom_rect(data=data.frame(), #lower bar (x bar)
              aes(xmin=xRange[1] - 0.001, xmax = xRange[2] + .001,
                  ymin=yRange[1] - fivePercYHight, ymax=yRange[1]),
              color="white", fill="white")+
    geom_rect(data=data.frame(), #right hand side bar to tidy frame
              aes(xmin=xRange[2], xmax=xRange[2] + fivePercXWidth,
                  ymin=yRange[1] - .001, ymax=yRange[2] + .001),
              color="white", fill="white")+
    annotate(geom = "text",
             label = cityName,
             x = xRange[2] - (fivePercXWidth / 12),
             y = yRange[1] - (fivePercYHight / 2),
             vjust = 0.5,
             hjust = 1.0,
             size = 100 * factor,
             color = colorList$textColor)  +
    scale_x_discrete(name = "",
                     breaks = xBreaks,
                     labels = sprintf("%.2f°E", xBreaks)) +
    scale_y_discrete(name = "",
                     breaks = yBreaks,
                     labels = sprintf("%.2f°N", yBreaks),
                     expand = expansion(mult = c(0, .1))) +
    theme(axis.text.y = element_text(angle = 90,
                                     hjust = .5,
                                     vjust = 0,
                                     size = 70 * factor,
                                     color = colorList$textColor),
          axis.text.x = element_text(hjust = 0.5,
                                     vjust = 0,
                                     size = 70 * factor,
                                     color = colorList$textColor))
  
  nextNumber <- max(1, 1 + as.numeric(stringr::str_extract(list.files(folder)[grepl(paste0(cityName, "_", ceiling(size * factor), "_", "[0-9]*\\.png"), list.files(folder))], "[0-9]+\\.")), na.rm = TRUE)
  print(paste0("----- Writing Map to: ", file.path(folder, paste0(cityName, "_", ceiling(size * factor), "_", nextNumber, ".png")), " -----"))
  print("----- This may take a hot minute, depending on the image size -----")
  
  ggsave(filename = file.path(folder, paste0(cityName, "_", ceiling(size * factor), "_", nextNumber, ".png")),
         plot = g,
         # width = ceiling(size * factor),
         # height =  ceiling(size * factor),
         # units = "cm",
         # dpi = 600,
         width = xDim,
         height =  yDim,
         units = "cm",
         dpi = 600,
         device = "png",
         bg = "white",
         type = "cairo")
  
  return(0)
}


fetchData <- function(coord, attrList, limit = 10){
  l <- list()
  tries <- 0
  
  for(i in seq_along(attrList)){
    while(length(l) != i &&  tries <= limit){
      if(tries > limit)stop("Out of tries, try again later")
      print(paste0("Fetching Key: ", attrList[[i]]$key," (Values: ", paste0(attrList[[i]]$value, collapse = ", "), ")"))
      l[[attrList[[i]]$name]] <- tryCatch({
        fetchDatum(attrList[[i]]$key, attrList[[i]]$value, coord)
      }, error = function(e){tries <<- tries + 1; print(e); Sys.sleep(5); return(NULL)})
      Sys.sleep(1)
    }
    if(attrList[[i]]$key == "boundary"){
      l[[attrList[[i]]$name]]$osm_lines <- l[[attrList[[i]]$name]]$osm_lines[l[[attrList[[i]]$name]]$osm_lines$admin_level == 9,]
    }
  }
  return(l)
}

fetchDatum <- function(key, value, coord){
  if("*" %in% value){
    dt <- coord %>%
      opq(timeout = 600)%>%
      add_osm_feature(key = key) %>%
      osmdata_sf() %>% 
      osmdata::unname_osmdata_sf()
  } else {
    dt <- coord %>%
      opq(timeout = 600)%>%
      add_osm_feature(key = key, 
                      value = value) %>%
      osmdata_sf() %>% 
      osmdata::unname_osmdata_sf()
  } 
  return(dt)
}

fetchDatumToDisk <- function(key, value, coord, file){
  if("*" %in% value){
    dt <- coord %>%
      opq(timeout = 600)%>%
      add_osm_feature(key = key) %>%
      osmdata_sf() %>% 
      osmdata::unname_osmdata_sf()
  } else {
    dt <- coord %>%
      opq(timeout = 600)%>%
      add_osm_feature(key = key, 
                      value = value) %>%
      osmdata_sf() %>% 
      osmdata::unname_osmdata_sf()
  }
  if(key == "building"){
    dt$osm_points <- NULL
    dt$osm_polygons <- dt$osm_polygons[, c("osm_id", "geometry")]
  }
  saveRDS(dt, file = file)
  return(0)
}

readDataFromDisk <- function(path, attrList){
  l <- list()
  for(i in seq_along(attrList)){
    print(paste0("Loading from Disk - Key: ", attrList[[i]]$key," (Values: ", paste0(attrList[[i]]$value, collapse = ", "), ")"))
    elementList <- list.files(path, pattern = paste0(attrList[[i]]$name, "_[0-9]+" , ".rds"), full.names = TRUE)
    if(length(elementList) > 1){
      l2 <- list()
      for(j in seq_along(elementList)){
        l2[[j]] <- readRDS(elementList[j])
      }
      l[[attrList[[i]]$name]] <- do.call(c, l2)  
    } else {
      l[[attrList[[i]]$name]] <- readRDS(elementList[1])
    }
    if(attrList[[i]]$key == "boundary"){
      l[[attrList[[i]]$name]]$osm_lines <- l[[attrList[[i]]$name]]$osm_lines[l[[attrList[[i]]$name]]$osm_lines$admin_level == 2,]
    }
  }
  return(l)
}


latLongDist <- function(yRange, xRange){
  if(length(yRange) != 2) stop("length of yRange is not 2")#
  if(length(xRange) != 2) stop("legnth of xRange is not 2")
  yDist <- (max(yRange) - min(yRange)) * 111.111
  xDist <- (max(xRange) * 111.111 * cos(mean(yRange)* pi / 180)) - (min(xRange) * 111.111 * cos(mean(yRange)* pi / 180))
  return(list("yDist" = yDist,
              "xDist" = xDist))
}


expandYRange <- function(yRange, dist){
  expandHalfLength <- (dist / 111.111) / 2
  return(c(yRange[1] - expandHalfLength,
           yRange[2] + expandHalfLength))
}


expandXRange <- function(xRange, dist, meanY){
  expandHalfLength <- (dist / (111.111 * cos(meanY * pi / 180))) / 2
  return(c(xRange[1] - expandHalfLength,
           xRange[2] + expandHalfLength))
}



ratioCoordinates <- function(yRange, xRange, yDim, xDim){
  l <- latLongDist(yRange = yRange, xRange = xRange)
  ratio <- xDim / yDim
  ratioDiff <- ratio - (l$xDist / l$yDist)
  
  if(abs((l$xDist / l$yDist) - ratio) < 0.0001){
    return(list("yRange" = yRange,
                "xRange" = xRange))
  } else if(ratioDiff > 0){
    dist <- l$xDist * ((ratio / (l$xDist / l$yDist)) - 1)
    return(list("yRange" = yRange,
                "xRange" = expandXRange(xRange, dist, mean(yRange)))
    )
  } else {
    dist <- l$yDist * (((l$xDist / l$yDist) / ratio) - 1)
    return(list("yRange" = expandYRange(yRange, dist),
                "xRange" = xRange)
    )
  }
}


splitCoord <- function(coord, nr, nc){
  xRange <- seq(coord["x", "min"], coord["x", "max"], length.out = nc +1)
  yRange <- seq(coord["y", "min"], coord["y", "max"], length.out = nr +1)
  coordList <- list()
  for(i in seq(nr )){
    for(j in seq(nc)){
      coordList[[length(coordList) + 1]] <- matrix(c(xRange[j], xRange[j + 1], yRange[i], yRange[i + 1]),
                                                   nrow = 2,
                                                   byrow = TRUE,
                                                   dimnames = list(c("x", "y"), c("min", "max")))
    }
  }
  return(coordList)
}

fetchDatumSplited <- function(key, value, coordList, limit = 10){
  li <- list()
  tries <- 0
  
  for(i in seq_along(coordList)){
    while(length(li) != i &&  tries <= limit){
      if(tries > limit)stop("Out of tries, try again later")
      print(paste0("Fetching Sector: ", i , " / ", length(coordList)))
      li[[length(li) + 1]] <- tryCatch({
        fetchDatum(key = key, value = value, coord = coordList[[i]])
      }, error = function(e){tries <<- tries + 1; print(e); Sys.sleep(5); return(NULL)})
      Sys.sleep(1)
    }
  }
  return(li)
}

fetchDataSplited <- function(coord, attrList, limit = 10, splits = 16){
  coordList <- splitCoord(coord, ceiling(sqrt(splits)), ceiling(sqrt(splits)))
  
  l <- list()
  tries <- 0
  
  for(i in seq_along(attrList)){
    while(length(l) != i &&  tries <= limit){
      if(tries > limit)stop("Out of tries, try again later")
      print(paste0("Fetching Key: ", attrList[[i]]$key," (Values: ", paste0(attrList[[i]]$value, collapse = ", "), ")"))
      if(attrList[[i]]$split){
        datumSplitted <- tryCatch({
          fetchDatumSplited(attrList[[i]]$key, attrList[[i]]$value, coordList = coordList)
        }, error = function(e){tries <<- tries + 1; print(e); Sys.sleep(5); return(NULL)})
        l[[attrList[[i]]$name]] <- do.call(c, datumSplitted)
      } else {
        l[[attrList[[i]]$name]] <- tryCatch({
          fetchDatum(attrList[[i]]$key, attrList[[i]]$value, coord = coord)
        }, error = function(e){tries <<- tries + 1; print(e); Sys.sleep(5); return(NULL)})
      }
      
      Sys.sleep(1)
    }
    if(attrList[[i]]$key == "boundary"){
      l[[attrList[[i]]$name]]$osm_lines <- l[[attrList[[i]]$name]]$osm_lines[l[[attrList[[i]]$name]]$osm_lines$admin_level == 2,]
    }
  }
  return(l)
}

fetchDataSplitedToDisk <- function(coord, attrList, limit = 10, splits = 16, path){
  coordList <- splitCoord(coord, ceiling(sqrt(splits)), ceiling(sqrt(splits)))
  tries <- 0
  
  for(i in seq_along(attrList)){
    s <- 1
    while(s != 0 &&  tries <= limit){
      if(tries > limit)stop("Out of tries, try again later")
      print(paste0("Fetching Key: ", attrList[[i]]$key," (Values: ", paste0(attrList[[i]]$value, collapse = ", "), ")"))
      if(attrList[[i]]$split){
        s <- tryCatch({
          fetchDatumSplitedToDisk(attrList[[i]]$key,
                                  attrList[[i]]$value,
                                  coordList = coordList,
                                  path = path,
                                  name = attrList[[i]]$name)
        }, error = function(e){tries <<- tries + 1; print(e); Sys.sleep(5); return(NULL)})
      } else {
        tryCatch({
          s <- fetchDatumToDisk(attrList[[i]]$key,
                                attrList[[i]]$value,
                                coord = coord,
                                file = file.path(path, paste0(attrList[[i]]$name, "_1.rds"))
          )
        }, error = function(e){tries <<- tries + 1; print(e); Sys.sleep(5); return(NULL)})
      }
      
      Sys.sleep(1)
    }
    # if(attrList[[i]]$key == "boundary"){
    #   l[[attrList[[i]]$name]]$osm_lines <- l[[attrList[[i]]$name]]$osm_lines[l[[attrList[[i]]$name]]$osm_lines$admin_level == 2,]
    # }
  }
  return(0)
}

fetchDatumSplitedToDisk <- function(key, value, coordList, limit = 10, path, name){
  tries <- 0
  
  for(i in seq_along(coordList)){
    s <- 1
    while(s != 0 &&  tries <= limit){
      if(tries > limit)stop("Out of tries, try again later")
      print(paste0("Fetching Sector: ", i , " / ", length(coordList)))
      tryCatch({
        s <- fetchDatumToDisk(key = key,
                              value = value,
                              coord = coordList[[i]],
                              file = file.path(path, paste0(name, "_", i, ".rds"))
        )
      }, error = function(e){tries <<- tries + 1; print(e); Sys.sleep(5); return(NULL)})
      Sys.sleep(1)
    }
  }
  return(0)
}