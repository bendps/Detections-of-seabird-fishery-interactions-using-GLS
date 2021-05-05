rm(list = ls())

#~~~~~~#
library(lubridate); library(tidyverse)
library(ggrepel) ; library(ggmap) ; library(viridis)
require(maps); library(cowplot) ; library(maptools) ;
library(sp) ; library(rgdal) ; library(raster)
#~~~~~~#

folderpath <- "Data/Fulmar/Final" 
data.files <- list.files(folderpath, pattern = "DATA")

zones <- readOGR("Data/Zones/My_zones.shp") 
world <- ne_countries(scale = "medium", returnclass = "sf") 

savingpath <- "Data/Fulmar/Final/Located" 

for(i in 1:length(data.files)){
  encounters <- readRDS(paste0(folderpath,"/",data.files[i]))
  encounters$lon <- NULL #Remove because we add location again in this script, but to every line of the data
  encounters$lat <- NULL 
  encounters$zone <- NULL
  
  loc <- readRDS("Data/Fulmar/Output_GLS_&_IRMA_locations_Fulmarus_glacialis_2020-04-30.rds") 
  loc$timestamp <- with_tz(loc$timestamp,tzone="GMT") #to fix TZ
  loc <- loc %>% dplyr::select(individ_id,timestamp,lon,lat,loc_type)
  loc$individ_id <- str_replace(loc$individ_id, "-", "_")

  myloc <- loc %>%
    filter(hour(timestamp) > 15 | hours(timestamp) < 4) #filtering only the midnigth loc
  myloc <- loc[loc$individ_id %in% encounters$individ_id,]
  
  # Lat and long ----
  z <- lapply(intersect(encounters$individ_id,myloc$individ_id),function(id) {
    encounters <- subset(encounters,individ_id==id)
    myloc <- subset(myloc,individ_id==id)
    
    encounters$indices <- sapply(encounters$date_time,function(d) which.min(abs(myloc$timestamp - d)))
    myloc$indices <- 1:nrow(myloc)
    
    merge(encounters,myloc,by=c('individ_id','indices'))
  })
  
  mydataloc <- do.call(rbind,z)
  mydataloc$indices <- NULL
  mydataloc$timestamp <- NULL
  
  # Zones ----
  positions <- mydataloc
  encounters$lon <- positions$lon
  encounters$lat <- positions$lat
  encounters$loc_type <- positions$loc_type
  
  # Prepare CRS Strings
  # CRS for WGS84
  proj.latlon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  # Center projection on the centroid of our observations
  medlon <- 0
  medlat <- 66.5
  
  # CRS string
  proj.aeqd <- paste("+proj=aeqd +lat_0=",round(medlat), " +lon_0=",round(medlon)," +units=km ", sep="")
  
  # Transforms to a SpatialPointsDataFrame
  coordinates(positions) <- ~ lon + lat
  
  # Define CRS
  proj4string(positions) <- CRS(proj.latlon)
  
  # Project the data
  positions <- spTransform(positions, CRS(proj.aeqd))
  
  
  # Project the World SpatialPolygon
  world <- st_transform(world, CRS(proj.aeqd))
  zones <- spTransform(zones, CRS(proj.aeqd))
  
  # Overlap with zones
  positions.df <- data.frame(positions)
  poszones <- positions %over% zones
  positions.df$zone <- poszones$LME_NAME
  encounters$zone <- positions.df$zone
  
  #Save
  if(length(which(encounters$detect_boat == 1)) > 0){
    saveRDS(encounters,paste0(savingpath,
                              "/DATA-",
                              encounters$session_id[1],"-",
                              encounters$individ_id[1],
                              encounters$class[1],
                              encounters$colony[1],"-DETECT.rds"))
  }
  if(length(which(encounters$detect_boat == 1)) == 0){
    saveRDS(encounters,paste0(savingpath,
                              "/DATA-",
                              encounters$session_id[1],"-",
                              encounters$individ_id[1],
                              encounters$class[1],
                              encounters$colony[1],"-EMPTY.rds"))
  }
  print(i)
}

