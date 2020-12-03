rm(list = ls())
#Your WD #!!#
#setwd("C:/Users/dupui/Desktop/Shared_method_seabirds-fisheries")

#~~~~~~#
library(dplyr); library(lubridate); library(ggplot2);
library(ggrepel) ; library(ggmap) ; library(viridis);
require(maps); library(cowplot) ; library(maptools) ;
library(sp) ; library(rgdal) ; library(raster)
#~~~~~~#

# Load Files ----
folderpath <- "Data/Fulmar/Processed/Corrected/Night_and_no_twilight" #Change to the output path of script 3. #!!#
data.files <- list.files(folderpath, pattern = "DATA") 

detect <- readRDS("Data/Fulmar/Encounters/encounters_final_global.rds") #Change to the output path of script 7. #!!#
detect$tfirst <- force_tz(detect$tfirst,tzone="GMT")
detect$tend <- force_tz(detect$tend,tzone="GMT")
detect$nightbeg <- force_tz(detect$nightbeg,tzone="GMT")
detect$nightend <- force_tz(detect$nightend,tzone="GMT")
detect$londeg <- detect$lon
detect$latdeg <- detect$lat

# Add zones to detections ----
zones <- readOGR("Data/Zones/My_zones.shp") #Change to the path containing your zones of interest #!!#

# CRS for WGS84
proj.latlon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Center projection on the centroid of our observations
medlon <- 0
medlat <- 66.5

# CRS string 
proj.aeqd <- paste("+proj=aeqd +lat_0=",round(medlat), " +lon_0=",round(medlon)," +units=km ", sep="")

# Transforms to a SpatialPointsDataFrame
coordinates(detect) <- ~ lon + lat

# Define CRS
proj4string(detect) <- CRS(proj.latlon)

# Project the data
detect <- spTransform(detect, CRS(proj.aeqd))


# Project the World SpatialPolygon
zones <- spTransform(zones, CRS(proj.aeqd))
detect.df <- data.frame(detect) #Important because ggplot doesn't like spatial points

overlap <- detect %over% zones

detect.df$zone <- overlap$LME_NAME
detect.df$optional <- NULL
detect.df$lon <- detect.df$londeg
detect.df$lat <- detect.df$latdeg
detect.df$latdeg <- NULL
detect.df$londeg <- NULL

saveRDS(detect.df, "Data/Fulmar/Encounters/detect_final_zoned.rds") #Change to the output path desired for the zones annotated detections #!!#
  
# Add Annotation of boat and night detections ----
data.files <- list.files(folderpath, pattern = "DATA") 

detect <- readRDS("Data/Fulmar/Encounters/detect_final_zoned.rds")
detect$tfirst <- force_tz(detect$tfirst,tzone="GMT")
detect$tend <- force_tz(detect$tend,tzone="GMT")
detect$nightbeg <- force_tz(detect$nightbeg,tzone="GMT")
detect$nightend <- force_tz(detect$nightend,tzone="GMT")


for(i in 1:length(data.files)){
  mydf <- readRDS(paste0(folderpath,"/",data.files[i])) #Load the data
  row.names(mydf) <- NULL
  
  #Create 5 columns to identify night with detection and exact period of detections and loc
  mydf$detect_night <- NA #assess if the bird detect at least once light during this night, 1 if yes - 0 if no
  mydf$detect_boat <- NA #assess if the bird is currently close to light source, 1 if yes - 0 if no
  mydf$lon <- NA
  mydf$lat <- NA
  mydf$zone <- NA
  
  mydf$date_time <- force_tz( mydf$date_time,tzone="GMT")
  mydf$begin <- force_tz(mydf$begin,tzone="GMT")
  mydf$end <- force_tz(mydf$end,tzone="GMT")
  
  mydetect <- subset(detect, detect$session_id == mydf$session_id[1])
  
  refnight <- interval(mydetect$nightbeg, mydetect$nightend) #Interval containing all the night with detections for that session
  refboat <- interval(mydetect$tfirst, mydetect$tend) #Same but only for the exact detections period
  
  mydf$detect_night[which(interval(mydf$begin, mydf$end) %in% refnight)] <- 1 #annotate night with light detection
  
  for(k in which(mydf$detect_night == 1)){#annotate the exact detections
    if(any(mydf$date_time[k] %within% refboat)){
      mydf$detect_boat[k] <- 1
    }
    mydf$lon[k] <- mydetect$lon[which(mydetect$nightbeg == mydf$begin[k])[1]]
    mydf$lat[k] <- mydetect$lat[which(mydetect$nightbeg == mydf$begin[k])[1]]
    mydf$zone[k] <- as.character(mydetect$zone[which(mydetect$nightbeg == mydf$begin[k])[1]])
  }
  mydf$detect_night[which(is.na(mydf$detect_night))] <- 0
  mydf$detect_boat[which(is.na(mydf$detect_boat))] <- 0
  if(mydf$model[1] %in% c("mk4093","mk4083","mk13","mk14","mk18")){tresh <- "-low-"}
  if(mydf$model[1] %in% c("mk3006","mk3005","mk15","mk19","mk3","mk4","mk7")){tresh <- "-high-"}
  if(mydf$producer[1] == "MT" |(mydf$producer[1] == "Migrate Technology")){tresh <- "-no-"}
  mydf$class <- tresh
  
  #Save
  savingpath <- "Data/Fulmar/Final" #Can be changed to the path where you want the output file to be saved #!!#
  if(nrow(mydetect) >= 1){
    saveRDS(mydf,paste0(savingpath,
                        "/DATA-",
                        mydf$session_id[1],"-",
                        mydf$individ_id[1],
                        mydf$class[1],
                        mydf$colony[1],"-DETECT.rds"))
  }
  if(nrow(mydetect) == 0){
    saveRDS(mydf,paste0("Data/Fulmar/Final/DATA-",
                        mydf$session_id[1],"-",
                        mydf$individ_id[1],
                        mydf$class[1],
                        mydf$colony[1],"-EMPTY.rds"))
  }
  print(i)
}

