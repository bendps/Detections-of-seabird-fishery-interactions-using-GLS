
#~~~~~~#
library(dplyr);library(lubridate);library(data.table)
#~~~~~~#

folderpath <- "Data/Fisheries" 

#Associate vessel type to location via mmsi####
vesselID <- readRDS(paste0(folderpath,"/fishing-vessels-v2.rds"))
loc.files <- list.files(paste0(folderpath,"/2012-2016_filtered/")) #We previously filter the fishing data to restrict them only to our study area, but its possible to use all of them, just need more memory
totvessel <- data.frame()

for(i in 1:length(loc.files)){
  vesselloc <- read.csv(paste0(folderpath,"/2012-2016_filtered/",loc.files[i]))
  vesselloc$geartype <- NA
  vesselloc$flag <- NA
  vesselloc$date <- as.Date(vesselloc$date)
  vesselloc$month <- month(vesselloc$date)
  vesselloc$year <- year(vesselloc$date)
  for(j in 1:nrow(vesselloc)){
    mmsi <- NA
    mmsi <- vesselloc$mmsi[j]
    id <- which(vesselID$mmsi == mmsi)
    vesselloc$flag[j] <- as.character(vesselID$flag[id])
    vesselloc$geartype[j] <- as.character(vesselID$geartype[id])
  }
  write.csv(vesselloc,paste0(folderpath,"/2012-2016_filtered_loc/",loc.files[i]))
  print(i)
}

#Group all the data in a rds file
loc.files <- list.files(paste0(folderpath,"/2012-2016_filtered_loc/")) 
totvessel <- data.frame()
for(i in 1:length(loc.files)){
  vesselloc <- read.csv(paste0(folderpath,"/2012-2016_filtered_loc/",loc.files[i]))
  totvessel <- rbind(totvessel,vesselloc)
  print(i)
}

saveRDS(totvessel, paste0(folderpath, "/2012-2016_filtered_loc.rds"))
