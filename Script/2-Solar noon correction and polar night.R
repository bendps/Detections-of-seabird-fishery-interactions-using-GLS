

#~~~~~~#
library(tidyverse)
library(chron);library(data.table); library(lubridate); library(suncalc)
#~~~~~~#

# encounters and locations data
loc <- readRDS("Data/Fulmar/Output_GLS_&_IRMA_locations_Fulmarus_glacialis_2020-04-30.rds") #change to the GLS and IRMA location file path #!!#
loc <- loc %>% dplyr::select(individ_id,timestamp,lon,lat,loc_type)
loc$individ_id <- str_replace(loc$individ_id, "-", "_") #annotation issue

# List of files
folderpath <- "Data/Fulmar/Processed" 
data.files <- list.files(folderpath, pattern = "DATA")
mysavingpath <- "Data/Fulmar/Processed/Corrected" 

# Loop to open files 1 by one
for (i in 1:length(data.files)){
  
  # Import light data
  light <- readRDS(paste0(folderpath,"/",data.files[i]))
  row.names(light) <- NULL

  light2 <- light %>% group_by(mydate,daynight) %>% summarise()
  light2 <- as.data.frame(light2)
  light2 <- light2 %>% arrange(mydate) #order by date

  # Beginning/end of polar night
  polar <- light2 %>%
    group_by(group = rleid(daynight)) %>%  # group by consecutive daynight values
    summarise(val = unique(daynight),      # get the daynight value
              Count = n(),                 # count rows of that value
              Beg = first(mydate),         # get first date
              End = last(mydate)) %>%      # get last date
    filter(Count >= 3 & val == 2) %>%      # keep only cases where there is 3+ daylight = 2
    dplyr::select(-group, -val)            # remove unecessary columns

  #Exclude breeding season
  polar <-polar[!(format(polar$Beg, format = "%m-%d") >= format(as.Date("05-01","%m-%d"),"%m-%d") &
                         format(polar$Beg, format = "%m-%d") <= format(as.Date("08-01","%m-%d"),"%m-%d")),]

  # Regroup date if interval < 15 days
  if (nrow(polar) > 1){
    j <- 2
    while (j <= nrow(polar)){
      if (abs(polar$End[j-1]-polar$Beg[j]) <= 15){

        polar$End[j-1] <- polar$End[j]
        polar$Count[j-1] <- abs(polar$End[j-1]-polar$Beg[j-1])
        polar <- polar[-j,]

      }
      else if(abs(polar$End[j-1]-polar$Beg[j]) > 10){j<-j+1}

    }
  }
  
  
  polar <- polar[which(polar$Count > 10),] # Do not consider interval smaller than 10 days
  if(nrow(polar) == 0){
    print(paste("no polar night with",data.files[i],"no correction"))
    
    light <- readRDS(paste0(folderpath,"/",data.files[i]))
    myloc <- loc[which(loc$timestamp >= min(light$date_time) &
                         loc$timestamp <= max(light$date_time)),]
    myloc <- myloc[which(myloc$individ_id == light$individ_id[1]),]
    
    if(nrow(myloc) > 0){
      saveRDS(light, paste0(mysavingpath,
                            "/DATA-",
                            light$session_id[1],
                            "-",
                            light$individ_id[1],
                            "-",
                            light$producer[1],
                            light$model[1],
                            "-",
                            light$colony[1],
                            ".rds"))
      print(i)
    }
    if(nrow(myloc) == 0){print(paste("no loc for", data.files[i]))} #if no locations are available for the light/activity data
  }
  if(nrow(polar) > 0){
    # Only keep loc of 1 individual at noon to avoid issues with the mapping and overlap
    myloc <- loc[which(loc$timestamp >= min(light$date_time) &
                       loc$timestamp <= max(light$date_time)),]
    myloc <- myloc[which(myloc$individ_id == light$individ_id[1]),]
    
    if(polar$Beg > date(min(myloc$timestamp)) &
       polar$End < date(max(myloc$timestamp))){
      noon <- getSunlightTimes(data = data.frame(date = date(myloc$timestamp), lon = myloc$lon, lat = myloc$lat),
                               tz = "CET",
                               keep = "solarNoon")
      noon <- noon %>% group_by(date) %>% summarise(solarNoon = mean(solarNoon))
      polar$Noonbeg <- as.POSIXct(NA)
      polar$Noonend <- as.POSIXct(NA)
      for(p in 1:nrow(polar)){
        polar$Noonbeg[p] <- noon$solarNoon[which(noon$date == polar$Beg[p])]
        polar$Noonend[p] <- noon$solarNoon[which(noon$date == polar$End[p])]
      }
      #Interval around the solar noon that will be removed
      myloc <- myloc %>%
        filter(hour(timestamp) <= hour(max(c(polar$Noonbeg,polar$Noonend)))+3 &
                 hour(timestamp) >= hour(min(c(polar$Noonbeg,polar$Noonend)))-3)
      
      # Get 10 days before and after each event
      if(nrow(polar) > 0){
        polar$min <- polar$Beg-10
        polar$max <- polar$End+10
        
        # Only keep solar noon for our intervals
        grpdate <- integer()
        class(grpdate) <- "POSIXct"
        
        if(nrow(myloc) > 0){
          for(k in 1:nrow(polar)){
            before <- myloc[lubridate::date(myloc$timestamp) %in%  seq.Date(from = polar$min[k], to = polar$Beg[k] , by = 1 ) ,2]
            after <- myloc[lubridate::date(myloc$timestamp) %in%  seq.Date(from = polar$End[k], to = polar$max[k] , by = 1 ) ,2]
            grpdate <- c(grpdate,before,after)
          }
          grpdate <- times(format(grpdate, "%H:%M:%S"))
          
          # Interval that need to be deleted for one individual
          mini <- as.POSIXct(as.character(min(grpdate)), format = "%H:%M:%S") - (60*60)
          maxi <- as.POSIXct(as.character(max(grpdate)), format = "%H:%M:%S") + (60*60)
          
          # Delete light data in this interval
          light <- readRDS(paste0(folderpath,"/",data.files[i]))
          supp <- which(format(light$date_time, "%H:%M:%S") >= format(mini, "%H:%M:%S") &
                        format(light$date_time, "%H:%M:%S") <= format(maxi, "%H:%M:%S"))
          light <- light[-supp,]
        }
      }
    }
    
    #Only save data if there is loc available
    myloc <- loc[which(loc$timestamp >= min(light$date_time) &
                         loc$timestamp <= max(light$date_time)),]
    myloc <- myloc[which(myloc$individ_id == light$individ_id[1]),]
    
    if(nrow(myloc) > 0){ #again
      saveRDS(light, paste0(mysavingpath,
                            "/DATA-",
                            light$session_id[1],
                            "-",
                            light$individ_id[1],
                            "-",
                            light$producer[1],
                            light$model[1],
                            "-",
                            light$colony[1],
                            ".rds"))
      print(i)
    }
    if(nrow(myloc) == 0){print(paste("no loc for", data.files[i]))}
  }
}


