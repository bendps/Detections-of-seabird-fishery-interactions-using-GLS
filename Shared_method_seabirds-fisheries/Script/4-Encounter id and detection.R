rm(list = ls())
# Your WD #!!#
#setwd("C:/Users/dupui/Desktop/Shared_method_seabirds-fisheries")

#~~~~~~#
library(lubridate) ; library(ggplot2) ; library(dplyr) ;
library(cowplot) ; library(data.table) ; library(tidyr);
library(ggpubr)
#~~~~~~#


# Automatic detection function ----
light.detect <- function(mydatan){
  row.names(mydatan) <- NULL
  detect <- matrix()
  mylist <- list()
  
  # Threshold of the loggers
  if (mydatan$model[1] == "mk13" |
      mydatan$model[1] == "mk14" |
      mydatan$model[1] == "mk18" |
      mydatan$model[1] == "mk4093" |
      mydatan$model[1] == "mk4083" ){
    threshold <- 5
  }
  if (mydatan$model[1] == "mk3006" |
      mydatan$model[1] == "mk3005" |
      mydatan$model[1] == "mk15" |
      mydatan$model[1] == "mk19" |
      mydatan$model[1] == "mk3" |
      mydatan$model[1] == "mk4" |
      mydatan$model[1] == "mk7" ){
    threshold <- 10
  }
  if (mydatan$producer[1] == "Migrate Technology"){threshold <- 20}
  if (mydatan$producer[1] == "MT"){threshold <- 20}
  
  #Initialization
  moylight <- numeric()
  moycond <- numeric()
  i <- 1
  j <- 1
  
  # Encounter detection
  while (i <= nrow(mydatan)){ #Scan the data
    if (mydatan$raw_light[i] > threshold){ #Check if light is over the threshold
      
      date <- format(mydatan$date_time[i], "%d-%m-%Y")
      tfirst <- format(mydatan$date_time[i], "%d-%m-%Y %H:%M:%S")
      nightbeg <- format(mydatan$begin[i], "%d-%m-%Y %H:%M:%S")
      comp <- mydatan$date_time[i]
      
      while(mydatan$raw_light[i] > threshold & i <= nrow(mydatan) & mydatan$end[i] >= mydatan$date_time[i] & comp+(20*60) >= mydatan$date_time[i]){ #Keep saving while light is over the threshold
        moylight <- c(moylight, mydatan$raw_light[i])
        moycond <- c(moycond, mydatan$std_conductivity[i])
        tend <- format(mydatan$date_time[i], "%d-%m-%Y %H:%M:%S")
        nightend <- format(mydatan$end[i], "%d-%m-%Y %H:%M:%S")
        comp <- mydatan$date_time[i]
        i <- i+1
      }
      #Calcul mean light and acitvity of the encounter
      mean_conductivity <- mean(na.omit(moycond))
      mean_light <- mean(moylight)
      
      mylist[[i]] <- c(date,tfirst,tend,mean_light,mean_conductivity,nightbeg,nightend)
      
      moylight <- numeric()
      moycond <- numeric()
      i <- i+1
    }
    
    else{
      i <- i+1
      }
  }
  detect <- do.call("rbind",mylist) #merge the detection list in one df
  detect <- as.data.frame(detect)
  if(nrow(detect) >= 1){
    colnames(detect) <- c("date","tfirst","tend","mean_light","mean_conductivity","nightbeg","nightend")
    
    #Changing the date-time into POSIX and adding bird ID
    detect$date <- as.POSIXct(detect$date, format = "%d-%m-%Y")
    detect$tfirst <- as.POSIXct(detect$tfirst, format = "%d-%m-%Y %H:%M:%S")
    detect$tend <- as.POSIXct(detect$tend, format = "%d-%m-%Y %H:%M:%S")
    detect$nightbeg <- as.POSIXct(detect$nightbeg, format = "%d-%m-%Y %H:%M:%S")
    detect$nightend <- as.POSIXct(detect$nightend, format = "%d-%m-%Y %H:%M:%S")
    detect$individ_id <- rep(mydatan$individ_id[1], nrow(detect))
    detect$session_id <- rep(mydatan$session_id[1], nrow(detect))
    
    #factor->character->numeric for light and conductivity
    detect$mean_light<-as.character(detect$mean_light)
    detect$mean_light<-as.numeric(detect$mean_light)
    
    detect$mean_conductivity<-as.character(detect$mean_conductivity)
    detect$mean_conductivity<-as.numeric(detect$mean_conductivity)
    
    # Calcul duration of detection
    detect$duration <- time_length(detect$tend-detect$tfirst,unit = "minute")+10
    detect$nightlength <- time_length(detect$nightend-detect$nightbeg,unit = "minute")
    detect$encounter <- 1
    
    # Duration between detection
    inter <- as.numeric()
    cum <- as.numeric()
    
    for (j in 1:nrow(detect)){
      if(j == 1){
        inter <- 0
        cum <- 0
      }
      if(j > 1){
        inter <- c(inter,time_length(detect$tfirst[j]-detect$tfirst[j-1], unit = "minute"))
        cum <- c(cumsum(inter))
      }
    }
    
    if(length(inter) != nrow(detect)){print("error")}
    inter[inter == 0] <- NA
    detect <- cbind(detect,inter)
    detect <- cbind(detect,cum)
    
    return(detect)
  }
  if(nrow(detect) < 1){
    print("nodetection")
    return("no detection")
  }
}

# Function application to the dataset ----

folderpath <- "Data/Fulmar/Processed/Corrected/Night_and_no_twilight" #change to the output path of script 3. #!!#
mysavingpath <- "Data/Fulmar/Encounters" #change to the path where you want the data to be saved #!!#

#If you're using the public data sample, only the MT logger is available, so just run that loop with pattern = "-no-"
for(pattern in c("-low-")){ #Apply the function to the different loggers group, only -low- here to fit the sample data
  if(pattern == "-high-" | pattern == "-low-"){prod <- "Biotrack"}
  if(pattern == "-no-"){prod <- "MT"}
  data.files <- list.files(folderpath, pattern = pattern) 
  
  # Loop to detect all the encounters
  tot <- list()
  for (k in 1:length(data.files)){
    tot[[k]] <- light.detect(readRDS(paste0(folderpath,"/",data.files[k])))
    print(k)
  }
  x <- which(tot == "no detection") #removed the element of the list with no detections
  tot[x] <- NULL
  detect <- do.call("rbind",tot)
  detect <- as.data.frame(detect)
  
  detect$producer <- prod
  row.names(detect) <- NULL
  saveRDS(detect, paste0(mysavingpath,
                         "/encounters_V1_",
                         prod,pattern,
                         ".rds"))
}
