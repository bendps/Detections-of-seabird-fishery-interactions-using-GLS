rm(list = ls())
# Your WD #!!#
#setwd("C:/Users/dupui/Desktop/Shared_method_seabirds-fisheries")

#~~~~~~#
library(lubridate) ; library(ggplot2) ; library(dplyr) ;
library(cowplot) ; library(data.table) ; library(tidyr);
library(ggpubr)
#~~~~~~#

# Load Files
folderpath <- "Data/Fulmar/Processed/Corrected" #Change this path to the saving path of script 2. #!!#
data.files <- list.files(folderpath, pattern = "DATA")

mysavingpath <- "Data/Fulmar/Processed/Corrected/Night_and_no_twilight" #Path where the output data will be saved #!!#

for(i in 1:length(data.files)){
  mydata <- readRDS(paste0(folderpath,"/",data.files[i]))
  row.names(mydata) <- NULL
  
  # Only keep night data of the non-breeding season
  mydata <- mydata %>% filter(month(date_time) > 9 | month(date_time) < 4)
  mydatan <- mydata[which(mydata$daynight == 2),]
  
  if(nrow(mydata) == 0){print(paste("error",i))}
  
  if(nrow(mydata) > 1){
    #Solve polar night annotation issues
    na_inds_begin <- as.numeric((is.na(mydatan$begin)))
    na_inds_end <- as.numeric((is.na(mydatan$end)))
    
    na_diffs_lead <- c(0, diff(na_inds_begin))
    na_diffs_lag <- c(diff(na_inds_end), 0)
    
    first_nas <- na_inds_begin == 1 & na_diffs_lead > 0
    last_nas <- na_inds_end == 1 & na_diffs_lag < 0
    
    mydatan$begin[first_nas] <- mydatan$date_time[first_nas]
    mydatan$end[last_nas] <- mydatan$date_time[last_nas]
    
    mydatan$begin[first_nas] <- mydatan$date_time[first_nas]
    mydatan$end[last_nas] <- mydatan$date_time[last_nas]
    
    if(is.na(mydatan$begin[1])){
      mydatan$begin[1] <- mydatan$date_time[1]
    }
    
    if(is.na(mydatan$end[nrow(mydatan)])){
      mydatan$end[nrow(mydatan)] <- mydatan$date_time[nrow(mydatan)]
    }
    
    mydatan <-
      mydatan %>%
      fill(begin, .direction = "down") %>%
      fill(end, .direction = "up")

    # Remove data 30 min after begin and 30 min before end for each twilight
    mydatan$delete <- 0
    mydatan$delete[which(mydatan$date_time >= mydatan$begin & mydatan$date_time <= (mydatan$begin + 30*60))] <- 1
    mydatan$delete[which(mydatan$date_time <= mydatan$end & mydatan$date_time >= (mydatan$end - 30*60))] <- 1
    
    if(length(which(mydatan$delete == 1)) > 0){
      x<-mydatan[-which(mydatan$delete == 1),]
    }
    if(length(which(mydatan$delete == 1)) == 0){x<-mydatan}
    myfinal <- dplyr::select(x, -delete)
    
    # Loggers threshold categories
    if(myfinal$model[1] %in% c("mk4093","mk4083","mk13","mk14","mk18")){tresh <- "low"}
    if(myfinal$model[1] %in% c("mk3006","mk3005","mk15","mk19","mk3","mk4","mk7")){tresh <- "high"}
    if(myfinal$producer[1] == "MT" |(myfinal$producer[1] == "Migrate Technology")){tresh <- "no"}
    
    
    saveRDS(myfinal, paste0(mysavingpath,
                            "/DATA-",
                            myfinal$session_id[1],
                            "-",
                            myfinal$individ_id[1],
                            "-",
                            myfinal$producer[1],
                            myfinal$model[1],
                            "-",
                            tresh,
                            "-",
                            myfinal$colony[1],
                            "-",
                            "untwilight",
                            ".rds"))
    print(i)
  }
}
