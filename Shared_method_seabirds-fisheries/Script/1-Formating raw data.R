rm(list=ls())
#your WD #!!#
#setwd("C:/Users/dupui/Desktop/Shared_method_seabirds-fisheries")

#~~~~~~#
library(lubridate) ; library(ggplot2); library(dplyr) ;
library(data.table) ; library(baytrends) ; library(tidyr) ;
library(stringr)
#~~~~~~#

# Import Data + Format
deployments <- readRDS("Data/Fulmar/SUMMARY-DEPLOYMENTS_all_colonies.rds") #path to the deployment file #!!#
transitions <- readRDS("Data/Fulmar/TRANSITIONS_all_colonies.rds") #path to the transitions file #!!#
folderpath <- "Data/Fulmar/Raw" #folderpath of your raw data #!!#
activity.files <- list.files(folderpath, pattern = "ACTIVITY") 
light.files <- list.files(folderpath, pattern = "LIGHT")

mysavingpath <- "Data/Fulmar/Processed" #Path where the output data will be saved #!!#
# Correct colony annotation and names ####
#Due to small amount of data and small distances, some colonies are merged
deployments$colony[which(deployments$colony == "Breidafjordur")] <- "Breidafjordur & R."
deployments$colony[which(deployments$colony == "Reykjanes")] <- "Breidafjordur & R."

deployments$colony[which(deployments$colony == "Langanes and Skjalfandi")] <- "Langanes & S. & G."
deployments$colony[which(deployments$colony == "Grimsey")] <- "Langanes & S. & G."

deployments$colony[which(deployments$colony == "Papey")] <- "Papey & H."
deployments$colony[which(deployments$colony == "Holmahals")] <- "Papey & H."

#deployments <- deployments[-which(deployments$colony == "Isle of Canna"),]
saveRDS(deployments,"Data/Fulmar/SUMMARY-DEPLOYMENTS_all_colonies.rds") #Change to the deployment file path #!!#
# Loop to open every session_id one by one ####
for (i in 1:nrow(deployments)){

  # First line finds the file name, second line opens the corresponding file 
  myact.file <- NA
  mylight.file <- NA
  myact.file <- activity.files [grep(deployments$session_id[i], activity.files )]
  mylight.file <- light.files [grep(deployments$session_id[i], light.files )]
  
  if (length(myact.file) == 0 | length(mylight.file) == 0){
    print("no") #if no file available for the deployment
  }
  else if(length(myact.file) >= 1 | length(mylight.file) >= 1){
    myactivity <-readRDS(paste(folderpath,myact.file, sep="/")) 
    mylight <- readRDS(paste(folderpath,mylight.file, sep="/"))

    # Adding date and hour colums
    mylight$mydate <- date(mylight$date_time)
    mylight$myhour <- hour(mylight$date_time)
    
    myactivity$mydate <- date(myactivity$date_time)
    myactivity$myhour <- hour(myactivity$date_time)
    
    # Filter transitions and locations
    mytrans <- transitions %>%
      filter(session_id == deployments$session_id[i] & individ_id == deployments$individ_id[i])
    
    # Convert to data.table
    setDT(mylight)
    setDT(myactivity)
    setDT(mytrans)
    
    # Correct wrong day/night annotation
    mytrans$duration <- time_length(mytrans$tsecond-mytrans$tfirst, unit = "day")
    error <- which(mytrans$duration >= 1) #identify long transition period
    
    night <- which(mytrans$duration >= 1 &
                     as.POSIXct(format(mytrans$tfirst, "%m-%d"), format = "%m-%d") >= date("2021-09-10")|
                     mytrans$duration >= 1 &
                     as.POSIXct(format(mytrans$tfirst, "%m-%d"), format = "%m-%d") <= date("2021-03-10") )
    
    day <- which(mytrans$duration >= 1 &
                   as.POSIXct(format(mytrans$tfirst, "%m-%d"), format = "%m-%d") >= date("2021-03-11")&
                   as.POSIXct(format(mytrans$tfirst, "%m-%d"), format = "%m-%d") <= date("2021-09-09") )
    
    mytrans$twl_type[night] <- 2
    mytrans$twl_type[day] <- 1
    
    # Dummy time for data (this method need an interval)
    mylight$dummy <- mylight$date_time
    myactivity$dummy <- myactivity$date_time
    
    #Setting to tell data.table what to join on
    setkey(mylight, date_time, dummy)
    setkey(myactivity, date_time, dummy)
    
    #Joining activity and light data
    mydata <- foverlaps(mylight, myactivity, nomatch=NA)[, dummy := NULL]
    mydata <- mydata %>% dplyr::select(i.session_id,i.individ_id,i.date_time,i.mydate,i.myhour,raw_light,conductivity,std_conductivity)
    mydata$conductivity<-fillMissing(mydata$conductivity, span = 1, max.fill = 8)
    mydata$std_conductivity<-fillMissing(mydata$std_conductivity, span = 1, max.fill = 8)
    
    mydata$dummy <- mydata$i.date_time
    setkey(mydata, i.date_time, dummy)
    setkey(mytrans, tfirst, tsecond)
    
    # Creating the joined tab 
    mydatadn <- foverlaps(mydata, mytrans, nomatch=NA)[, dummy := NULL]
    mydatadn <- mydatadn %>% dplyr::select(i.session_id,i.individ_id,i.date_time,i.mydate,i.myhour,tfirst,tsecond,twl_type,raw_light,conductivity,std_conductivity)
    names(mydatadn) <- c("session_id", "individ_id", "date_time", "mydate", "myhour","begin","end", "daynight","raw_light","conductivity","std_conductivity")
    mydatadn$std_date <- as.Date(format(mydatadn$date_time, format = "%m-%d"), format = "%m-%d")
    
    #Fixing decimal
    mydatadn$std_conductivity<-round(mydatadn$std_conductivity,3)
    mydatadn$conductivity<-round(mydatadn$conductivity,3)
    
    #Fixing last value issue
    if(is.na(mydatadn$std_conductivity[nrow(mydatadn)])){
      mydatadn$std_conductivity[nrow(mydatadn)] <- mydatadn$std_conductivity[nrow(mydatadn)-1]
      mydatadn$conductivity[nrow(mydatadn)] <- mydatadn$conductivity[nrow(mydatadn)-1]
    }
    
    # replacing NA by 3 for polar day/night and 0 to fix
    # 1440 = gap bigger than 10 days in data are consider to be polar day/night
    mydatadn$daynight[with(rle(is.na(mydatadn$daynight)), rep(lengths >= 1440  & values, lengths))] <- 3
    
    #nofixing
    mydatadn<-mydatadn[-which(is.na(mydatadn$daynight)),]
    
    # replacing 3 by day/nigth
    key <- which(mydatadn$daynight == 3)
    mydatadn$daynight[key] <- ifelse(mydatadn$std_date[key] >= as.Date("2020-03-01", format = "%Y-%m-%d") & mydatadn$std_date[key] <= as.Date("2020-09-10", format = "%Y-%m-%d"), 1,
                                     ifelse(mydatadn$std_date[key] > as.Date("2020-09-10", format = "%Y-%m-%d") | mydatadn$std_date[key] < as.Date("2020-03-01", format = "%Y-%m-%d"), 2, NA))
    
    # Adding the producer
    if (deployments$producer[i] == "Migrate Technology"){prod <- "MT"}
    if (deployments$producer[i] == "Biotrack" | deployments$producer[i] == "BAS" ){prod <- "Biotrack"}
    
    mydatadn$producer <- prod
    mydatadn$colony <- deployments$colony[i]
    mydatadn$model <- deployments$logger_model[i]
    
    # Saving the data, change the path depending on your WD #!!#
    saveRDS(object = mydatadn,file =(paste0(mysavingpath,
                                            "/DATA-",
                                           deployments$session_id[i],
                                           "-",
                                           deployments$individ_id[i],
                                           "-",
                                           deployments$colony[i],
                                           "-",
                                           prod,
                                           deployments$logger_model[i],
                                           ".rds")))
    print(i)
  }
}
