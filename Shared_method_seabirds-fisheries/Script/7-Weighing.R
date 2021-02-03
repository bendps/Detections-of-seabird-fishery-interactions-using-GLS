rm(list = ls())
#Your WD #!!#
#setwd("C:/Users/dupui/Desktop/Shared_method_seabirds-fisheries")

#~~~~~~~#
library(tidyr);library(suncalc);library(lubridate);library(data.table)
library(ggplot2); library(purrr); library(dplyr) ; library(stringr)
#~~~~~~~#

folderpath <- "Data/Fulmar/Encounters" #change to the output path of script 5. #!!#
savingpath <- "Data/Fulmar/Encounters" #change to the path where you want the corrected data to be saved #!!#
normdata <- NULL

#Load the data
#no <- readRDS(paste0(folderpath,"/encounters_localized-no-.rds")) #only one available in the public sample
#high <- readRDS(paste0(folderpath,"/encounters_localized-high-.rds"))
low <- readRDS(paste0(folderpath,"/encounters_localized-low-.rds"))

deployments <- readRDS("Data/Fulmar/SUMMARY-DEPLOYMENTS_all_colonies.rds") #change to the path of the deployment file #!!#
deployments$Yt <- year(deployments$retrieval_date) - year(deployments$deployment_date) #number of years tracked


#If you're using the public data sample, only the MT logger is available, so just run that loop with logtype = "-no-"
for (logtype in c("-high-", "-no-", "-low-")){
  mydata <- readRDS(paste0(folderpath,"/encounters_localized",logtype,".rds"))
  row.names(mydata) <- NULL
  mydata$colony <- NA 
  mydata$Pn <- NA
  mydata$Yt <- NA
  mydata$NE <- NA
  row.names(mydata) <- NULL
  
  for (i in 1:nrow(mydata)){
    # Add colony column and number of tracks
    mydata$colony[i] <- deployments$colony[which(deployments$session_id == mydata$session_id[i])]
    mydata$Yt[i] <- deployments$Yt[which(deployments$session_id == mydata$session_id[i])] #number of year tracked
    mydata$Pn[i] <- as.numeric(mydata$nightend[i]-mydata$nightbeg[i])/24 #proportion of the night
    
    if(mydata$Pn[i] > 1){#when it's polar night
      mydata$Pn[i] <- 1
    }
  }
  Nw <- mydata %>% dplyr::group_by(colony) %>% dplyr::summarise(Nw = sum(Yt))
  
  for (i in 1:nrow(mydata)){
    mydata$NE[i] <- 1/(mydata$Pn[i]*Nw$Nw[which(Nw$colony == mydata$colony[i])])
  }
  
  normdata <- rbind(normdata, mydata)
}

row.names(normdata) <- NULL
normdata$NEnorm <- NA

for(j in 1:nrow(normdata)){
  normdata$NEnorm[j] <- normdata$NE[j]/min(normdata$NE) #Apply wheighing process
}

saveRDS(normdata, paste0(savingpath,"/encounters_final_global.rds"))
