
#~~~~~~~#
library(tidyverse);library(suncalc);library(lubridate);library(data.table)
#~~~~~~~#

folderpath <- "Data/Fulmar/Encounters" 
savingpath <- "Data/Fulmar/Encounters"
normdata <- NULL

#Load the data
#no <- readRDS(paste0(folderpath,"/encounters_localized-no-.rds")) 
#high <- readRDS(paste0(folderpath,"/encounters_localized-high-.rds"))
low <- readRDS(paste0(folderpath,"/encounters_localized-low-.rds"))

deployments <- readRDS("Data/Fulmar/SUMMARY-DEPLOYMENTS_all_colonies.rds") 
deployments$Yt <- year(deployments$retrieval_date) - year(deployments$deployment_date) #number of years tracked

for (logtype in c("-low-")){  # c("-high-", "-no-", "-low-")
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
