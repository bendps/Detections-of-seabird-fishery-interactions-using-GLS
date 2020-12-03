rm(list = ls())
#Your WD #!!#
setwd("C:/Users/dupui/Documents/Internship_M1/Shared_method_seabirds-fisheries")

#~~~~~~#
library(tidyverse)
#~~~~~~#

# Global process
# - for a given detection, get its caracteristics
# - open the corresponding file
# - translate in lines logger's data
# - random sampling between 1 and n-duration
# - Check potential overlap with detection:
# 	--> if yes, sample again
# 	--> if no, control is okay

#Load detections 
mydetect <- readRDS("Data/Fulmar/detect_final_zoned.rds") #change to the output file of script n°12 #!!#
mydetect<- mydetect[-which(is.na(mydetect$zone)),]

folderpath <- "C:/Users/dupui/Documents/Internship_M1/R/Data/Fulmar/Final/Located" #Change to the output path of script n°13 #!!#
mycontrol <- numeric()
diffreq <- numeric()

#~~~~~~#
# LOOP #
#~~~~~~#

for(i in 1:nrow(mydetect)){
  # Load the corresponding session file
  data.files <- list.files(folderpath, pattern = mydetect$session_id[i]) 
  mydata <- readRDS(paste0(folderpath,"/",data.files))
  
  # Caracteristics of the detection
  mydur <- mydetect$duration[i]
  myzone <- mydetect$zone[i]
  
  # Check the frequency of recording and the number of lines to consider
  myfreq <- as.numeric(mydata$date_time[2]-mydata$date_time[1])
  mylines <- mydur/myfreq
  
  if(myfreq != 10){diffreq <- c(diffreq,mydata$model[1])} #allows to check if we indeed record every 10' for all loggers at that point [EDIT: indeed some models are recording every 5 min but it doesn't matter here]
  
  # Random sampling
  myori <- sample(1:(nrow(mydata)-mylines),1)
  mysample <- mydata[myori:(myori+mylines-1),]
  
  #Check the zone and the overlap and resample 
  while(all(mysample$zone %in% myzone) == FALSE & sum(mysample$detect_boat) != 0){
    myori <- sample(1:nrow(mydata)-mylines,1)
    mysample <- mydata[myori:(myori+mylines-1),]
  }
  
  mycontrol <- rbind(mycontrol,mysample)
  print(i)
}

#List of logger recording every 5'
levels(as.factor(diffreq))

#Conductivity to activity
mycontrol$activity_cat <- NA
mycontrol$activity_cat[which(mycontrol$std_conductivity > 0.95)] <- "Floating"
mycontrol$activity_cat[which(mycontrol$std_conductivity < 0.05)] <- "Flying"
mycontrol$activity_cat[which(mycontrol$std_conductivity <= 0.95 & mycontrol$std_conductivity >= 0.05)] <- "Foraging"
mycontrol<- mycontrol[-which(is.na(mycontrol$activity_cat)),]


#Group to compare to the data
grpcontrol <- mycontrol %>% group_by(zone, activity_cat, individ_id) %>% summarise(n = n())
grpcontrol <- grpcontrol[-which(is.na(grpcontrol$zone)),]

grpcontrol$tot <- NA
grpcontrol$prop <- NA

grptemp <- grpcontrol %>% group_by(individ_id,zone) %>% summarise(tot = sum(n))

for(i in 1:nrow(grpcontrol)){
  grpcontrol$tot[i] <- grptemp$tot[which(grptemp$zone == grpcontrol$zone[i] & grptemp$individ_id == grpcontrol$individ_id[i])]
  
}
grpcontrol$prop <- grpcontrol$n/grpcontrol$tot*100

#Correct zone names
grpcontrol$zone <- as.character(grpcontrol$zone)
grpcontrol$zone[grpcontrol$zone == "Canadian Eastern Arctic - West Greenland"] <- "Canadian E. A."
grpcontrol$zone[grpcontrol$zone == "Iceland Shelf and Sea"] <- "Iceland"
grpcontrol$zone <- as.factor(grpcontrol$zone)

grpcontrol$zone <- ordered(grpcontrol$zone, levels = c("Canadian E. A.",
                                               "Labrador Sea",
                                               "Atlantic Ocean",
                                               "Greenland Sea",
                                               "Iceland",
                                               "Faroe Plateau",
                                               "North Sea",
                                               "Norwegian Sea",
                                               "Barents Sea",
                                               "Kara Sea"))

#Save
saveRDS(grpcontrol, "Data/Fulmar/control_stat_encounter.rds") #Change to the desired output #!!#
