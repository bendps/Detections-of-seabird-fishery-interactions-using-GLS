rm(list = ls())

#~~~~~~#
library(lubridate)
library(tidyverse); library(cowplot); library(plyr)
library(viridis)
#~~~~~~#

#Creating of Stacked file of all the data ####
deployments <- readRDS("Data/Fulmar/SUMMARY-DEPLOYMENTS_all_colonies.rds")
folderpath <- "Data/Fulmar/Final/Located"

data.files <- list.files(folderpath, pattern = "DATA")
mydf <- NULL

  for (i in 1:length(data.files)) {
    mydf<- rbind(mydf,readRDS(paste0(folderpath,"/",data.files[i])))
    print(i)
  }

mydf$activity_cat <- NA
mydf$activity_cat[which(mydf$std_conductivity > 0.95)] <- "resting"
mydf$activity_cat[which(mydf$std_conductivity < 0.05)] <- "flying"
mydf$activity_cat[which(mydf$std_conductivity <= 0.95 & mydf$std_conductivity >= 0.05)] <- "foraging"

mydf <- mydf[which(!is.na(mydf$zone)),]
saveRDS(mydf, "Data/Fulmar/stack_files.rds") 

#Example: using the stacked file to compare zone conductivity ####
actzone <- data.frame()
mydf <- readRDS("Data/Fulmar/stack_files.rds") 

  for(y in names(table(mydf$zone))){
    mydfzone <- subset(mydf, zone == y) #We proceed by zone
    
    #Data during encounters
    meanboat <- mean(na.omit(mydfzone$std_conductivity[which(mydfzone$detect_boat == 1)]))
    sdboat <- sd(na.omit(mydfzone$std_conductivity[which(mydfzone$detect_boat == 1)]))
    nboat <- length(which(!is.na(mydfzone$std_conductivity[which(mydfzone$detect_boat == 1)])))
    seboat <- sdboat/sqrt(nboat)
    boat <- c(y,meanboat,sdboat,"boat",nboat,seboat)
    
    #Data of nights during encounters
    meannight <- mean(na.omit(mydfzone$std_conductivity[which(mydfzone$detect_night == 1)]))
    sdnight <- sd(na.omit(mydfzone$std_conductivity[which(mydfzone$detect_night == 1)]))
    nnight <- length(which(!is.na(mydfzone$std_conductivity[which(mydfzone$detect_night == 1)])))
    senight <- sdnight/sqrt(nnight)
    night <- c(y,meannight,sdnight,"night",nnight,senight)
    
    #General data
    meanglobal <- mean(na.omit(mydfzone$std_conductivity))
    sdglobal <- sd(na.omit(mydfzone$std_conductivity))
    nglobal <- length(which(!is.na(mydfzone$std_conductivity)))
    seglobal <- sdglobal/sqrt(nglobal)
    global <- c(y,meanglobal,sdglobal,"global",nglobal,seglobal)
    
    #Data of night without encounters
    meannight0 <- mean(na.omit(mydfzone$std_conductivity[which(mydfzone$detect_night == 0)]))
    sdnight0 <- sd(na.omit(mydfzone$std_conductivity[which(mydfzone$detect_night == 0)]))
    nnight0 <- length(which(!is.na(mydfzone$std_conductivity[which(mydfzone$detect_night == 0)])))
    senight0 <- sdnight0/sqrt(nnight0)
    night0 <- c(y,meannight0,sdnight0,"night0",nnight0,senight0)
    
    actzone<- rbind(actzone,rbind(boat,night,global,night0))
  }

colnames(actzone) <- c("zone", "mean", "sd", "type", "n","se")
rownames(actzone) <- NULL

actzone$mean <- as.numeric(as.character(actzone$mean))
actzone$sd <- as.numeric(as.character(actzone$sd))
actzone$n <- as.numeric(as.character(actzone$n))
actzone$se <- as.numeric(as.character(actzone$se))

saveRDS(actzone, "Data/Fulmar/activity_zone_compare.rds") 

data <- readRDS("Data/Fulmar/stack_files.rds") 

data$activity_cat <- as.factor(data$activity_cat)
#Nights without detections data ####
mydata <- subset(data, detect_night == 0)

grpdata <- mydata %>% group_by(session_id, mydate, activity_cat, individ_id, .drop = F) %>%
  dplyr::summarise(n = n())

grpgen <- mydata %>% dplyr::group_by(session_id,mydate) %>% dplyr::summarise(n = n())

grpdata$tot <- NA
for(i in 1:nrow(grpdata)){
  grpdata$tot[i] <- grpgen$n[which(grpgen$session_id == grpdata$session_id[i] & grpgen$mydate == grpdata$mydate[i])]
}

grpdata$prop <- grpdata$n/grpdata$tot*100
saveRDS(grpdata,"Data/Fulmar/Behaviour/grpglobaldata.RDS") 

myglobal <- grpdata %>% group_by(activity_cat) %>% dplyr::summarise(mean = mean(prop), sd = sd(prop), n = n(), type = "global")

myglobal$se <- myglobal$sd/sqrt(myglobal$n)

#1.Nights with detections data ####
mydata <- subset(data, detect_night == 1)
grpdata <- mydata %>% group_by(session_id,mydate,activity_cat, individ_id, .drop = F) %>%
  dplyr::summarise(n = n())

grpgen <- mydata %>% group_by(session_id, mydate) %>% dplyr::summarise(n = n())

grpdata$tot <- NA
for(i in 1:nrow(grpdata)){
  grpdata$tot[i] <- grpgen$n[which(grpgen$session_id == grpdata$session_id[i] & grpgen$mydate == grpdata$mydate[i])]
}

grpdata$prop <- grpdata$n/grpdata$tot*100
saveRDS(grpdata,"Data/Fulmar/Behaviour/grpNightdata.RDS") 

myNight <- grpdata %>% group_by(activity_cat) %>% dplyr::summarise(mean = mean(prop), sd = sd(prop), n = n(), type = "Night")

myNight$se <- myNight$sd/sqrt(myNight$n)

#2.Encounter data ####
mydata <- subset(data, detect_night == 1 & detect_boat == 1)
grpdata <- mydata %>% group_by(individ_id,activity_cat, .drop = F) %>%
  dplyr::summarise(n = n())

grpgen <- mydata %>% group_by(individ_id) %>% dplyr::summarise(n = n())

grpdata$tot <- NA
for(i in 1:nrow(grpdata)){
  grpdata$tot[i] <- grpgen$n[which(grpgen$individ_id == grpdata$individ_id[i])]
}

grpdata$prop <- grpdata$n/grpdata$tot*100
saveRDS(grpdata,"Data/Fulmar/Behaviour/grpboatdata.RDS") 

myboat <- grpdata %>% group_by(activity_cat) %>% dplyr::summarise(mean = weighted.mean(x = prop, w = tot), sd = sd(prop), n = n(), type = "boat")

myboat$se <- myboat$sd/sqrt(myboat$n)

#3.Control data ####
mydata <- readRDS("Data/Fulmar/control_stat_encounter.rds") 

grpdata <- mydata %>% group_by(individ_id,activity_cat, .drop = F) %>%
  dplyr::summarise(n = n())

grpgen <- mydata %>% group_by(individ_id) %>% dplyr::summarise(n = n())

grpdata$tot <- NA
for(i in 1:nrow(grpdata)){
  grpdata$tot[i] <- grpgen$n[which(grpgen$individ_id == grpdata$individ_id[i])]
}

grpdata$prop <- grpdata$n/grpdata$tot*100
saveRDS(grpdata,"Data/Fulmar/Behaviour/grpcontroldata.RDS") 

mycontrol <- grpdata %>% group_by(activity_cat) %>% dplyr::summarise(mean = weighted.mean(x = prop, w = tot), sd = sd(prop), n = n(), type = "Control")

mycontrol$se <- mycontrol$sd/sqrt(mycontrol$n)

#4.Encounterfinal ####
myfinal <- rbind(myboat,myNight,myglobal,mycontrol)

#Rename categories for understanding
myfinal$type <- as.character(myfinal$type)
myfinal$type[myfinal$type == "boat"] <- "Encounters"
myfinal$type[myfinal$type == "global"] <- "Nights"
myfinal$type[myfinal$type == "Night"] <- "Nights detected"
myfinal$type <- as.factor(myfinal$type)

myfinal$type <- ordered(myfinal$type, levels = c("Nights",
                                                 "Nights detected",
                                                 "Encounters",
                                                 "Control"))

myfinal$type <- plyr::revalue(myfinal$type, c("Encounters"="During encounters",
                                              "Nights detected"="Nights with encounters",
                                              "Nights"="Nights without encounters",
                                              "Control"="Encounter controls"))

#Same with activities
myfinal$activity_cat <- as.character(myfinal$activity_cat)
myfinal$activity_cat[myfinal$activity_cat == "flying"] <- "Flying"
myfinal$activity_cat[myfinal$activity_cat == "foraging"] <- "Foraging"
myfinal$activity_cat[myfinal$activity_cat == "resting"] <- "Resting"
myfinal$activity_cat <- as.factor(myfinal$activity_cat)
myfinal$activity_cat <- ordered(myfinal$activity_cat, levels = c("Resting",
                                                                 "Foraging",
                                                                 "Flying"))

saveRDS(myfinal,"Data/Fulmar/Behaviour/grpfinal.RDS") 

#5.Plot ####
#Classic bw theme of ggplot2

myfinal <- readRDS("Data/Fulmar/Behaviour/grpfinal.RDS") 


