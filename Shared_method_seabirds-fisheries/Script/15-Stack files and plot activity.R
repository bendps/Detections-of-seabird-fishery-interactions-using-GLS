rm(list = ls())
#Your WD #!!#
#setwd("C:/Users/dupui/Documents/Internship_M1/Shared_method_seabirds-fisheries")

#~~~~~~#
library(dplyr); library(lubridate); library(ggplot2)
library(tidyverse); library(cowplot); library(plyr)
library(viridis)
#~~~~~~#

#Creating of Stacked file of all the data ####
#Change these paths to the one of 1.Deployment file 2.the saving path of script n°13 #!!#
deployments <- readRDS("Data/Fulmar/SUMMARY-DEPLOYMENTS_all_colonies.rds")
folderpath <- "Data/Fulmar/Final/Located"

data.files <- list.files(folderpath, pattern = "DATA")
mydf <- NULL

  for (i in 1:length(data.files)) {
    mydf<- rbind(mydf,readRDS(paste0(folderpath,"/",data.files[i])))
    print(i)
  }

mydf <- mydf[which(!is.na(mydf$zone)),]
saveRDS(mydf, "Data/Fulmar/stack_files.rds") #Change to the desire output path #!!#

#Example: using the stacked file to compare zone conductivity ####
actzone <- data.frame()
mydf <- readRDS("Data/Fulmar/stack_files.rds") #Use the output path of the stacked file #!!#

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

saveRDS(actzone, "Data/Fulmar/activity_zone_compare.rds") #Change to the desired output path #!!#

#Figure of activity comparison depending on light (fig.3)####

data <- readRDS("Data/Fulmar/stack_files.rds") #Change to the outpu of the first part of this script #!!#

#Nights without detections data ####
mydata <- subset(data, detect_night == 0)
grpdata <- mydata %>% group_by(session_id,mydate,activity_cat, zone) %>%
  summarise(n = n(), individ_id = min(individ_id))

grpgen <- mydata %>% group_by(session_id,mydate, zone) %>% summarise(n = n())

for(i in 1:nrow(grpdata)){
  x <- subset(grpgen, session_id == grpdata$session_id[i] & mydate == grpdata$mydate[i] & zone == grpdata$zone[i])
  grpdata$tot[i] <- x$n
  if(nrow(x)>1){print(paste("error",i))}
}
grpdata$prop <- grpdata$n/grpdata$tot*100
saveRDS(grpdata,"Data/Fulmar/Behaviour/grpglobaldata.RDS") #Can be changed to the desired output path #!!#

myglobal <- grpdata %>% group_by(activity_cat) %>% summarise(mean = mean(prop), sd = sd(prop), n = n(), type = "global")

myglobal$se <- myglobal$sd/sqrt(myglobal$n)

#1.Nights with detections data ####
mydata <- subset(data, detect_night == 1)
grpdata <- mydata %>% group_by(session_id,mydate,activity_cat, zone) %>%
  summarise(n = n())

grpgen <- mydata %>% group_by(session_id,mydate, zone) %>% summarise(n = n())

for(i in 1:nrow(grpdata)){
  x <- subset(grpgen, session_id == grpdata$session_id[i] & mydate == grpdata$mydate[i] & zone == grpdata$zone[i])
  grpdata$tot[i] <- x$n
  if(nrow(x)>1){print(paste("error",i))}
}
grpdata$prop <- grpdata$n/grpdata$tot*100
saveRDS(grpdata,"Data/Fulmar/Behaviour/grpNightdata.RDS") #Can be changed to the desired output path #!!#

myNight <- grpdata %>% group_by(activity_cat) %>% summarise(mean = mean(prop), sd = sd(prop), n = n(), type = "Night")

myNight$se <- myNight$sd/sqrt(myNight$n)

#2.Encounter data ####
mydata <- subset(data, detect_night == 1 & detect_boat == 1)
grpdata <- mydata %>% group_by(individ_id,activity_cat, zone) %>%
  summarise(n = n())

grpgen <- mydata %>% group_by(individ_id, zone) %>% summarise(n = n())

for(i in 1:nrow(grpdata)){
  x <- subset(grpgen, individ_id == grpdata$individ_id[i] & zone == grpdata$zone[i])
  grpdata$tot[i] <- x$n
  if(nrow(x)>1){print(paste("error",i))}
}
grpdata$prop <- grpdata$n/grpdata$tot*100
saveRDS(grpdata,"Data/Fulmar/Behaviour/grpboatdata.RDS") #Can be changed to the desired output path #!!#

myboat <- grpdata %>% group_by(activity_cat) %>% summarise(mean = weighted.mean(x = prop, w = tot), sd = sd(prop), n = n(), type = "boat")

myboat$se <- myboat$sd/sqrt(myboat$n)

#3.Control data ####
mydata <- readRDS("Data/Fulmar/control_stat_encounter.rds") #Change to the output file of script n°14 #!!#

grpdata <- mydata %>% group_by(individ_id,activity_cat, zone) %>%
  summarise(n = n())

grpgen <- mydata %>% group_by(individ_id, zone) %>% summarise(n = n())

for(i in 1:nrow(grpdata)){
  x <- subset(grpgen, individ_id == grpdata$individ_id[i] & zone == grpdata$zone[i])
  grpdata$tot[i] <- x$n
  if(nrow(x)>1){print(paste("error",i))}
}
grpdata$prop <- grpdata$n/grpdata$tot*100
saveRDS(grpdata,"Data/Fulmar/Behaviour/grpcontroldata.RDS") #Can be changed to the desired output path #!!#

mycontrol <- grpdata %>% group_by(activity_cat) %>% summarise(mean = weighted.mean(x = prop, w = tot), sd = sd(prop), n = n(), type = "Control")

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
myfinal$activity_cat[myfinal$activity_cat == "Dry"] <- "Flying"
myfinal$activity_cat[myfinal$activity_cat == "Foraging"] <- "Foraging"
myfinal$activity_cat[myfinal$activity_cat == "Wet"] <- "Resting"
myfinal$activity_cat <- as.factor(myfinal$activity_cat)
myfinal$activity_cat <- ordered(myfinal$activity_cat, levels = c("Resting",
                                                                 "Foraging",
                                                                 "Flying"))

saveRDS(myfinal,"Data/Fulmar/Behaviour/grpfinal.RDS") #Can be changed to the desired output path #!!#

#5.Plot ####
#Classic bw theme of ggplot2

myfinal <- readRDS("Data/Fulmar/Behaviour/grpfinal.RDS") #Change to the output path just above #!!#

ggplot(myfinal, aes(x = activity_cat, y = mean, ymin = mean-se, ymax = mean+se, col = activity_cat)) + 
  geom_point(stat = "identity", size = 5)+
  geom_errorbar(width = 0.6, size = 0.8)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Mean percentage", col = "Activity")+
  facet_grid(~type, labeller = label_wrap_gen(width=5))+
  scale_color_viridis(discrete = T)+
  theme_bw()+
  theme(text = element_text(size = 30),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black"),
        panel.grid = element_line(color = "#C4C4C4"),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom")

ggsave(filename = "fig3.svg", units = "in", width = 16, height = 9)

  
