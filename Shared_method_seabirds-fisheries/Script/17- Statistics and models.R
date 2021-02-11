rm(list = ls())
#Your WD #!!#
setwd("C:/Users/dupui/Documents/Internship_M1/Shared_method_seabirds-fisheries")

#~~~~~~#
library(dplyr) ; library(lubridate) ; library(ggplot2)
library(nlme) ; library(lme4) ; library(multcomp)
#~~~~~~#

#Binomial Model probability of detection ####
# Here we have "detect_night" as response variable = 0 if no encounter, 
#                                                    1 if at leat one

# data 

TAB <- readRDS("Data/Fulmar/stack_files.rds") #Change to the output of script n°15 #!!#

#Calcul night duration
TAB$nightdur <- as.numeric(difftime(TAB$end,TAB$begin, units ="hours"))

#Fix night that last for more than 24h
TAB$nightdur <- ifelse(TAB$nightdur> 24, 24, TAB$nightdur)

# Data exploration
barplot(table(TAB$detect_night))

plot(TAB$detect_night~TAB$nightdur)
abline(lm(TAB$detect_night~TAB$nightdur))

#Choose fixed effects
TAB$colony <- as.factor(TAB$colony) #Order colonies west to east
TAB$colony <- ordered(TAB$colony,  levels = c("Alkefjellet",
                                              "Breidafjordur & R.",
                                              "Langanes & S. & G.",
                                              "Papey & H.",
                                              "Jan Mayen",
                                              "Faroe Islands",
                                              "Eynhallow",
                                              "Jarsteinen",
                                              "Bjørnøya"))

#Null model 
mod0 <- glmer(detect_night ~ 1 + (1 | individ_id), family = binomial,
              data = TAB)
summary(mod0)
AIC(mod0)


#Most complex model
modA <- glmer(detect_night ~ colony + zone + nightdur + (1 | individ_id), family = binomial,
              data = TAB)
summary(modA)
AIC(modA)

#Simplification = remove factor 1 at a time to find the best model
modB <- glmer(detect_night ~ colony + zone  + (1 | individ_id), family = binomial,
              data = TAB)
summary(modB)
AIC(modB)

modC <- glmer(detect_night ~ colony + nightdur  + (1 | individ_id), family = binomial,
              data = TAB)
summary(modC)
AIC(modC)

modD <- glmer(detect_night ~ zone  + nightdur + (1 | individ_id), family = binomial,
              data = TAB)
summary(modD)
AIC(modD)

modE <- glmer(detect_night ~ colony  + (1 | individ_id), family = binomial,
              data = TAB)
summary(modE)
AIC(modE)

modF <- glmer(detect_night ~ zone  + (1 | individ_id), family = binomial,
              data = TAB)
summary(modF)
AIC(modF)

modG <- glmer(detect_night ~ nightdur  + (1 | individ_id), family = binomial,
              data = TAB)
summary(modG)
AIC(modG)

#Post hoc on modC for colonies and modD for zones
summary(glht(modC, linfct = mcp(colony = "Tukey")), test = adjusted("holm"))
pair <- glht(modC, linfct = mcp(colony = "Tukey")) #get the significance label of fig. 4.A

summary(glht(modD, linfct = mcp(zone = "Tukey")), test = adjusted("holm"))

#V2

#Duration model####
detect <- readRDS("Data/Fulmar/detect_final_zoned.rds") #Change to path were you saved output of script n°12
detect <- detect[-which(is.na(detect$zone)),]

detect$colony <- as.factor(detect$colony)
detect$class <- as.factor(detect$class)

detect$nightlength <- detect$nightlength/60 # night in hours
detect$nightlength <- ifelse(detect$nightlength>24, 24, detect$nightlength)
# random structure = 1|individ_id
# fixed effects = zone, colony, logger type, duration of the night

#Data exploration
hist(detect$duration)
detect$duration.log <- log(1+detect$duration)
hist(detect$duration.log)
detect$duration.sqrt <- sqrt(detect$duration)
hist(detect$duration.sqrt)

boxplot(detect$duration.log~detect$zone)

detect$colony <- as.factor(detect$colony)
detect$class <- as.factor(detect$class)

detect$nightlength <- detect$nightlength/60 # night in hours

# test binomial short vs long encounters

detect$DurCat <- as.factor(ifelse(detect$duration<=10, 'short', 'long'))
table(detect$DurCat)

# detect$nightlength2 <- scale(detect$nightlength)


# filter zones with very few points 

detect2 <- detect %>% filter(!(zone %in% c("Kara Sea", "Labrador Sea" , "Canadian Eastern Arctic - West Greenland")))

mod0 <- glmer(DurCat ~ 1 + (1 | individ_id), family = binomial,
              data = detect2)
AIC(mod0)


modA <- glmer(DurCat ~ colony + zone + class + (1 | individ_id), family = binomial,
              data = detect2)
# summary(modA)
AIC(modA)

modB <- glmer(DurCat ~ zone + class + (1 | individ_id), family = binomial, #  control = glmerControl(optimizer ="optim"),
              data = detect2)
AIC(modB)
summary(modB)
plot(modB)

modC <- glmer(DurCat ~ colony + class + (1 | individ_id), family = binomial,
              data = detect2)
AIC(modC)

modD <- glmer(DurCat ~  class + (1 | individ_id), family = binomial,
              data = detect2)
AIC(modD)

modE <- glmer(DurCat ~ zone +  (1 | individ_id), family = binomial,
              data = detect2)
AIC(modE)

modF <- glmer(DurCat ~ colony +  (1 | individ_id), family = binomial,
              data = detect2)
AIC(modF)

#Activity (bootstrap 95%CI) ####

meanfun <- function(data, i){
  
  d <- data[i, ]
  
  return(mean(d))  
  
}

#Load in your environment SUB1 and SUB2.
#These shall respectively your output files from script 15. regrouping for each individual the proportion spent in each activity during encounters and control.

library(boot)

#Flying#######

SUB2_f <- as.data.frame(subset(SUB2, activity_cat == "Flying"))

SUB1_f <- as.data.frame(subset(SUB1, activity_cat == "Flying"))

bo <- boot(SUB2_f[, "prop", drop = FALSE], statistic=meanfun, R=1000)

bo

boot.ci(bo, conf=0.95, type="bca")



bo <- boot(SUB1_f[, "prop", drop = FALSE], statistic=meanfun, R=1000)

bo

boot.ci(bo, conf=0.95, type="bca")


#Foraging #######

SUB2_fo <- as.data.frame(subset(SUB2, activity_cat == "Foraging"))

SUB1_fo <- as.data.frame(subset(SUB1, activity_cat == "Foraging"))



bo <- boot(SUB2_fo[, "prop", drop = FALSE], statistic=meanfun, R=1000)

bo

boot.ci(bo, conf=0.95, type="bca")



bo <- boot(SUB1_fo[, "prop", drop = FALSE], statistic=meanfun, R=1000)

bo

boot.ci(bo, conf=0.95, type="bca")


#Resting #######

SUB2_r <- as.data.frame(subset(SUB2, activity_cat == "Resting"))

SUB1_r <- as.data.frame(subset(SUB1, activity_cat == "Resting"))



bo <- boot(SUB2_r[, "prop", drop = FALSE], statistic=meanfun, R=1000)

bo

boot.ci(bo, conf=0.95, type="bca")



bo <- boot(SUB1_r[, "prop", drop = FALSE], statistic=meanfun, R=1000)

bo

boot.ci(bo, conf=0.95, type="bca")

#Old Activity model ####
mydf <- readRDS("Data/Fulmar/Behaviour/grpfinal.RDS")
deployments <- readRDS("Data/Fulmar/SUMMARY-DEPLOYMENTS_all_colonies.RDS")

#Group night, global and boat data from script n°15 path need to be changes according to the one of script 15
TAB <- rbind(readRDS("Data/Fulmar/Behaviour/grpNightdata.RDS")) #!!#
TAB$type <- "Night" #correspond to nights with encounters

TAB$individ_id <- NA #add indiv ID
for(i in 1:nrow(TAB)){
  if(is.na(TAB$individ_id[i])){
    TAB$individ_id[i] <- deployments$individ_id[which(deployments$session_id == TAB$session_id[i])]
  }
}

#Merge with the other dataset (global and boat corresponds to nights without encounters and encounters)
prov <- readRDS("Data/Fulmar/Behaviour/grpglobaldata.RDS")
prov$type <- "global"
TAB <- rbind(TAB,prov)

prov <- readRDS("Data/Fulmar/Behaviour/grpboatdata.RDS")
prov$type <- "boat"
TAB <- rbind(TAB,prov)

#Correct zone, categories and activity names
TAB$zone <- as.character(TAB$zone)
TAB$zone[TAB$zone == "Canadian Eastern Arctic - West Greenland"] <- "Canadian E. A."
TAB$zone[TAB$zone == "Iceland Shelf and Sea"] <- "Iceland"
TAB$zone <- as.factor(TAB$zone)

TAB$zone <- ordered(TAB$zone, levels = c("Canadian E. A.",
                                                 "Labrador Sea",
                                                 "Atlantic Ocean",
                                                 "Greenland Sea",
                                                 "Iceland",
                                                 "Faroe Plateau",
                                                 "North Sea",
                                                 "Norwegian Sea",
                                                 "Barents Sea",
                                                 "Kara Sea"))

TAB$type <- as.character(TAB$type)
TAB$type[TAB$type == "boat"] <- "Encounters"
TAB$type[TAB$type == "global"] <- "Nights"
TAB$type[TAB$type == "Night"] <- "Nights detected"
TAB$type <- as.factor(TAB$type)

TAB$session_id <- as.factor(TAB$session_id)
TAB$individ_id <- as.factor(TAB$individ_id)

TAB$activity_cat <- as.character(TAB$activity_cat)
TAB$activity_cat[TAB$activity_cat == "Dry"] <- "Flying"
TAB$activity_cat[TAB$activity_cat == "Foraging"] <- "Foraging"
TAB$activity_cat[TAB$activity_cat == "Wet"] <- "Floating"
TAB$activity_cat <- as.factor(TAB$activity_cat)
TAB$activity_cat <- ordered(TAB$activity_cat, levels = c("Floating",
                                                                 "Foraging",
                                                                 "Flying"))

mydata <- TAB

#Compare encounters with control / nights with or without detections
mydata <- subset(TAB, type == "Encounters") #adapt the subset, here were check encounters and their control

#Only if need encounters control! 
mycontrol <- readRDS("C:/Users/dupui/Documents/Internship_M1/R/Data/Fulmar/control_stat_encounter.rds")
mycontrol$type <- "Control"
mydata <- mydata[,-c(1,2)]
mydata <- rbind(mydata,mycontrol)

mydata <- subset(mydata, activity_cat == "Foraging") #Test one activity at a time

mydata$type <- factor(mydata$type)
mydata$individ_id <- factor(mydata$individ_id)

mydata$zone <- factor( mydata$zone , ordered = FALSE )
mydata$activity_cat <- factor( mydata$activity_cat , ordered = FALSE )

#Data exploration
hist(mydata$prop)
mydata$prop.arcsin <- asin(sqrt(mydata$prop / 100)) #arcsin transformation
hist(mydata$prop.arcsin)

boxplot(mydata$prop.arcsin ~ mydata$zone)

#Random is individ_id and fixed zone, type de detection, activity cat
#Model null
mod0 <- lme(prop.arcsin ~ 1 , random =  ~1|individ_id, data = mydata,
            method = "ML")
summary(mod0)
AIC(mod0)


#Most complex model
mod1 <- lme(prop.arcsin ~  zone, random =  ~1|individ_id, data = mydata,
            method = "ML")
summary(mod1)
AIC(mod1)

summary(glht(mod1, linfct = mcp(zone = "Tukey")), test = adjusted("holm"))


