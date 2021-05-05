
#~~~~~~#
library(dplyr) ; library(lubridate) ; library(ggplot2)
library(nlme) ; library(lme4) ; library(multcomp)
#~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#A) Binomial Model probability of detection ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Here we have "detect_night" as response variable = 0 if no encounter, 
#                                                    1 if at leat one

# data 

TAB <- readRDS("Data/Fulmar/stack_files.rds") 

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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#B) Duration of encounters ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#C) Activity ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Activity (bootstrap 95%CI) ####

meanfun <- function(data, i){
  
  d <- data[i, ]
  
  return(mean(d))  
  
}

#Load in your environment SUB1 and SUB2.
#These shall respectively your output files from script 15. regrouping for each individual the proportion spent in each activity during encounters and control.

library(boot)


# encounters
SUB1 <- readRDS("Data/Fulmar/Behaviour/grpboatdata.RDS")

# controls 
SUB2 <- readRDS("Data/Fulmar/Behaviour/grpcontroldata.RDS")


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




