rm(list = ls())
# Your WD #!!#
#setwd("C:/Users/dupui/Desktop/Shared_method_seabirds-fisheries")

#~~~~~~#
library(lubridate) ;  library(dplyr) ; library(tidyr);
library(stringr) ; library(data.table)
#~~~~~~~#

folderpath <- "Data/Fulmar/Encounters" #change to the output path of script 5. #!!#
mysavingpath <- "Data/Fulmar/Encounters" #change to the path where you want the data to be saved #!!#

# Locations data
loc <- readRDS("Data/Fulmar/Output_GLS_&_IRMA_locations_Fulmarus_glacialis_2020-04-30.rds") #path to the location file #!!#
loc$timestamp <- with_tz(loc$timestamp,tzone="GMT") #to fix TZ
loc <- loc %>% dplyr::select(individ_id,timestamp,lon,lat,loc_type)
loc$individ_id <- str_replace(loc$individ_id, "-", "_") #fix annotation

# Encounters data
#If you're using the public data sample, only the Biotrack Low logger is available, so just run that loop with m = "Biotrack-low-"
for(m in c("MT-no-","Biotrack-low-", "Biotrack-high-")){
  encounters <- readRDS(paste0(folderpath,"/encounters_BEC_",m,".rds"))
  row.names(encounters) <- NULL
  encounters$tfirst <- force_tz(encounters$tfirst,tzone="GMT") #check TZ correction
  encounters$tend<- force_tz(encounters$tend,tzone="GMT")
  encounters$nightbeg <- force_tz(as.POSIXct(encounters$nightbeg),tzone="GMT")
  encounters$nightend <- force_tz(as.POSIXct(encounters$nightend),tzone="GMT")
  
  # Only keep 1 loc per day (the midnight one)
  myloc <- loc %>%
    filter(hour(timestamp) > 15 | hour(timestamp) < 4)
  
  
  # Merging data with locations
  z <- lapply(intersect(encounters$individ_id,myloc$individ_id),function(id) {
    encounters <- subset(encounters,individ_id==id)
    myloc <- subset(myloc,individ_id==id)
    
    encounters$indices <- sapply(encounters$tfirst,function(d) which.min(abs(myloc$timestamp - d)))
    myloc$indices <- 1:nrow(myloc)
    
    merge(encounters,myloc,by=c('individ_id','indices'))
  })
  
  mydataloc <- do.call(rbind,z)
  mydataloc$indices <- NULL
  row.names(mydataloc) <- NULL
  saveRDS(mydataloc, paste0(mysavingpath,"/encounters_localized",encounters$class[1],".rds"))
}
