rm(list = ls())
# Your WD #!!#
#setwd("C:/Users/dupui/Desktop/Shared_method_seabirds-fisheries")

#Note the DiveMove was removed from CRAN in January 2021, you can dowload the package here https://cran.r-project.org/src/contrib/Archive/diveMove/
#~~~~~~#
library(ggplot2) ; library(cowplot) ; library(diveMove) ; library(dplyr) ; library(lubridate)
#~~~~~~#

folderpath <- "Data/Fulmar/Encounters" #change to the output path of script 4. #!!#
mysavingpath <- "Data/Fulmar/Encounters" #change to the path where you want the data to be saved #!!#

#If you're using the public data sample, only the Biotrack low logger is available, so just run that loop with m = "Biotrack-low-"
for(m in c("MT-no-","Biotrack-low-", "Biotrack-high-")){
  # Calculate BEC ----
  # Load data 
  encounters <- readRDS(paste0(folderpath,"/encounters_V1_",m,".rds"))
  row.names(encounters) <- NULL
  
  #We followed instructions for the bouts.mle function availaible here:
  #https://cran.r-project.org/web/packages/diveMove/diveMove.pdf
  
  #Remove isolated encounters, here only encounter separated by less than 24 hours
  diffencounter <- na.omit(encounters$inter[encounters$inter < 1440])
  
  lnfreq <- boutfreqs(diffencounter,
                      bw = 10,
                      method = "standard",
                      plot = TRUE)
  
  startval <- boutinit(lnfreq, 80)
  
  p <- startval[[1]]["a"] / (startval[[1]]["a"] + startval[[2]]["a"])
  
  ## Fit the reparameterized (transformed parameters) model
  ## Drop names by wrapping around as.vector()
  init.parms <- list(p=as.vector(logit(p)),
                     lambda1=as.vector(log(startval[[1]]["lambda"])),
                     lambda2=as.vector(log(startval[[2]]["lambda"])))
  bout.fit1 <- bouts.mle(bouts2.LL, start=init.parms, x=diffencounter,
                         method="L-BFGS-B", lower=c(-2, -5, -10))
  coefs <- as.vector(coef(bout.fit1))
  
  ## Un-transform and fit the original parameterization
  init.parms <- list(p=unLogit(coefs[1]), lambda1=exp(coefs[2]),
                     lambda2=exp(coefs[3]))
  bout.fit2 <- bouts.mle(bouts2.ll, x=diffencounter, start=init.parms,
                         method="L-BFGS-B", lower=rep(1e-08, 3),
                         control=list(parscale=c(1, 0.1, 0.01)))
  plotBouts(bout.fit2, diffencounter)
  
  ## Estimated BEC
  bec <- bec2(bout.fit2)
  
  # Apply BEC to the data ----
  encounters$indice <- seq.int(nrow(encounters))
  final <- data.frame()
  
  for (i in 1:length(table(encounters$session_id))) {
    mydf <- encounters %>%
      filter(session_id == names(table(encounters$session_id))[i])
    mydf$bouts <- NA
    
    ## Label bouts
    mydf$bouts <- diveMove::labelBouts(mydf$cum, rep(bec, nrow(mydf)),
                                       bec.method="seq.diff")
    mydf <- mydf %>% dplyr::group_by(bouts) %>% dplyr::summarise(date = min(date),
                                                                 tfirst = min(tfirst),
                                                                 tend = max(tend),
                                                                 mean_light = (sum(mean_light*duration)/sum(duration)),
                                                                 mean_conductivity = (sum(mean_conductivity*duration)/sum(duration)),
                                                                 nightbeg = min(nightbeg),
                                                                 nightend = max(nightend),
                                                                 individ_id = max(individ_id),
                                                                 session_id = max(session_id),
                                                                 duration = sum(duration),
                                                                 nightlength = min(nightlength),
                                                                 encounter = sum(encounter),
                                                                 inter = max(inter),
                                                                 producer = min(producer))
    final <- rbind(final, mydf)
  }
  
  final$duration <- time_length(final$tend-final$tfirst,unit = "minute")+10 #Because we work with data recorded on 10min interval
  final <- as.data.frame(final)
  if(m == "MT-no-"){final$class <- "-no-"}
  if(m == "Biotrack-low-"){final$class <- "-low-"}
  if(m == "Biotrack-high-"){final$class <- "-high-"}
  row.names(final) <- NULL
  saveRDS(final, paste0(mysavingpath,"/encounters_BEC_",final$producer[1],final$class[1],".rds"))
}
