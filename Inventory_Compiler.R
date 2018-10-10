
# compiles inventory plot data to stand level estimates 
# PNW Forestry 
# Al Pancoast 
# June 2017

# TPA and Basal area by species and DBH class are done by changing code maually for now - automate later if needed
# Volume is not right - seperate script for volume and merchandising is used and results combined later


start <- Sys.time()

library(tidyverse)

RawData <- read.csv("Plot_Data.csv")
Overview <- read.csv("Stand_Info.csv")
SiteIndex <- read.csv("Site_Trees.csv")

#Prepare data - get defects and veg from whole numbers into % 

RawData <- RawData %>%
  mutate(REPROD_COUNT = ifelse(is.na(REPROD_COUNT),1,REPROD_COUNT)) %>%
  mutate(`VEG_pct` = ifelse(is.na(`VEG_pct`),0,`VEG_pct`)) %>%
  mutate(DEFECT_B = ifelse(is.na(DEFECT_B),0,DEFECT_B)) %>%
  mutate(DEFECT_M = ifelse(is.na(DEFECT_M),0,DEFECT_M)) %>%
  mutate(DEFECT_T = ifelse(is.na(DEFECT_T),0,DEFECT_T))

RawData$SPECIES[which(RawData$SPECIES == "H")] = "F"

RawData$`VEG_pct` <- RawData$`VEG_pct`/100

# Create summary tables ---------------------------------------------------

#how many stands do we have?

nstands <- RawData %>% 
  group_by(STAND_ID)%>%
  count(unique(STAND_ID)) 

#Set baseline cruise detials 

n <- as.numeric(nrow(nstands)) # nummber of stands 
RegenEF <- 100 # expansion factor for 1/100th acre fixed plot - regen tally
SnagEF <- 5 # expansion factor for 1/5th acre fixed plot - snags
BH_age <- 7 # assumed 7 years to reach breast height - Ponderosa Pine, Ochocco region


#create empty summary table with space for all desired variables 

PlotSummary <- as.data.frame(matrix(nrow = n ,ncol = 38))
colnames(PlotSummary)[1] <- "STD"
PlotSummary$STD <- nstands$STAND_ID
colnames(PlotSummary)[2] <- "n_Plots"
colnames(PlotSummary)[3] <- "MerchTC"
colnames(PlotSummary)[4] <- "MerchSE"
colnames(PlotSummary)[5] <- "MerchBA_ac"
colnames(PlotSummary)[6] <- "MerchTPA"
colnames(PlotSummary)[7] <- "MerchVol_ac" # not working right here, do volumes and merchandising in seperate script and combine later
colnames(PlotSummary)[8] <- "TotalValue"
colnames(PlotSummary)[9] <- "QMD"
colnames(PlotSummary)[10] <- "PP_regenTPA"
colnames(PlotSummary)[11] <- "DF_regenTPA"
colnames(PlotSummary)[12] <- "WJ_regenTPA"
colnames(PlotSummary)[13] <- "Total_regenTPA"
colnames(PlotSummary)[14] <- "%Grass"
colnames(PlotSummary)[15] <- "%Forb"
colnames(PlotSummary)[16] <- "%Shrub"
colnames(PlotSummary)[17] <- "Snags_ac"
colnames(PlotSummary)[18] <- "AveSnagDecay"
colnames(PlotSummary)[19] <- "AveSnagDBH"
colnames(PlotSummary)[20] <- "AveSnagHT"
colnames(PlotSummary)[21] <- "AveRegenCrownRatio"
colnames(PlotSummary)[22] <- "AveMerchCrownRatio"
colnames(PlotSummary)[23] <- "AveButtDefect"
colnames(PlotSummary)[24] <- "AveMidDefect"
colnames(PlotSummary)[25] <- "AveTopDefect"
colnames(PlotSummary)[26] <- "StandAge"
colnames(PlotSummary)[27] <- "PPSiteIndex"
colnames(PlotSummary)[28] <- "DFSiteIndex"
colnames(PlotSummary)[29] <- "RadGrowth"
colnames(PlotSummary)[30] <- "BAF"
colnames(PlotSummary)[31] <- "PP_BA" # will be blank - changed by hand for now - automate later if needed
colnames(PlotSummary)[32] <- "PP_TPA" # will be blank - changed by hand for now - automate later if needed
colnames(PlotSummary)[33] <- "DF_BA" # will be blank - changed by hand for now - automate later if needed
colnames(PlotSummary)[34] <- "DF_TPA" # will be blank - changed by hand for now - automate later if needed
colnames(PlotSummary)[35] <- "WJ_BA" # will be blank - changed by hand for now - automate later if needed
colnames(PlotSummary)[36] <- "WJ_TPA" # will be blank - changed by hand for now - automate later if needed
colnames(PlotSummary)[37] <- "Total_BA"
colnames(PlotSummary)[38] <- "Total_TPA"

################################################
# Start loop that summarizes data stand by stand  
################################################

# Maybe not most efficient approach but it works
# for each stand - filter attribute and fill in appropriate col and row, then on to next stand

# for TPA and basal area by species and DBH class, right now just change filter by hand, 
# need to nest addtional loop to automate species annd DBH class seperation 

for(i in 1:n) { # where n is stand number 
  
  #number of plots
  num_plots <- RawData %>%
    filter(RawData$STAND_ID == PlotSummary[i,1])
  num_plots <- unique(num_plots$PLOT)
  
  PlotSummary[i,2] <- length(num_plots) # fill in col

  #merch tree Count 
  plot_filter <- RawData %>% 
    filter(STAND_ID == PlotSummary[i,1]) %>%
    filter(FLAG == 'T') %>%
    filter(SPECIES != 'WJ') %>%
    filter(DBH >= 8.0 ) %>%
    group_by(PLOT, FLAG, BAF) %>%
    count(FLAG)
  
  meantc <- mean(plot_filter$n)
  PlotSummary[i,3] <- round(mean(plot_filter$n), digits = 1)
  
  #SE
  se_trees <- sd(plot_filter$n)/sqrt(PlotSummary[i,2])
  se_percent <- se_trees/meantc
  PlotSummary[i,4] <- round(se_trees/meantc, digits = 2)

  #Merch BA
  BAF <- as.numeric(min(plot_filter$BAF))
  PlotSummary[i, 30] <- min(plot_filter$BAF)
  BA_AC <- sum(plot_filter$n)*BAF /PlotSummary[i,2]
  PlotSummary[i,5] <- round(BA_AC, digits=1)
  
  #Merch TPA
  TPA_filter <- RawData %>% 
    filter(STAND_ID == PlotSummary[i,1]) %>%
    filter(FLAG == 'T') %>%
    filter(SPECIES != 'WJ') %>%
    filter(DBH >= 8 ) %>%
    select(STAND_ID, SPECIES, FLAG, DBH) 
  
  TPA_filter$BA_t <- TPA_filter$DBH^2 * 0.005454 
  
  TPA_filter$VarEF <- BAF / TPA_filter$BA_t
  
  TPA <- sum(TPA_filter$VarEF) / PlotSummary[i,2]
  
  PlotSummary[i,6] <- round(sum(TPA_filter$VarEF) / PlotSummary[i,2], digits = 1)
  
  #
  # This is where to change species and DBH class to seperate TPA and Basal area
  # 
  
  # BA - seperate by hand 
  plot_filter <- RawData %>% 
    filter(STAND_ID == PlotSummary[i,1]) %>%
    filter(FLAG == 'T') %>%
    #filter(SPECIES == 'WJ') %>%
    filter(DBH >= 8.0 ) %>%
    group_by(PLOT, FLAG, BAF) %>%
    count(FLAG)
  BAF <- as.numeric(min(plot_filter$BAF))
  #PlotSummary[i, 30] <- min(plot_filter$BAF)
  BA_AC <- sum(plot_filter$n)*BAF /PlotSummary[i,2]
  PlotSummary[i,37] <- round(BA_AC, digits=1)
  
  
  # TPA - seperate by hand
  TPA_filter <- RawData %>% 
    filter(STAND_ID == PlotSummary[i,1]) %>%
    filter(FLAG == 'T') %>%
    #filter(SPECIES == 'WJ') %>%
    filter(DBH >= 8 ) %>%
    select(STAND_ID, SPECIES, FLAG, DBH) 
  
  TPA_filter$BA_t <- TPA_filter$DBH^2 * 0.005454 
  
  TPA_filter$VarEF <- BAF / TPA_filter$BA_t
  
  TPA <- sum(TPA_filter$VarEF) / PlotSummary[i,2]
  
  PlotSummary[i,38] <- round(sum(TPA_filter$VarEF) / PlotSummary[i,2], digits = 1)
  
  
  
  # QMD
  QMD_filter <- RawData %>% 
    filter(STAND_ID == PlotSummary[i,1]) %>%
    filter(FLAG == 'T' | FLAG == 'R') %>%
    filter(DBH >= 0.01 ) %>%
    select(STAND_ID, SPECIES, FLAG, DBH) 
  
  QMD <- sqrt(mean(QMD_filter$DBH^2*0.005454)/0.005454)
  PlotSummary[i,9] <- round(sqrt(mean(QMD_filter$DBH^2*0.005454)/0.005454), digits = 1)
  
  # RegenTPA - seperate filter for DF, PP, and WJ - Could nest additional loop for cleaner code
  RegenTC_filter <- RawData %>%
    filter(STAND_ID == PlotSummary[i,1]) %>%
    filter(FLAG == 'R'| FLAG == 'T') %>%
    filter(DBH >= .01 , DBH < 8.0 ) %>%
    filter(SPECIES == "PP") %>%
    group_by(PLOT, STAND_ID, BAF, DBH, REPROD_COUNT) %>%
    count(FLAG)
  
  RegenTPA <- sum(RegenTC_filter$REPROD_COUNT * RegenTC_filter$n * RegenEF)/PlotSummary[i,2]  
  PlotSummary[i,10] <- round(sum(RegenTC_filter$REPROD_COUNT * RegenTC_filter$n * RegenEF)/PlotSummary[i,2], digits = 1)
  
  RegenTC_filter <- RawData %>%
    filter(STAND_ID == PlotSummary[i,1]) %>%
    filter(FLAG == 'R'| FLAG == 'T') %>%
    filter(DBH >= .01 , DBH < 8.0 ) %>%
    filter(SPECIES == "DF") %>%
    group_by(PLOT, STAND_ID, BAF, DBH, REPROD_COUNT) %>%
    count(FLAG)
  
  RegenTPA <- sum(RegenTC_filter$REPROD_COUNT * RegenTC_filter$n * RegenEF)/PlotSummary[i,2]  
  PlotSummary[i,11] <- round(sum(RegenTC_filter$REPROD_COUNT * RegenTC_filter$n * RegenEF)/PlotSummary[i,2], digits = 0)
  
  RegenTC_filter <- RawData %>%
    filter(STAND_ID == PlotSummary[i,1]) %>%
    filter(FLAG == 'R'| FLAG == 'T') %>%
    filter(DBH >= .01 , DBH < 8.0 ) %>%
    filter(SPECIES == "WJ") %>%
    group_by(PLOT, STAND_ID, BAF, DBH, REPROD_COUNT) %>%
    count(FLAG)
  
  RegenTPA <- sum(RegenTC_filter$REPROD_COUNT * RegenTC_filter$n * RegenEF)/PlotSummary[i,2]  
  PlotSummary[i,12] <- round(sum(RegenTC_filter$REPROD_COUNT * RegenTC_filter$n * RegenEF)/PlotSummary[i,2], digits = 1)
  
  RegenTC_filter <- RawData %>%
    filter(STAND_ID == PlotSummary[i,1]) %>%
    filter(FLAG == 'R'| FLAG == 'T') %>%
    filter(DBH >= .01 , DBH < 8.0 ) %>%
    group_by(PLOT, STAND_ID, BAF, DBH, REPROD_COUNT) %>%
    count(FLAG)
  
  RegenTPA <- sum(RegenTC_filter$REPROD_COUNT * RegenTC_filter$n * RegenEF)/PlotSummary[i,2]  
  PlotSummary[i,13] <- round(sum(RegenTC_filter$REPROD_COUNT * RegenTC_filter$n * RegenEF)/PlotSummary[i,2], digits = 1)
  
  # Veg %
  Veg_filter <- RawData %>%
    filter(STAND_ID == PlotSummary[i,1]) %>%
    filter(FLAG == 'V') %>%
    filter(SPECIES == 'G') %>%
    select(PLOT, STAND_ID, `VEG_pct`, SPECIES) 
  Grass <- sum(Veg_filter$`VEG_pct`) / PlotSummary[i,2] 
  PlotSummary[i,14] <-round(sum(Veg_filter$`VEG_pct`) / PlotSummary[i,2] *100 , digits = 1 )
  
  Veg_filter <- RawData %>%
    filter(STAND_ID == PlotSummary[i,1]) %>%
    filter(FLAG == 'V') %>%
    filter(SPECIES == 'F') %>%
    select(PLOT, STAND_ID, `VEG_pct`, SPECIES)
  Grass <- sum(Veg_filter$`VEG_pct`) / PlotSummary[i,2] 
  PlotSummary[i,15] <-round( sum(Veg_filter$`VEG_pct`) / PlotSummary[i,2] *100 , digits=1)
  
  Veg_filter <- RawData %>%
    filter(STAND_ID == PlotSummary[i,1]) %>%
    filter(FLAG == 'V') %>%
    filter(SPECIES == 'S') %>%
    select(PLOT, STAND_ID, `VEG_pct`, SPECIES) 
  Grass <- sum(Veg_filter$`VEG_pct`) / PlotSummary[i,2] 
  PlotSummary[i,16] <-round(sum(Veg_filter$`VEG_pct`) / PlotSummary[i,2] *100 , digits = 1 )
  
  # Snags  
  Snag_filter <- RawData %>%
    filter(STAND_ID == PlotSummary[i,1]) %>%
    filter(FLAG == 'S') %>%
    group_by(PLOT, STAND_ID, SNAGS_CLASS, DBH, TOTHT)%>%
    count(FLAG)
  SnagTPA <- sum(Snag_filter$n * SnagEF)/PlotSummary[i,2]  
  PlotSummary[i,17] <- round(sum(Snag_filter$n * SnagEF)/PlotSummary[i,2], digits = 1)
  PlotSummary[i,18] <- round(mean(Snag_filter$SNAGS_CLASS, na.rm = TRUE)  , digits = 1)
  PlotSummary[i,19] <- round(mean(Snag_filter$DBH, na.rm = TRUE), digits = 1)
  PlotSummary[i,20] <- round(mean(Snag_filter$TOTHT, na.rm = TRUE), digits = 1)
  
  # Crown Ratio
  Crown_filter <- RawData %>%
    filter(STAND_ID == PlotSummary[i,1]) %>%
    filter(FLAG == 'R') %>%
    select(PLOT, STAND_ID, CR) 
  RegenCR <- mean(Crown_filter$CR, na.rm = TRUE)  
  PlotSummary[i,21] <-round(mean(Crown_filter$CR, na.rm = TRUE), digits = 1)  
  
  Crown_filter1 <- RawData %>%
    filter(STAND_ID == PlotSummary[i,1]) %>%
    filter(FLAG == 'T') %>%
    select(PLOT, STAND_ID, CR)
  
  RegenCR <- mean(Crown_filter1$CR, na.rm = TRUE)  
  PlotSummary[i,22] <-round(mean(Crown_filter1$CR, na.rm = TRUE), digits = 1) 
  
  # Defect - % in thirds - bottom, middle, top
  Defect_filter <- RawData %>%
    filter(STAND_ID == PlotSummary[i,1]) %>%
    filter(FLAG == 'T') %>%
    select(PLOT, STAND_ID, DEFECT_B, DEFECT_M, DEFECT_T)
  
  Defect_filter <- RawData %>%
    filter(STAND_ID == PlotSummary[i,1]) %>%
    filter(FLAG == 'T') %>%
    select(PLOT, STAND_ID, DEFECT_B, DEFECT_M, DEFECT_T)
  
  DefectB <- mean(Crown_filter$CR, na.rm = TRUE)  
  PlotSummary[i,23] <-round(mean(Defect_filter$DEFECT_B, na.rm = TRUE)  , digits = 1)
  
  DefectM <- mean(Crown_filter$CR, na.rm = TRUE)  
  PlotSummary[i,24] <-round(mean(Defect_filter$DEFECT_M, na.rm = TRUE) , digits = 1)
  
  DefectT <- mean(Crown_filter$CR, na.rm = TRUE)  
  PlotSummary[i,25] <-round(mean(Defect_filter$DEFECT_T, na.rm = TRUE) , digits = 1)
  
  # Age 
  Age_filter <- SiteIndex %>% 
    filter(Stand == PlotSummary[i,1])
  TotalAge <- mean(Age_filter$Age) + 7  
  PlotSummary[i,26] <-max(Age_filter$Age) + BH_age
  
  #Site Index Functions 
  CochranDF_SI <- function(TOTHT, AGE){
    
    SI <- 84.47 - exp(-0.37496+1.36164*log(AGE)-0.00243434*log(AGE)^4)*
      (0.52032 - (0.0013194 * AGE) + 27.2823/AGE)+
      (TOTHT - 4.5) * (0.52032 - (0.0013194 * AGE) + 27.2823/AGE)
    return(SI)
  }
  
  #there is a problem with ole Barrett here...fixed by hand for now...
  BarrettPP_SI <- function(TOTHT, AGE){
    
    SI <- 100.43 +4.5  - (1.198632 - (0.00283073 * AGE)+(8.44441/AGE))*
      (128.8952205*(1 - exp(-0.016959*AGE)^1.23114))+
      (1.198632 - (0.00283073 * AGE)+(8.44441/AGE) * (TOTHT - 4.5))
    return(SI)
  }
  
  SiteIndex$SI <- ifelse(SiteIndex$Species == "DF", CochranDF_SI(SiteIndex$Ht, SiteIndex$Age), 
                         BarrettPP_SI(SiteIndex$Ht, SiteIndex$Age))
  
  BarrettPP_SI(60,53) # NOT correct!
  CochranDF_SI(60,53) # correct
  
  #fill in site index cols
  SI_filterDF <- SiteIndex %>%
    filter(Stand == PlotSummary[i,1]) %>%
    filter(Species == 'DF')
  SI <- CochranDF_SI(SI_filterDF$Ht, SI_filterDF$Age)
  PlotSummary[i,28] <- round(mean(CochranDF_SI(SI_filterDF$Ht, SI_filterDF$Age)), digits = 1)
  
  SI_filterPP <- SiteIndex %>%
    filter(Stand == PlotSummary[i,1]) %>%
    filter(Species == 'PP')
  SI <- BarrettPP_SI(SI_filterPP$Ht, SI_filterPP$Age)
  PlotSummary[i,27] <- round(mean(BarrettPP_SI(SI_filterPP$Ht, SI_filterPP$Age)), digits =1 )
  
  # radial growth
  SI_filterRadGr <- SiteIndex %>%
    filter(Stand == PlotSummary[i,1])
  RadGr <- mean(SiteIndex$`10yr`, rm.na = TRUE)
  PlotSummary[i,29] <- round(mean(SI_filterRadGr$`10yr`, na.rm = TRUE), digits = 1)
  
  ####################################
  # Volume Calculation - Still in loop - take this out of this script, 
  # is done better in seperate mechandising script, then combined later
  ####################################
  
  #PNW Equation 4 -  Ponderosa Pine  
  Vol_PP <- function(DBH, TOTHT) { 
    BA <- DBH^2 * 0.005454
    CVTSL <- -8.521558 + 1.977243 * log(DBH) - 0.105288 * (log(TOTHT))^2 + (136.0489/TOTHT^2) + 1.99546 * log(TOTHT)
    CVTS <- exp(CVTSL)
    TARIF <- (CVTS*0.912733)/((1.033*(1.0+1.382937*exp(-4.015292*DBH)))*(BA+0.087266)-0.174533)
    CV4 <- TARIF * (BA - 0.087266)/0.912733
    CVT <- TARIF*(0.9679-0.1051*(0.5523^(DBH-1.5))*((1.033*(1.0+1.382937*exp(-4.015292*(DBH/10))))*(BA+0.087266)-0.174533))/0.912733
    
    RC6 <- 0.993-(0.993*0.62^(DBH-6.0))
    CV6 <- RC6 * CV4
    #If CV6>CV4 THen CV6 = CV4
    CUBUS <- CV4 - CV6
    B4 <- TARIF/0.912733
    RS616L <- 0.174439 + 0.117594 * log(DBH) * log(B4) - (8.210585/DBH^2)+ 0.236693* log(B4)-0.00001345*(B4^2)-0.00001937 * DBH^2  
    RS616 <- 10^(RS616L)
    RS632 <- 1.001491 - (6.924097/TARIF)+0.00001351 * DBH^2
    SV616 <- RS616 * CV6
    SV632 <- RS632 *SV616
    SCRIB <- SV616
    
    return(round(CVTS))
  }
  
  #Try Taper Equation 
  
  smalians <- function(d1, d2, len){
    vol <- (pi/(4*2*12*12))*(d1^2 + d2^2)*len
    return(round(vol, 4))}
  
  K02PP.fx <- function(HT, DBH, TOTHT){
    Z <- HT/TOTHT
    P <- 4.5/TOTHT #Check on this - 1.3 vs 4.5 feet?
    Q <- 1 - (HT/TOTHT)^(1/3)
    X <- Q / (1 - (P)^(1/3))
    
    a0 = 0.894080
    a1 = 1.050869
    a2 = -0.039679
    b1 = 0.468454
    b2 = -0.872955
    b3 = 1.211334
    b4 = 0.951671 
    b5 = 0.019065
    b6 = -0.381079
    
    dib <- (a0*(DBH^a1)*(TOTHT^a2)*X^(b1*(Z^4)+b2*(1/exp(DBH/TOTHT))+b3*(X^0.1)+b4*(1/DBH)+b5*(TOTHT^Q)+b6*X))
    return(dib)
  }
  
  # Function to calculate CVTS by dividing tree bole to 100 sections
  K02_vol <-function(DBH, TOTHT){
    sgmts <- 100
    L  <- TOTHT / sgmts
    i <- 0
    vol <- 0
    while(i<(sgmts-1)){
      H1  <- (L * i)
      H2 <- (L * (i+1))
      dib1 <- K02PP.fx(HT=H1, DBH=DBH, TOTHT=TOTHT)
      dib2 <- K02PP.fx(HT=H2, DBH=DBH, TOTHT=TOTHT)
      vol <-vol+smalians(dib1, dib2, L)
      i <- i+1
    }
    return(vol)
  }
  K02_vol(14,100)
  
  #PNW Equation 2 - Douglas-fir
  Vol_DF <- function(DBH, TOTHT){
    BA <- DBH^2 * 0.005454
    CVTSL <- -6.110493 + 1.81306 * log(DBH) + 1.083884 * log(TOTHT) 
    CVTS <- exp(CVTSL)
    TARIF <- (CVTS*0.912733)/((1.033*(1.0+1.382937*exp(-4.015292*(DBH/10))))*(BA+0.087266)-0.174533)
    CV4 <- TARIF * (BA - 0.087266)/0.912733
    CVT <- TARIF*(0.9679-0.1051*(0.5523^(DBH-1.5))*((1.033*(1.0+1.382937*exp(-4.015292*(DBH/10))))*(BA+0.087266)-0.174533))/0.912733
    
    RC6 <- 0.993-(0.993*0.62^(DBH-6.0))
    CV6 <- RC6 * CV4
    #If CV6>CV4 THen CV6 = CV4
    CUBUS <- CV4 -CV6
    B4 <- TARIF/0.912733
    RS616L <- 0.174439 + 0.117594 * log(DBH) * log(B4) - (8.210585/DBH^2)+ 0.236693* log(B4)-0.00001345*(B4^2)-0.00001937 * DBH^2  
    RS616 <- 10^(RS616L)
    RS632 <- 1.001491 - (6.924097/TARIF)+0.00001351 * DBH^2
    SV616 <- RS616 * CV6
    SV632 <- RS632 *SV616
    SCRIB <- SV616
    
    return(round(CVTS))
    
  }
  
  Vol_PP(14, 100)
  Vol_DF(14, 100)
  K02_vol(14,100)
  
  # Volume per acre  
  
  #Fit height to diameter equation for each species and stand -> HT = 4.5+exp(a_0+a_1*DBH^a_2)  
  MerchVol_filterDF <- RawData %>%
    filter(FLAG =="T")%>%
    filter(SPECIES == "DF") %>%
    select(BAF, SPECIES, DBH, TOTHT)
  
  MerchVol_filterPP <- na.omit(MerchVol_filterDF)
  
  HtDia_DF <- nls(TOTHT ~ 4.5 + exp(a0+a1*(DBH^a2)), 
                  data = MerchVol_filterDF, 
                  start = list(
                    a0 = 7.2621,
                    a1 = -5.88,
                    a2 = -0.287207))  
  DF_coeff <- as.data.frame(coefficients(HtDia_DF))
  summary(HtDia_DF)
  plot(HtDia_DF)
  
  MerchVol_filterPP <- RawData %>%
    filter(FLAG =="T")%>%
    filter(SPECIES == "PP") %>%
    select(BAF, SPECIES, DBH, TOTHT)
  
  MerchVol_filterPP <- na.omit(MerchVol_filterPP) 
  
  HtDia_PP <- nls(TOTHT ~ 4.5 + exp(a0+a1*(DBH^a2)), 
                  data = MerchVol_filterPP, 
                  start = list(
                    a0 = 7.2621,
                    a1 = -5.88,
                    a2 = -0.287207))  
  PP_coeff <- as.data.frame(coefficients(HtDia_PP))
  summary(HtDia_PP)
  plot(HtDia_PP)
  
  # Predict Heights based on fitted model 
  PredictHT <- RawData %>%
    filter(FLAG =="T")%>%
    filter(SPECIES != "WJ") %>%
    select(BAF, SPECIES, DBH, TOTHT)
  
  DF_HT <- function(DBH){
    
    a0 <- DF_coeff[1,1]
    a1 <- DF_coeff[2,1]
    a2 <- DF_coeff[3,1]
    
    DF_predHT <- 4.5 + exp(a0+a1*(DBH^a2))
    return(DF_predHT)
  }
  
  PP_HT <- function(DBH){
    
    a0 <- PP_coeff[1,1]
    a1 <- PP_coeff[2,1]
    a2 <- PP_coeff[3,1]
    
    PP_predHT <- 4.5 + exp(a0+a1*(DBH^a2))
    return(PP_predHT)
  }
  
  # predict heights on non-measure trees and calulate volumes with direct equation 
  MerchVol_filterDF <- RawData %>%
    filter(FLAG =="T")%>%
    filter(SPECIES == "DF") %>%
    filter(DBH>8.0)%>%
    select(STAND_ID, PLOT, BAF, SPECIES, DBH, TOTHT)
  
  # Keep field measured height, predict otherwise - calulate volume
  MerchVol_filterDF$TOTHT <- ifelse(is.na(MerchVol_filterDF$TOTHT), DF_HT(MerchVol_filterDF$DBH), MerchVol_filterDF$TOTHT) #predict DF height
  MerchVol_filterDF$VOLt <- Vol_DF(MerchVol_filterDF$DBH, MerchVol_filterDF$TOTHT) #estimate DF volume
  
  MerchVol_filterPP <- RawData %>%
    filter(FLAG =="T")%>%
    filter(SPECIES == "PP") %>%
    filter(DBH>8.0)%>%
    select(STAND_ID, PLOT, BAF, SPECIES, DBH, TOTHT)
  
  MerchVol_filterPP$TOTHT <- ifelse(is.na(MerchVol_filterPP$TOTHT), DF_HT(MerchVol_filterPP$DBH), MerchVol_filterPP$TOTHT) #predict PP height                  
  MerchVol_filterPP$VOLt <- Vol_PP(MerchVol_filterPP$DBH, MerchVol_filterPP$TOTHT)#estimate PP volume 
  
  Merch_Vol <- rbind(MerchVol_filterPP, MerchVol_filterDF) #combine DF and PP
  
  # Expand volume per tree to per acres basis 
  Merch_Vol$EF <- Merch_Vol$BAF/(Merch_Vol$DBH^2*0.005454) #Expansion factor
  Merch_Vol$VolAV_cont <- Merch_Vol$EF * Merch_Vol$VOLt   #volume contribution per acre 
  
  Merch_Vol_dummy <- Merch_Vol %>%
    filter(STAND_ID == PlotSummary[i,1]) 
  STvol <- sum(Merch_Vol_dummy$VolAV_cont) /PlotSummary[i,2]
  PlotSummary[i,7] <- sum(Merch_Vol_dummy$VolAV_cont) / PlotSummary[i,2]

} #END OF BIG LOOP

# test height - diameter eqs - seem ok 
PP_HT(12)
DF_HT(12)

# Test volume per acre - not right - this script should just be inventory - 
# do volume and merchandising in seperate script...

PropertyTest <- as.data.frame(matrix(nrow = n ,ncol = 6))
colnames(PropertyTest)[1] <- "STD"
colnames(PropertyTest)[2] <- "Gross_ac"
colnames(PropertyTest)[3] <- "Vol_ac"
colnames(PropertyTest)[4] <- "StandVol"
colnames(PropertyTest)[6] <- "StandBF_ac"
colnames(PropertyTest)[5] <- "StandBF"

PropertyTest$STD <- Overview$STAND
PropertyTest$Net_ac <- Overview$Acres.Net
PropertyTest$Vol_ac <- PlotSummary$MerchVol_ac
PropertyTest$StandVol <- PropertyTest$Net_ac * PropertyTest$Vol_ac
PropertyTest$StandBF_ac <- PropertyTest$Vol_ac * 6
PropertyTest$StandBF <- PropertyTest$StandBF_ac * PropertyTest$Gross_ac

print(PropertyTotal <- (sum(PropertyTest$StandVol)) * 4) / 1000 

# Write attribute table to .csv

#write_csv(PlotSummary, "Inventory_Attribute_Table.csv")

end <- Sys.time()
print(end-start)
