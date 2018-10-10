
# compiles inventory plot data to stand level estimates 
# PNW Forestry 
# Al Pancoast 
# June 2017

# TPA and Basal area by species and DBH class are done by changing code maually for now - automate later if needed
# PP Site index calulation is not working...
# empty cols still need work


start <- Sys.time()

library(tidyverse)

RawData <- read.csv("Plot_Data.csv")
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

stands_list <- RawData %>% 
  group_by(STAND_ID)%>%
  count(unique(STAND_ID)) %>% 
  select(STAND_ID)

#Set baseline cruise detials 

n <- as.numeric(nrow(stands_list)) # nummber of stands 
RegenEF <- 100 # expansion factor for 1/100th acre fixed plot - regen tally
SnagEF <- 5 # expansion factor for 1/5th acre fixed plot - snags
BH_age <- 7 # assumed 7 years to reach breast height - Ponderosa Pine, Ochocco region


#create empty summary table with space for all desired variables 

PlotSummary <- as.data.frame(matrix(nrow = n ,ncol = 38))
colnames(PlotSummary)[1] <- "STAND_ID"
PlotSummary$STAND_ID <- stands_list$STAND_ID
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
  
} #END OF LOOP

# Write attribute table to .csv

#write_csv(PlotSummary, "Inventory_Attribute_Table.csv")

end <- Sys.time()
print(end-start)
