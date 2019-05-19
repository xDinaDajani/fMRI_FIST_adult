
#==========Analyzing behavioral data from fMRI FIST adult task============
rm(list= ls())
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
getwd()
dev.off()
library(dplyr)

# LOAD DATA
acc.dat <- read.csv("Acc_data_fMRI_task_final3.8.18.csv", header = T, na = "") 

## =========ACC/RT Combo DV Calculation===================================
#based on calculation provided in Zelazo et al. 2013 (NIH Toolbox for DCCS)

##### To get accuracy on 0 to 5 scale, divide mean acc by 2. do this per run.

  # Generate subset with only accuracy variables
  NameList <- c("fMRIRun1MeanFlexTrialAcc", "fMRIRun2MeanFlexTrialAcc", "fMRIRun3MeanFlexTrialAcc", "fMRIRun4MeanFlexTrialAcc")
  col.num <- which(colnames(acc.dat) %in% NameList)
  fmriAcc.vector <- acc.dat[c(1, col.num)]
  # Generate subset with only RT variables
  NameList <- c("Med_RT_Run1", "Med_RT_Run2", "Med_RT_Run3", "Med_RT_Run4")
  col.num <- which(colnames(acc.dat) %in% NameList)
  fmriRT.vector <- acc.dat[c(1, col.num)]
  # Divide acc values by 2
  fmriAcc_rescaled <- (fmriAcc.vector[2:5]/2)*10
  #histogram of acc values
  hist(fmriAcc.vector$fMRIRun1MeanFlexTrialAcc, main = "fMRI Run 1 Flex Trials Histogram")
  hist(fmriAcc.vector$fMRIRun2MeanFlexTrialAcc, main = "fMRI Run 2 Flex Trials Histogram")
  hist(fmriAcc.vector$fMRIRun3MeanFlexTrialAcc, main = "fMRI Run 3 Flex Trials Histogram")
  hist(fmriAcc.vector$fMRIRun4MeanFlexTrialAcc, main = "fMRI Run 4 Flex Trials Histogram")

  #histogram of RT values
  hist(fmriRT.vector$Med_RT_Run1, main = "fMRI Run 1 Flex Trials Histogram", xlim=c(0, 8000))
  hist(fmriRT.vector$Med_RT_Run2, main = "fMRI Run 2 Flex Trials Histogram", xlim=c(0, 8000))
  hist(fmriRT.vector$Med_RT_Run3, main = "fMRI Run 3 Flex Trials Histogram", xlim=c(0, 8000))
  hist(fmriRT.vector$Med_RT_Run4, main = "fMRI Run 4 Flex Trials Histogram", xlim=c(0, 8000))
  
#### Collect RT values and put them on 0 to 5 scale
  
  # Determine upper and lower bound for RT based on sample data; range
  library(psych)
  describe(fmriRT.vector[2:5])
  #find min of RT across all 4 runs and subtract that value from everyone's RT so that RT is scaled to the maximum range
  min_RT <- min(apply(na.omit(fmriRT.vector[,2:5]), 2, min))
  max_RT <- max(apply(na.omit(fmriRT.vector[,2:5]), 2, max))
  range_RT <- max_RT - min_RT
  fmriRT.vector[,6:9] <- fmriRT.vector[,2:5] - min_RT
  describe(fmriRT.vector[6:9])
  #Minimum RT across runs is 3275, maximum is 7427.5ms; range is 4152.5 
  # Transform to 0 to 5 scale and reverse score so that higher values indicate smaller RT
  # the denominator should be the slowest possible RT to achieve
  fmriRT_rescaled <- 5 - (fmriRT.vector[6:9]*(5))/range_RT
  # Any NA RT values are revalued to 0 so they can be added to the rescaled acc scores
  fmriRT_rescaled[is.na(fmriRT_rescaled)] <- 0
  colnames(fmriRT_rescaled) <-c("RT_rescaled_Run1",
                                "RT_rescaled_Run2",
                                "RT_rescaled_Run3",
                                "RT_rescaled_Run4")
  describe(fmriRT_rescaled)
  b <- cbind(fmriRT.vector, fmriRT_rescaled)
  
  #histogram of rescaled RT
  hist(fmriRT_rescaled$RT_rescaled_Run1, main = "fMRI Run 1 Flex Trials Histogram")
  hist(fmriRT_rescaled$RT_rescaled_Run2, main = "fMRI Run 1 Flex Trials Histogram")
  hist(fmriRT_rescaled$RT_rescaled_Run3, main = "fMRI Run 1 Flex Trials Histogram")
  hist(fmriRT_rescaled$RT_rescaled_Run4, main = "fMRI Run 1 Flex Trials Histogram")
  
  # correlation between original RT and rescaled RT value
  #they are obviously perfectly correlated
  cor.test(b$Med_RT_Run1, b$RT_rescaled_Run1)
  library("ggplot2")
  ggplot(b, aes(x=Med_RT_Run1, y=RT_rescaled_Run1)) +geom_point() +ylim(0,5)
  ggplot(b, aes(x=Med_RT_Run2, y=RT_rescaled_Run2)) +geom_point()+ylim(0,5)
  ggplot(b, aes(x=Med_RT_Run3, y=RT_rescaled_Run3)) +geom_point()+ylim(0,5)
  ggplot(b, aes(x=Med_RT_Run4, y=RT_rescaled_Run4)) +geom_point()+ylim(0,5)
  
  
  
  
### ADD rescaled RT to rescaled accuracy ONLY FOR PARTICIPANTS WITH AT LEAST 80% ACC (equivalent to a rescaled acc of 4).
  
  rescaled_both <- cbind(fmriAcc_rescaled, fmriRT_rescaled)
  colnames(rescaled_both) <- c("fMRIRun1Acc_rescaled", 
                               "fMRIRun2Acc_rescaled", 
                               "fMRIRun3Acc_rescaled",
                               "fMRIRun4Acc_rescaled",
                               "Med_RT_Run1_rescaled",
                               "Med_RT_Run2_rescaled",
                               "Med_RT_Run3_rescaled",
                               "Med_RT_Run4_rescaled")
  rescaled_both_newDV <- rescaled_both %>%
    mutate(accRT_DV_Run1 = ifelse(fMRIRun1Acc_rescaled>=4, fMRIRun1Acc_rescaled+Med_RT_Run1_rescaled,fMRIRun1Acc_rescaled),
           accRT_DV_Run2 = ifelse(fMRIRun2Acc_rescaled>=4, fMRIRun2Acc_rescaled+Med_RT_Run2_rescaled,fMRIRun2Acc_rescaled),
           accRT_DV_Run3 = ifelse(fMRIRun3Acc_rescaled>=4, fMRIRun3Acc_rescaled+Med_RT_Run3_rescaled,fMRIRun3Acc_rescaled),
           accRT_DV_Run4 = ifelse(fMRIRun4Acc_rescaled>=4, fMRIRun4Acc_rescaled+Med_RT_Run4_rescaled,fMRIRun4Acc_rescaled))

  #Take a look at variability of this accRT DV
  describe(rescaled_both_newDV[,9:12])
  accv2.dat <- cbind(acc.dat, rescaled_both_newDV[,9:12])  
  #histogram of new DV; negatively skewed so that more people are closer to 7/8 than 2/3
  hist(accv2.dat$accRT_DV_Run1, main = "fMRI Run 1 Flex Trials Histogram")
  hist(accv2.dat$accRT_DV_Run2, main = "fMRI Run 2 Flex Trials Histogram")
  hist(accv2.dat$accRT_DV_Run3, main = "fMRI Run 3 Flex Trials Histogram")
  hist(accv2.dat$accRT_DV_Run4, main = "fMRI Run 4 Flex Trials Histogram")
  
  # Investigate what it means to be in the middle of this new accRT scale.
  accv2.dat %>%
    select(fMRIRun1MeanFlexTrialAcc, Med_RT_Run1, accRT_DV_Run1) %>%
    mutate(fMRIRun1MeanFlexTrialAcc = scale(fMRIRun1MeanFlexTrialAcc),
           Med_RT_Run1 =  scale(Med_RT_Run1))
  
  ## COMPARE VARIABILITY OF NEW DV TO ACC AND RT ALONE
  
  NameList <- c("fMRIRun1MeanFlexTrialAcc", 
                "fMRIRun2MeanFlexTrialAcc", 
                "fMRIRun3MeanFlexTrialAcc", 
                "fMRIRun4MeanFlexTrialAcc", 
                "Med_RT_Run1", 
                "Med_RT_Run2", 
                "Med_RT_Run3", 
                "Med_RT_Run4",
                "accRT_DV_Run1",
                "accRT_DV_Run2",
                "accRT_DV_Run3",
                "accRT_DV_Run4")
  col.num <- which(colnames(accv2.dat) %in% NameList)
  describe(accv2.dat[c(col.num[c(1,5,9)])])
  
##======Write out new DV to CSV file====================================

  #write.csv(accv2.dat, "Acc_data_fMRI_task_newDV_final3.8.18.csv", row.names=FALSE)
  write.csv(accv2.dat, "Acc_data_fMRI_task_newDV_5.18.19_updated.csv", row.names=FALSE)
  
