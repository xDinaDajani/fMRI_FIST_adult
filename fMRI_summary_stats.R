
###Analyzing behavioral data from fMRI FIST adult task#######
#############################################################

rm(list=ls())
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
getwd()
acc.dat <- read.csv("Acc_data_fMRI_task_newDV_final4.23.18.csv", header = T) 
dev.off()

#Packages
library(psych)

#================Get summary stats across ALL runs=======================================

sumstats <- with(acc.dat, psych::describe(acc.dat[,2:ncol(acc.dat)]))

  # Mean of mean accuracy across all runs
    fmriAcc.vector<-stack(acc.dat, select=c("fMRIRun1MeanFlexTrialAcc", 
                                            "fMRIRun2MeanFlexTrialAcc", 
                                            "fMRIRun3MeanFlexTrialAcc", 
                                            "fMRIRun4MeanFlexTrialAcc"))
    range_flex_acc <- range(fmriAcc.vector[,1])
    mean_flex_acc <- mean(fmriAcc.vector[,1])
    sd_flex_acc <- sd(fmriAcc.vector[,1])
    fmriAcc_Cnt.vector<-stack(acc.dat, select=c("fMRIRun1CntAcc", 
                                                "fMRIRun2CntAcc", 
                                                "fMRIRun3CntAcc", 
                                                "fMRIRun4CntAcc"))
    range_control_acc <- range(fmriAcc_Cnt.vector[,1])
    mean_control_acc <-mean(fmriAcc_Cnt.vector[,1])
    sd_control_acc <-sd(fmriAcc_Cnt.vector[,1])
  
  # Median of median RT across all runs
    fmriRT.vector<-stack(acc.dat, select=c("Med_RT_Run1", "Med_RT_Run2", "Med_RT_Run3", "Med_RT_Run4"))
    range_flex_RT <- range(fmriRT.vector[,1], na.rm=TRUE)
    med_flex_RT <- median(fmriRT.vector[,1], na.rm=TRUE)
    sd_flex_RT <- sd(fmriRT.vector[,1], na.rm=TRUE)
  
  allruns <- data.frame(mean_flex_acc, sd_flex_acc, mean_control_acc, sd_control_acc, med_flex_RT, sd_flex_RT)
  allruns
  
#========Table of run-level stats========================================================
  
  #install.packages("kableExtra")
  library(kableExtra)
  library(knitr)
  options(knitr.table.format = "html") 
  
  # First, re-order rows in sumstats dataframe
  sumstats_ordered <- rbind(sumstats[seq(1,16,4),],
                            sumstats[seq(2,16,4),],
                            sumstats[seq(3,16,4),],
                            sumstats[seq(4,16,4),])
  
  # Mean and SD information that I want in my table
  my_matrix <- as.matrix(cbind(sumstats_ordered[,8:9], sumstats_ordered[,3:4]))
  rownames(my_matrix)[seq(1,16,4)] <- "Flexibility Accuracy"
  rownames(my_matrix)[seq(2,16,4)] <- "Control Accuracy"
  rownames(my_matrix)[seq(3,16,4)] <- "Flexibility Median RT (ms)"
  rownames(my_matrix)[seq(4,16,4)] <- "Flexibility Accuracy-RT"
  
  # Insert median for RT info
  my_matrix[seq(3,16,4),3] <- medians[seq(9,12)]  
  
  # Capitalize column names
  colnames(my_matrix) <-c("Min", "Max", "Mean", "SD")
  
  # Round all data to two decimal places except RT (integer)
  my_matrix <- round(my_matrix, 2)
  my_matrix[seq(3,16,4),] <- round(my_matrix[seq(3,16,4),], 0)
  # Make table!   
  kable(my_matrix, "html", caption = "", align=c(rep('c', 5))) %>%
    kable_styling("hover", 
                  full_width = F,
                  position = "center") %>%
    group_rows("Run 1", 1, 4) %>%
    group_rows("Run 2", 5, 8) %>%
    group_rows("Run 3", 9, 12) %>%
    group_rows("Run 4", 13, 16)
  
#=====Compare accuracy between control and Flex trials===================================
  
# using two-way repeated measures ANOVA to test (two within-subjects factors, Run and Trial type)
  
  library(lmerTest)
  library(tidyr)
  library(dplyr)
  #install.packages("tidyverse")  
 
  # subset data to just include accuracy for runs1-4 control & flex
  acc.dat_subset <- acc.dat[,1:9]
  #rename Flex trials so that they have the same structure as control trials
  colnames(acc.dat_subset)[2:5] <- c("fMRIRun1FlxAcc",
                                     "fMRIRun2FlxAcc",
                                     "fMRIRun3FlxAcc",
                                     "fMRIRun4FlxAcc")
  # reshape data 
  datalong <- acc.dat_subset %>% 
    gather(trialtype, avg.acc, 2:9) %>% 
    extract(trialtype, c("Run", "Event"), regex = "(Run.)(...Acc)")

  datalong$Run <-factor(datalong$Run)
  datalong$Event <-factor(datalong$Event)
  
  # contrasts
  contrasts(datalong$Run) <- contr.poly(4)
  modelRMAOV <-aov(avg.acc~Run*Event+Error(PID/(Run*Event)), data=datalong)
  summary(modelRMAOV, split=list(Run=list("Linear"=1, "Quadratic"=2, "Cubic"=3)))
  
  # summarizing mean accuarcy grouped by run and event type
  groups <- datalong %>% group_by(Run, Event)
  summarise(groups, mean(avg.acc))
  
  #post-contrasts to describe cubic trend
  require(nlme)         ## for lme()
  #install.packages("multcomp")
  require(multcomp)  ## for multiple comparison stuff
  Lme.mod <- lme(avg.acc ~ Run, random = ~1 | PID/Run, data=datalong)
  anova(Lme.mod)
  summary(Lme.mod)
  summary(glht(Lme.mod, linfct=mcp(Run="Tukey")))
  
#=====Changes RT for Flex trials across runs =================================================
  acc.dat.RT <-acc.dat[,c(1,10:13)]
  #remove subjects with any missing values because repeated measures ANOVA cannot handle missing values
  acc.dat.RT <-na.omit(acc.dat.RT)
  datalong <- reshape(data=acc.dat.RT, 
                      varying =2:5, 
                      v.names = "run_level_RT", 
                      timevar = "Run",
                      idvar = "PID",
                      direction = "long")
  datalong$Run <-factor(datalong$Run)
  contrasts(datalong$Run) <- contr.poly(4)
  modelRMAOV <-aov(run_level_RT~Run+Error(PID/Run), data=datalong)
  summary(modelRMAOV, split=list(Run=list("Linear"=1, "Quadratic"=2, "Cubic"=3)))
  Lme.mod <- lme(run_level_RT ~ Run, random = ~1 | PID/Run, data=datalong)
  anova(Lme.mod)
  summary(Lme.mod)
  summary(glht(Lme.mod, linfct=mcp(Run="Tukey")))
# =====Changes in accuracy-RT across runs================================================
  
  # Does accuracy-RT change across runs?
  #using repeated measures ANOVA to test
  
  # reshape data 
  datalong <- reshape(data=acc.dat, 
                      varying =2:5, 
                      v.names = "run_level_accuracy", 
                      timevar = "Run",
                      idvar = "PID",
                      direction = "long")
  datalong <- reshape(data=acc.dat, 
                      varying =14:17, 
                      v.names = "run_level_acc_RT", 
                      timevar = "Run",
                      idvar = "PID",
                      direction = "long")
  datalong$Run <-factor(datalong$Run)
  
  #contrasts
  contrasts(datalong$Run) <- contr.poly(4)
  # using accuracy as outcome variable
  #modelRMAOV <-aov(run_level_accuracy~Run+Error(PID/Run), data=datalong)  
  #using acc-RT metric as ourcome variable
  modelRMAOV <-aov(run_level_acc_RT~Run+Error(PID/Run), data=datalong) 
  summary(modelRMAOV, split=list(Run=list("Linear"=1, "Quadratic"=2, "Cubic"=3)))
  
  #post-contrasts to describe cubic trend
  require(nlme)         ## for lme()
  #install.packages("multcomp")
  require(multcomp)  ## for multiple comparison stuff
  Lme.mod <- lme(run_level_acc_RT ~ Run, random = ~1 | PID/Run, data=datalong)
  anova(Lme.mod)
  summary(Lme.mod)
  summary(glht(Lme.mod, linfct=mcp(Run="Tukey")))  

#=====Check on behavioral outliers based on both flex & control trial acc================

#outliers are defined as values lying outside the 1.5*interquartile range (outside of 25th - 75th percentiles) on BOTH flexibility and control trials 

# the below loop only considers Flexibility trials (Accuracy)
  for (run in 1:4) 
    {
    outlier_values_fMRIacc <- boxplot.stats(acc.dat[,run+1])$out
    boxplot(acc.dat[,run+1], main = "Mean fMRI Task Accuracy Flex", boxwex = .1)
    mtext(paste("Outliers: ", paste(outlier_values_fMRIacc, collapse=",   ")), cex=1)
    if (length(outlier_values_fMRIacc) > 1) {
      for (outlier in 1:length(outlier_values_fMRIacc)) {
        PID[outlier] <- as.character(acc.dat[which(acc.dat[,run+1] == outlier_values_fMRIacc[outlier], arr.ind=TRUE), 1])
        if (outlier == 1) {
          cat("Run:", run, ", Outliers identified:",  length(outlier_values_fMRIacc), "\n")
        }
        cat("PID:", PID[outlier])
      }
    }
    else {
      PID <- as.character(acc.dat[which(acc.dat[,run+1] == outlier_values_fMRIacc, arr.ind=TRUE), 1])
      cat("Run:", run, ", Outliers identified: ",  length(outlier_values_fMRIacc), ", PID:", PID, "\n")
    }
  }

# the below loop only considers control trials (Accuracy)
for (run in 1:4) 
{
  outlier_values_fMRIacc <- boxplot.stats(acc.dat[,run+5])$out
  boxplot(acc.dat[,run+5], main = "Mean fMRI Task Accuracy Control", boxwex = .1)
  mtext(paste("Outliers: ", paste(outlier_values_fMRIacc, collapse=",   ")), cex=1)
  if (length(outlier_values_fMRIacc) > 1) {
    for (outlier in 1:length(outlier_values_fMRIacc)) {
      PID[outlier] <- as.character(acc.dat[which(acc.dat[,run+5] == outlier_values_fMRIacc[outlier], arr.ind=TRUE), 1])
      if (outlier == 1) {
        cat("Run:", run, ", Outliers identified:",  length(outlier_values_fMRIacc), "\n")
      }
      cat("PID:", PID[outlier])
    }
  }
  else {
    PID <- as.character(acc.dat[which(acc.dat[,run+5] == outlier_values_fMRIacc, arr.ind=TRUE), 1])
    cat("Run:", run, ", Outliers identified: ",  length(outlier_values_fMRIacc), ", PID:", PID, "\n")
  }
}
  
# the below loop only considers flex trials for ACC-RT metric
for (run in 1:4) 
{
  outlier_values_fMRIacc <- boxplot.stats(acc.dat[,run+13])$out
  boxplot(acc.dat[,run+13], main = "Mean fMRI Task Accuracy ACC-RT metric", boxwex = .1)
  mtext(paste("Outliers: ", paste(outlier_values_fMRIacc, collapse=",   ")), cex=1)
  if (length(outlier_values_fMRIacc) > 1) {
    for (outlier in 1:length(outlier_values_fMRIacc)) {
      PID[outlier] <- as.character(acc.dat[which(acc.dat[,run+13] == outlier_values_fMRIacc[outlier], arr.ind=TRUE), 1])
      if (outlier == 1) {
        cat("Run:", run, ", Outliers identified:",  length(outlier_values_fMRIacc), "\n")
      }
      cat("PID:", PID[outlier])
    }
  }
  else {
    PID <- as.character(acc.dat[which(acc.dat[,run+13] == outlier_values_fMRIacc, arr.ind=TRUE), 1])
    cat("Run:", run, ", Outliers identified: ",  length(outlier_values_fMRIacc), ", PID:", PID, "\n")
  }
}

#update 4/9/2018: Outliers defined as less than or equal to 50% on BOTH flexibility and control trials for a single run

# create accuracy columns with NA for outliers
acc.dat["fMRIRun1Flex_no_outliers"] <- acc.dat[,2]
acc.dat["fMRIRun2Flex_no_outliers"] <- acc.dat[,3]
acc.dat["fMRIRun3Flex_no_outliers"] <- acc.dat[,4]
acc.dat["fMRIRun4Flex_no_outliers"] <- acc.dat[,5]
acc.dat["fMRIRun1Cnt_no_outliers"] <- acc.dat[,6]
acc.dat["fMRIRun2Cnt_no_outliers"] <- acc.dat[,7]
acc.dat["fMRIRun3Cnt_no_outliers"] <- acc.dat[,8]
acc.dat["fMRIRun4Cnt_no_outliers"] <- acc.dat[,9]

# for loop to assign NA to Flex and Control variables with 50% or less acc
# on BOTH Flex and control for a particular run
for (run in 1:4) 
{   for (row in 1:32) {
        if (acc.dat[row,run+1] <.60 & acc.dat[row,run+5] <.60)
        {
          cat("Run:", run, ", Outlier identified: ", as.character(acc.dat[row,1]), "\n")
          acc.dat[row,run+17] <- NA
          acc.dat[row,run+21] <- NA
        }
}
}

# NOT USED #=============================================================================
# per run, assign NA if outlier on BOTH flex and control trial

remove_outliers <- function(x, na.rm = TRUE, ...) 
{
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
acc.dat$meanfMRIacc_no_outliers <-remove_outliers(acc.dat$meanfMRIacc)

outlier_values_ComputerAcc <- boxplot.stats(acc.dat$ComputerMeanFlexTrialAcc)$out
boxplot(acc.dat$ComputerMeanFlexTrialAcc, main = "Mean Computer-based Task Accuracy", boxwex = .1)
mtext(paste("Outliers: ", paste(outlier_values_ComputerAcc, collapse=",   ")), cex=1)




