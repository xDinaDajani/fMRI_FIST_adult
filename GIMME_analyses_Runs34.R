# GIMME Analyses Flexible Item Selection Task (FIST)

rm(list=ls())
#path <- "/Users/ddajani/Box Sync/Dajani_eventrelatedGIMME"
path <- "/projects/scratch/abide/Dina/FIST/Dajani_eventrelatedGIMME"
setwd(path)
getwd()

# Load in ROI timecourses AND canonical HRF event regressors # ===============================================

# grab subject IDs
# path to event regressors
events <- paste(path, '/EventRegressors', sep="")
subjfiles <- list.files(events, full.names=FALSE, recursive = FALSE) 
subjnames <- unique(regmatches(subjfiles, regexpr("LUDD_FST_.....", subjfiles)))
  
#initializing
datalistRun1 <- list()
datalistRun2 <- list()
datalistRun3 <- list()
datalistRun4 <- list()
eventregs1 <- list()
eventregs2 <- list()
eventregs3 <- list()
eventregs4 <- list()

# number of subjects
dim32 <- length(subjnames)

# Path to timecourses
tcpath <- paste(path, "/Timecourses/4mm", sep="")

# Loop through and create list of all subjects' Run 1 data, Run2 data, etc....
# Do this for event regressors as well
for (i in 1:dim32) {
  for (run in 1:4) {
    filenameEV <- paste(events, '/Subj_', subjnames[i], '_Run', run, '_eventregressors.txt', sep = "")
    matrixiEV <- read.csv(filenameEV, header = TRUE)
    filename <- paste(tcpath, '/Subj_', subjnames[i], '_Run', run, '_4mm_8_ROIs_timecourses.txt', sep = "")
    matrixi <- read.csv(filename, header = TRUE)
    if (run == 1) {
      eventregs1[[i]] <- matrixiEV
      names(eventregs1)[i] <- subjnames[i]
      datalistRun1[[i]] <- matrixi
      names(datalistRun1)[i] <- subjnames[i]
    } else if (run ==2) {
      eventregs2[[i]] <- matrixiEV
      names(eventregs2)[i] <- subjnames[i]
      datalistRun2[[i]] <- matrixi
      names(datalistRun2)[i] <- subjnames[i]
    } else if (run ==3) {
      eventregs3[[i]] <- matrixiEV
      names(eventregs3)[i] <- subjnames[i]
      datalistRun3[[i]] <- matrixi
      names(datalistRun3)[i] <- subjnames[i]
    } else if (run ==4) {
      eventregs4[[i]] <- matrixiEV
      names(eventregs4)[i] <- subjnames[i]
      datalistRun4[[i]] <- matrixi
      names(datalistRun4)[i] <- subjnames[i]
    }
  }
}

# ==Standardize ROI tcs and EV tcs BEFORE concatenation===
# it's important to do this before concatenation because each run's mean signal intensity may be different (and IS, in some cases)
library(psych)
# Z-score
source(paste0(path, '/scale1.R'))
eventregs1z <- mapply(scale1, eventregs1, SIMPLIFY=FALSE)
eventregs2z <- mapply(scale1, eventregs2, SIMPLIFY=FALSE)
eventregs3z <- mapply(scale1, eventregs3, SIMPLIFY=FALSE)
eventregs4z <- mapply(scale1, eventregs4, SIMPLIFY=FALSE)

datalistRun1z <- mapply(scale1, datalistRun1, SIMPLIFY=FALSE)
datalistRun2z <- mapply(scale1, datalistRun2, SIMPLIFY=FALSE)
datalistRun3z <- mapply(scale1, datalistRun3, SIMPLIFY=FALSE)
datalistRun4z <- mapply(scale1, datalistRun4, SIMPLIFY=FALSE)

# ==Now add missing data point at the end of Runs 1 and 3===
missingtp <- rep(NA, 8)
missingtp_events <- rep(NA, 2)
datalistRun1na <-lapply(datalistRun1z, rbind, missingtp)
datalistRun3na <-lapply(datalistRun3z, rbind, missingtp)
eventregs1na <- lapply(eventregs1z, rbind, missingtp_events)
eventregs3na <- lapply(eventregs3z, rbind, missingtp_events)

# ==Now concatenate runs 1 and 2 / Runs 3 and 4===================
# ==ROIs
# Without NAs
Run1and2 <- mapply(rbind, datalistRun1z, datalistRun2z, SIMPLIFY=FALSE)
Run3and4 <- mapply(rbind, datalistRun3z, datalistRun4z, SIMPLIFY=FALSE)
Events1and2 <- mapply(rbind, eventregs1z, eventregs2z, SIMPLIFY=FALSE)
Events3and4 <- mapply(rbind, eventregs3z, eventregs4z, SIMPLIFY=FALSE)
# With NAs
Run1naand2 <- mapply(rbind, datalistRun1na, datalistRun2z, SIMPLIFY=FALSE)
dim(Run1naand2[[1]])
describe(Run1naand2[[1]])

Run3naand4 <- mapply(rbind, datalistRun3na, datalistRun4z, SIMPLIFY=FALSE)
Events1naand2 <- mapply(rbind, eventregs1na, eventregs2z, SIMPLIFY=FALSE)
Events3naand4 <- mapply(rbind, eventregs3na, eventregs4z, SIMPLIFY=FALSE)

# Create list with all ROIs all runs (no event regressors)
#first add NA to the end of Run 2 so we can concatenate runs 2 and 3
Run1naand2na <- lapply(Run1naand2, rbind, missingtp)
Events1naand2na <- lapply(Events1naand2, rbind, missingtp_events)

Allruns <-mapply(rbind, Run1and2, Run3and4, SIMPLIFY=FALSE)
Allrunsna <-mapply(rbind, Run1naand2na, Run3naand4, SIMPLIFY=FALSE)

# Bind Timecourses and Event Regressors 
# One list with all variables (ROIs + EventRegressors)
# Event regressors will be identified by their headers as exogeneous variables
Allvars1and2na <- mapply(cbind, Run1naand2, Events1naand2, SIMPLIFY=FALSE)
dim(Allvars1and2na[[1]])
describe(Allvars1and2na[[1]])
# #remove subject 21 because gimme cannot find a good model for it and this causes the data to not be written
# Allvars1and2na[21] <- NULL
# Allvars1and2na[24] <- NULL
# length(Allvars1and2na)
# dim(Allvars1and2na[[1]]) 

Allvars1and2 <- mapply(cbind, Run1and2, Events1and2, SIMPLIFY=FALSE)
dim(Allvars1and2[[1]])

Allvars3and4 <- mapply(cbind, Run3and4, Events3and4, SIMPLIFY=FALSE)
dim(Allvars3and4[[1]])

Allvars3and4na <- mapply(cbind, Run3naand4, Events3naand4, SIMPLIFY=FALSE)
dim(Allvars3and4na[[1]])

Allvarsallruns <-mapply(rbind, Allvars1and2, Allvars3and4, SIMPLIFY=FALSE)
dim(Allvarsallruns[[1]])

Allvars1and2na_tmp <- mapply(cbind, Run1naand2na, Events1naand2na, SIMPLIFY=FALSE)
Allvarsallrunsna <-mapply(rbind, Allvars1and2na_tmp, Allvars3and4na, SIMPLIFY=FALSE)
dim(Allvarsallrunsna[[1]])

# Load in smooth FIR event regressors # =======================================================

# Event onsets for sFIR
# 6.26.16 updated Run 1  csv to correctly read in only 3 columns 
eventonset.run1 <-read.csv(paste(path, '/FIST_10trials8s_Run1_tnullmax12-001.csv', sep=""))
eventonset.run2 <-read.csv(paste(path, '/FIST_10trials8s_Run2_tnullmax12-001.csv', sep=""))

# Flxibility = 1 Fixation = 0 Control = 2
# Control trials
# initialize vector of 0s same length as each run (122 TPs)
Control_event.run1 <- rep(0,122)
Flex_event.run1 <- rep(0,122)
Control_event.run2 <- rep(0,122)
Flex_event.run2 <- rep(0,122)

flex.onset.run1 <- which(eventonset.run1$TrialNumber == 1) 
control.onset.run1 <- which(eventonset.run1$TrialNumber == 2)
flex.onset.run2 <- which(eventonset.run2$TrialNumber == 1) 
control.onset.run2 <- which(eventonset.run2$TrialNumber == 2)

# Run 1
for (i in 1:10) {
  # Flex trials
  # event onset for trial i (in TRs)
  eventonset.run1.triali <- eventonset.run1[flex.onset.run1[i], 1]/2 +1
  # event duration is always 4 TRs (3 TRs after event onset)
  Flex_event.run1[eventonset.run1.triali:(eventonset.run1.triali+3)] <- 1
  
  #Control trials
  eventonset.run1.triali <- eventonset.run1[control.onset.run1[i], 1]/2 +1
  Control_event.run1[eventonset.run1.triali:(eventonset.run1.triali+3)] <- 1
}

# Run 2
for (i in 1:10) {
  # Flex trials
  # event onset for trial i (in TRs)
  eventonset.run2.triali <- eventonset.run2[flex.onset.run2[i], 1]/2 +1
  # event duration is always 4 TRs (3 TRs after event onset)
  Flex_event.run2[eventonset.run2.triali:(eventonset.run2.triali+3)] <- 1
  
  #Control trials
  eventonset.run2.triali <- eventonset.run2[control.onset.run2[i], 1]/2 +1
  Control_event.run2[eventonset.run2.triali:(eventonset.run2.triali+3)] <- 1
}

# Use template from eventregs1 to get names of each dataframe (named as subj number)

# Concatenate Flex and Control event regressors
eventregs1_sFIR <-cbind(Flex_event.run1,Control_event.run1)
eventregs2_sFIR <-cbind(Flex_event.run2,Control_event.run2)

eventregs1_sFIR.df <- data.frame(eventregs1_sFIR)
colnames(eventregs1_sFIR.df) <- c("Flexibility", "Control")
eventregs2_sFIR.df <- data.frame(eventregs2_sFIR)
colnames(eventregs2_sFIR.df) <- c("Flexibility", "Control")
eventregs1_sFIR.list <- eventregs1
eventregs2_sFIR.list <- eventregs2
for (i in 1:32) {
  for (run in 1:2) {
    if (run == 1) {
      eventregs1_sFIR.list[[i]] <- eventregs1_sFIR.df
    } else if (run ==2) {
      eventregs2_sFIR.list[[i]] <- eventregs2_sFIR.df
    }
  }
}


# Now concatenate runs 1 and 2 / Runs 3 and 4
Events1and2_sFIR <- mapply(rbind, eventregs1_sFIR.list, eventregs2_sFIR.list, SIMPLIFY=FALSE)
Events3and4_sFIR <- mapply(rbind, eventregs1_sFIR.list, eventregs2_sFIR.list, SIMPLIFY=FALSE)

# Bind Timecourses and Event Regressors 
# One list with all variables (ROIs + EventRegressors)
# Event regressors will be identified by their headers as exogeneous variables

Allvars1and2_sFIR <- mapply(cbind, Run1and2, Events1and2_sFIR, SIMPLIFY=FALSE)
describe(Allvars1and2_sFIR[[1]])
Allvars3and4_sFIR <- mapply(cbind, Run3and4, Events3and4_sFIR, SIMPLIFY=FALSE)
describe(Allvars3and4_sFIR[[1]])
Allvarsallruns_sFIR <-mapply(rbind, Allvars1and2_sFIR, Allvars3and4_sFIR, SIMPLIFY=FALSE) #488 x 10
describe(Allvarsallruns_sFIR[[1]])

# GIMME # ====================================================================

# 7.4.2018. Trying cran version again
library(gimme)

out <- paste(path, '/Output/8ROIs/Runs3and4_selfstd_sFIR_mult2IFJ', sep = "")
exog_LIFJ <-c("LIFJ*Flexibility","LIFJ*Control")

fit <- gimmeSEM(data = Allvars3and4_sFIR, 
         out = out,
         header      = TRUE,
         ar          = TRUE, 
         plot        = TRUE,
         subgroup    = TRUE,
         confirm_subgroup = NULL,
         paths       = NULL, #good for when you want to interrogate a certain path that may not be present for the majority of the sample
         exogenous   = c("Flexibility", "Control"),
         mult_vars   = exog_LIFJ,
         mean_center_mult = TRUE,
         #ex_lag = TRUE,
         conv_vars = c("Flexibility", "Control"),
         conv_interval = 2,
         standardize = TRUE,
         groupcutoff = .75, #.75 is what was used for validation papers
         subcutoff   = .51, #this is lower because subgroups can be much smaller n's; 
                            # .51 is what algorithm was validated with;
                            # .50 is the defaul
         diagnos     = FALSE)

setwd(out)

# make sure to save a workspace image so you can look at the results later
save.image(file = "workspace.Rdata")
