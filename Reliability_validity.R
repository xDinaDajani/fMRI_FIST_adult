
#==========Analyzing behavioral data from fMRI FIST adult task============

rm(list= ls())
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
getwd()
dev.off()

# ==========LOAD DATA for internal consistency==============================
acc.dat <- read.csv("Acc_data_fMRI_task_newDV_final4.23.18.csv", header = T, na = "") 
acc_CB.dat <- read.csv("Acc_data_computer_task_final4.9.18.csv", header = T)



# Load item-level data from subject-level csvs
data = '/Volumes/Groups/LUddin_Lab/Lab/Experiments/FIST/Data_adults'
FILES <- list.files(data)
# initialize empty dataframe for items in each run, where rows are subjects and columns are item-level accuracy
items_Run1 <- matrix(, nrow = 32, ncol = 10)
items_Run2 <- matrix(, nrow = 32, ncol = 10)
items_Run3 <- matrix(, nrow = 32, ncol = 10) 
items_Run4 <- matrix(, nrow = 32, ncol = 10)
for (i in 1:length(FILES)) {
  for (run in 1:4) {
    pattern = paste('*_Run', run, '_*', sep="")
    filename = list.files(paste(data, '/', FILES[i], '/AccRT', sep=""), pattern=pattern)
    filepath = paste(data, '/', FILES[i], '/AccRT/', filename, sep="")
    if (i ==25) {
      filepath = filepath[1]
    }
    FILE = read.csv(file=filepath, header = T, sep = ",")
    #temporary file of subject-level accuracy items for a single run
    TempData <- subset(FILE, select = "Acc_Flex")
    TempData <- TempData[1:10, ]
    # there is something wrong with this if statement
    if (run == 1) {
      items_Run1[i, ] <- TempData
    } else if (run == 2) {
      items_Run2[i, ] <- TempData
    } else if (run == 3) {
      items_Run3[i, ] <- TempData
    } else if (run == 4) {
      items_Run4[i, ] <- TempData
    }
  }
}

# items_Run1 is a kxm matrix with item responses, k subjects in rows and m items in columns

#can concatenate Runs 1 and 2 to get a 32 x 20 matrix 

items_Run1and2 <- cbind(items_Run1, items_Run2)
items_Run3and4 <- cbind(items_Run3, items_Run4)

# ==========Internal Consistency====================================================

#install.packages("DescTools")
library(DescTools)

# Combining Runs 1 and 2
CronbachAlpha(items_Run1and2, conf.level = .95, cond = TRUE, na.rm = FALSE)

# Combining Runs 3 and 4
CronbachAlpha(items_Run3and4, conf.level = .95, cond = TRUE, na.rm = FALSE)

# ==========Test-retest reliability==================================================

# Histogram of combined acc-RT dependent variable per run. These DVs will be used to calculate ICC across all runs
hist(x = acc.dat$accRT_DV_Run1)
hist(x = acc.dat$accRT_DV_Run2)
hist(x = acc.dat$accRT_DV_Run3)
hist(x = acc.dat$accRT_DV_Run4)

# Generating dataframe for input to ICC function, rows as subjects and columns as ratings per run

ICC_data <- acc.dat[, 14:17]

# Using ICC as the measure of test-retest reliability across all 4 runs

library(psych)
ICC_results <- ICC(ICC_data, missing=TRUE, alpha=.05)

ICC_results$results 
ICC_results$summary #anova summary table
ICC_results$stats #anova statistics 
ICC_results$MSW #mean square within based upon the anova
ICC_results$n.obs #should be 32
ICC_results$n.judge #should be 4

##===Validity analyses: CB-fMRI task correlations========================================

library("ggplot2")

# Average acc_RT across runs
avg.acc_RT <- rowMeans(acc.dat[,14:17])
avg.acc_RTdf <- as.data.frame(rowMeans(acc.dat[,14:17]))
colnames(avg.acc_RTdf) <- "avg.acc_RT"
acc.dat$avg.acc_RT <-avg.acc_RTdf$avg.acc_RT

# Correlate the computer-based Flex_acc variable with averaged acc-RT variable across runs
cor.test(acc_CB.dat[,2], avg.acc_RT)
CB_fmri <- cbind(acc_CB.dat[,2], avg.acc_RTdf)
p <- ggplot(CB_fmri, aes(x=acc_CB.dat[, 2], y=avg.acc_RT)) +
  geom_point(size=2, shape=23) +   
  geom_smooth(method=lm) +
  labs(x="Computer-Based Task Acc", y = "fMRI Task Acc-RT") +
  ggtitle("Computer-fMRI Task Convergent Validity") +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5, size=16), axis.text=element_text(size=14), axis.title.y=element_text(size=14), axis.title.x=element_text(size=14)) +
  ylim(0,10)
p

ggsave(p, 
       filename = "plots/fMRI task/Computer_fMRI_Convergent_Validity.pdf",
       device=cairo_pdf,
       width=5,
       height=5,
       units = "in")

dev.off()

#========== Validity analyses: BRIEF-fMRI task correlations==============================

library(foreign)
library("ggplot2")
BRIEF.dat <- read.spss("/Volumes/Groups/LUddin_Lab/Lab/Dina Dajani/Projects/FIST_GLM/Data/Adult_BRIEF_data_16.sav", to.data.frame = T)

# Average acc_RT across runs
avg.acc_RT <- rowMeans(acc.dat[,14:17])
avg.acc_RTdf <- as.data.frame(rowMeans(acc.dat[,14:17]))
colnames(avg.acc_RTdf) <- "avg.acc_RT"
acc.dat$avg.acc_RT <-avg.acc_RTdf$avg.acc_RT

# Create dataframe the merged spss file and accRT file based on PID

colnames(BRIEF.dat)[1] <- "PID"
BRIEF_fmridata <- merge(BRIEF.dat, acc.dat, by="PID")
attach(BRIEF_fmridata)
cor.test(Shift_T, avg.acc_RT)
ggplot(acc.dat, aes(x=avg.acc_RT, y=Shift_T)) +
  geom_point(size=2, shape=23) +   
  geom_smooth(method=lm, color="blue") +
  geom_point(aes(x=avg.acc_RT, y=Inhibit_T), size=2, shape=22) +
  geom_smooth(aes(x=avg.acc_RT, y=Inhibit_T), method=lm, color="red") +
  geom_point(aes(x=avg.acc_RT, y=WorkingMem_T), size=2, shape=21) +
  geom_smooth(aes(x=avg.acc_RT, y=WorkingMem_T), method=lm, color="gray22") +
  theme_classic() +
  ggtitle("fMRI Task Ecological Validity") +
  labs(x="fMRI Task Acc-RT", y = "Executive Function") +
  theme(plot.title = element_text(hjust = .5, size=16), axis.text=element_text(size=14), axis.title.y=element_text(size=14), axis.title.x=element_text(size=14))
ggsave("Computer_fMRI_Ecological_Validity.pdf")

dev.off()

cor.test(Inhibit_T, avg.acc_RT)
cor.test(WorkingMem_T, avg.acc_RT)

cor.test(Shift_T, Inhibit_T) 
cor.test(Shift_T, WorkingMem_T)
cor.test(Inhibit_T, WorkingMem_T)

# Correlation table between BRIEF scores and average Acc/RT
subset.cor <- BRIEF_fmridata[c("avg.acc_RT", "Shift_T", "Inhibit_T", "WorkingMem_T")]
colnames(subset.cor) <- c("Average Acc-RT", "BRIEF Shift", "BRIEF Inhibition", "BRIEF Working Memory")
#install.packages("apaTables")
library(apaTables)
apa.cor.table(subset.cor, filename = "CORR.doc")

detach(BRIEF_fmridata)

#========== Validity analyses: RBQ-fMRI task correlations==============================

# Average acc_RT across runs
avg.acc_RT <- rowMeans(acc.dat[,14:17])
avg.acc_RTdf <- as.data.frame(rowMeans(acc.dat[,14:17]))
colnames(avg.acc_RTdf) <- "avg.acc_RT"
acc.dat$avg.acc_RT <-avg.acc_RTdf$avg.acc_R
colnames(BRIEF.dat)[1] <- "PID"
RBQ <- c("PID", "AdultReptitiveBehaviorRBQ_IS_Score",
         "AdultReptitiveBehaviorRBQ_RMB_Score",
         "AdultReptitiveBehaviorRBQ_Total_Score")
RBQ.dat<-BRIEF.dat[RBQ]
RBQ_fmridata <- merge(RBQ.dat, acc.dat, by="PID")

library(psych)
describe(RBQ_fmridata)
hist(RBQ_fmridata$AdultReptitiveBehaviorRBQ_IS_Score)
hist(RBQ_fmridata$AdultReptitiveBehaviorRBQ_RMB_Score)
hist(RBQ_fmridata$AdultReptitiveBehaviorRBQ_Total_Score)

cor.test(AdultReptitiveBehaviorRBQ_IS_Score, avg.acc_RT)
cor.test(AdultReptitiveBehaviorRBQ_RMB_Score, avg.acc_RT)
cor.test(AdultReptitiveBehaviorRBQ_Total_Score, avg.acc_RT)

