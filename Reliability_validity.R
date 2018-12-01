
#==========Analyzing behavioral data from fMRI FIST adult task============

rm(list= ls())
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
dir <- dirname(path)
setwd(path)
getwd()
dev.off()

# ==========LOAD DATA ========================================================================
acc.dat <- read.csv("Acc_data_fMRI_task_newDV_final4.23.18.csv", header = T, na = "") 
acc_CB.dat <- read.csv("Acc_data_computer_task_final4.9.18.csv", header = T)
library(readxl)
RBQ.dat <- read_excel(paste0(dirname(path), '/FST_export_7.24.18.xlsx'), na="-999")
RBQ.dat <- RBQ.dat[52:81]
names(RBQ.dat)

# Load item-level data from subject-level csvs
data = '/Volumes/Groups/LUddin_Lab/Lab/Experiments/FIST/Data_adults'
# nTPs x 11 matrix per subject for n=150
FILES <- list.files(data)
# initialize empty dataframe for items in each run, where rows are subjects and columns are item-level accuracy
items_Run1 <- matrix(NA, nrow = 32, ncol = 10)
items_Run2 <- matrix(NA, nrow = 32, ncol = 10)
items_Run3 <- matrix(NA, nrow = 32, ncol = 10) 
items_Run4 <- matrix(NA, nrow = 32, ncol = 10)
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

# items_Run1 is a kxm matrix with item responses, k subjects in tows and m items in columns

#can concatenate Runs 1 and 2 to get a 32 x 20 matrix 

items_Run1and2 <- cbind(items_Run1, items_Run2)
items_Run3and4 <- cbind(items_Run3, items_Run4)

# ==========Internal Consistency====================================================

#install.packages("DescTools")
library(DescTools)

CronbachAlpha(items_Run1, conf.level = NA, cond = TRUE, na.rm = FALSE)
CronbachAlpha(items_Run2, conf.level = NA, cond = TRUE, na.rm = FALSE)
CronbachAlpha(items_Run3, conf.level = NA, cond = TRUE, na.rm = FALSE)
CronbachAlpha(items_Run4, conf.level = NA, cond = TRUE, na.rm = FALSE)

# Combining Runs 1 and 2?
CronbachAlpha(items_Run1and2, conf.level = .95, cond = TRUE, na.rm = FALSE)

# Combining Runs 3 and 4?
CronbachAlpha(items_Run3and4, conf.level = .95, cond = TRUE, na.rm = FALSE)


# Investigating item 8 Run 1 and item 2 Run 3

  # plot of mean acc for items in Run 1
  library(ggplot2)
  column.names <- c("Item1", "Item2", "Item3", "Item4", "Item5", 
                      "Item6", "Item7", "Item8", "Item9", "Item10")
  Run1 <- data.frame(items_Run1)
  colnames(Run1)<-column.names
  library(psych)
  describe(Run1)
  Run1<-stack(Run1, select=c("Item1", "Item2", "Item3", "Item4", "Item5", 
                             "Item6", "Item7", "Item8", "Item9", "Item10"))
  colnames(Run1) <- c("Accuracy", "Item")
  p <- ggplot(Run1, aes(x=Item, y=Accuracy)) + geom_boxplot()
  p

  
  # Run 3
  Run3 <- data.frame(items_Run3)
  colnames(Run3)<-column.names
  describe(Run3)
  Run3<-stack(Run3, select=c("Item1", "Item2", "Item3", "Item4", "Item5", 
                             "Item6", "Item7", "Item8", "Item9", "Item10"))
  colnames(Run3) <- c("Accuracy", "Item")
  p <- ggplot(Run3, aes(x=Item, y=Accuracy)) + geom_boxplot()
  p

# ==========Test-retest reliability==================================================

# Generating dataframe for input to ICC function, rows as subjects and columns as ratings per run
  # grabbing Acc-RT (effificency) values
  ICC_data <- acc.dat[, 14:17]
ICC_data_Runs12 <-ICC_data[, 1:2]
ICC_data_Runs34 <-ICC_data[, 3:4]

# Using ICC as the measure of test-retest reliability across all 4 runs

library(psych)
describe(ICC_data)
ICC_results <- ICC(ICC_data, missing=FALSE, alpha=.05)

ICC_results$results 
ICC_results$summary #anova summary table
ICC_results$stats #anova statistics 
ICC_results$MSW #mean square within based upon the anova
ICC_results$n.obs #should be 32
ICC_results$n.judge #should be 4
ICC_results$results[5,c(2,7:8)] #averaged across all 4 runs
ICC_results$results[2,c(2,7:8)] #single run reli

# How many repeated measures (runs) are needed for good reliability? [>.75]
min_reli <- .75
lower_CI_ICC2 <- ICC_results$results[2,7]
runs <- (min_reli*(1-lower_CI_ICC2))/(lower_CI_ICC2*(1-min_reli))
runs

ICC_results_2runs <-ICC(ICC_data_Runs12, missing=TRUE, alpha=.05)
ICC_results_2runs$results[5,c(2,7:8)]

ICC_results_2runs <-ICC(ICC_data_Runs34, missing=TRUE, alpha=.05)
ICC_results_2runs$results[5,c(2,7:8)]

##===Validity analyses: CB-fMRI task correlations========================================

library("ggplot2")

# Average accuracy across runs
avg.acc <-rowMeans(acc.dat[,2:5])
cor.test(acc_CB.dat[,2], avg.acc)
avg.accdf <- as.data.frame(avg.acc)
colnames(avg.accdf) <- "avg.acc.fMRI.Flex"
acc.dat$avg.acc.fMRI.Flex <-avg.accdf

CB_fmri <- cbind(acc_CB.dat[,2], avg.accdf)
p <- ggplot(CB_fmri, aes(x=acc_CB.dat[, 2], y=avg.acc.fMRI.Flex)) +
  geom_point(size=2, shape=23) +   
  geom_smooth(method=lm) +
  labs(x="Computer-Based Task Acc", y = "fMRI Task Acc") +
  #ggtitle("Computer-fMRI Task Convergent Validity") +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5, size=16), axis.text=element_text(size=14), axis.title.y=element_text(size=14), axis.title.x=element_text(size=14)) +
  ylim(0,1)
p

ggsave(p, 
       filename = "plots/Computer_fMRI_Convergent_Validity_Acc.pdf",
       device=cairo_pdf,
       width=5,
       height=5,
       units = "in")

dev.off()