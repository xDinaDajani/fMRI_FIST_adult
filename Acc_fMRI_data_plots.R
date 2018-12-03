
###Creating plots from behavioral data from fMRI FIST adult task#######

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
getwd()

# acc.dat <- read.csv("Acc_RT_data_DD.csv", header = T)
# head(acc.dat)
# dim(acc.dat) #should be 32 by 18.
# 
# summary(acc.dat)
# acc.dat$meanfMRIacc <-rowMeans(acc.dat[,6:9])
# acc.dat$meanfMRIacc_control <-rowMeans(acc.dat[,10:13])
# write.csv(acc.dat, "Acc_RT_data_DD_v2.csv", row.names=F)
acc.dat <- read.csv("Acc_data_fMRI_task_newDV_final4.23.18.csv", header = T) 


hist(acc.dat$fMRIRun1MeanFlexTrialAcc, main = "fMRI Run 1 Flex Trials Histogram")
#plot(acc.dat$ComputerMeanFlexTrialAcc, acc.dat$meanfMRIacc, xlim=c(0,1), ylim=c(0,1), main="Convergent Validity with Computer-based Task", xlab = "Computer-based Task Accuracy", ylab="fMRI Task Accuracy")
#abline(lm(acc.dat$meanfMRIacc ~ acc.dat$ComputerMeanFlexTrialAcc, data=acc.dat), col="blue", lwd=3, lty=2)


## =======FIGURE 4: Plot ACC/RT DV Across Runs=========================  
# Reshape data for violin plots
Acc.vector<-stack(acc.dat, select=c("accRT_DV_Run1", 
                                      "accRT_DV_Run2", 
                                      "accRT_DV_Run3",
                                      "accRT_DV_Run4"))

#change factor names
library(plyr)
Acc.vector$ind <- revalue(Acc.vector$ind, c("accRT_DV_Run1"="Run1",
                                            "accRT_DV_Run2"="Run2",
                                            "accRT_DV_Run3"="Run3",
                                            "accRT_DV_Run4"="Run4"))

#change column names
colnames(Acc.vector) <- c("FlexibilityScore", "Run")

#create violin plot
library(ggplot2)
p <- ggplot(Acc.vector, aes(x=Run, y=FlexibilityScore)) +
  geom_violin(position=position_dodge(.5), scale="width", fill = "gray60", trim=FALSE) +
  geom_boxplot(width=.1, fill="white", position=position_dodge(.5)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, color="red") +
  theme_classic() + 
  ylim(0,10) +
  labs(x="", y = "Accuracy-RT") +
  ggtitle("fMRI Task Performance") +
  theme(plot.title = element_text(hjust = .5, size=16), axis.text=element_text(size=12), axis.title.y=element_text(size=14))
p

ggsave(p, 
       filename = "plots/fMRI task/Figure3.pdf",
       device=cairo_pdf,
       width=5,
       height=5,
       units = "in")

#===FIGURE S3: VIOLIN PLOTS===============================================

# Reshape data for violin plots
fmriAcc.vector<-stack(acc.dat, select=c("fMRIRun1MeanFlexTrialAcc", 
                                        "fMRIRun2MeanFlexTrialAcc", 
                                        "fMRIRun3MeanFlexTrialAcc", 
                                        "fMRIRun4MeanFlexTrialAcc"))
label <- rep(1,128)
label.f <-factor(label, labels="Flexibility trials")
fmriAcc.vector <-cbind(fmriAcc.vector, label.f)
fmriAcc_Cnt.vector<-stack(acc.dat, select=c("fMRIRun1CntAcc", 
                                            "fMRIRun2CntAcc", 
                                            "fMRIRun3CntAcc", 
                                            "fMRIRun4CntAcc"))
label <- rep(2,128)
label.f <-factor(label, labels="Control trials")
fmriAcc_Cnt.vector <-cbind(fmriAcc_Cnt.vector, label.f)
fmri_flex_control <- rbind(fmriAcc.vector, fmriAcc_Cnt.vector)

#change factor names
library(plyr)
fmri_flex_control$ind <- revalue(fmri_flex_control$ind, c("fMRIRun1MeanFlexTrialAcc"="Run1", 
                                                          "fMRIRun2MeanFlexTrialAcc"="Run2", 
                                                          "fMRIRun3MeanFlexTrialAcc" = "Run3", 
                                                          "fMRIRun4MeanFlexTrialAcc"="Run4", 
                                                          "fMRIRun1CntAcc"="Run1", 
                                                          "fMRIRun2CntAcc"="Run2", 
                                                          "fMRIRun3CntAcc"="Run3", 
                                                          "fMRIRun4CntAcc"="Run4"))

#change column names
colnames(fmri_flex_control) <- c("MeanAccuracy", "Run", "TrialType")

#create violin pltot

library(ggplot2)
p <- ggplot(fmri_flex_control, aes(x=Run, y=MeanAccuracy, group=interaction(Run, TrialType))) +
  geom_violin(position=position_dodge(.5), aes(fill=TrialType), scale="width") + 
  scale_fill_manual(values=c("azure1","#999999")) +
  geom_boxplot(width=.1, fill="white", position=position_dodge(.5)) +
  theme_classic() + 
  labs(x="", y = "Mean accuracy") +
  ggtitle("fMRI Task Accuracy") +
  theme(plot.title = element_text(hjust = .5, size=16), axis.text=element_text(size=12), axis.title.y=element_text(size=14))
p
ggsave(p, 
       filename = "plots/fMRI task/SuppFig6.pdf",
       device=cairo_pdf,
       width=6.5,
       height=5,
       units = "in")

