
###Analyzing behavioral data from computer-based FIST adult task#######

## =====Initializing=============================================================
rm(list= ls())
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
getwd()

acc.dat <- read.csv("Acc_data_computer_task_final4.9.18.csv", header = T)

## =====Descriptives=============================================================

library(psych)
sumstats <- with(acc.dat, psych::describe(acc.dat[,2:ncol(acc.dat)]))

# Trial-level accuracy
acc.dat[,2]
perc_perfect_score <-(sum(acc.dat[,2]==6))/length(acc.dat[,2])
sumstats[1,]
trial_level_acc_perc <- sumstats[1,3:4]/6
  trial_level_acc_perc
  
# Selection-level accuracy
selection_level_acc_perc <-sumstats[2:4,3:4]/6
  selection_level_acc_perc
  
# Does accuracy change across selections?
  #using repeated measures ANOVA to test
    # reshape data 
    datalong <- reshape(data=acc.dat, 
                        varying =3:5, 
                        v.names = "selection_level_accuracy", 
                        timevar = "Selection",
                        idvar = "PID",
                        direction = "long")
    datalong$Selection <-factor(datalong$Selection)
  #contrasts
    contrasts(datalong$Selection) <- contr.poly(3)
    modelRMAOV <-aov(selection_level_accuracy~Selection+Error(factor(PID)), data=datalong)  
    print(summary(modelRMAOV))  
    summary(modelRMAOV, split=list(Selection=list("Linear"=1, "Quadratic"=2, "Cubic"=3)))

## =====Graph out selection by selection accuracy================================
# Supplementary Figure 1
    
# Reshape data for violin plots
CBAcc.vector<-stack(acc.dat, select=c("AccFlex1", 
                                      "AccFlex2", 
                                      "AccFlex3"))

#change factor names
library(plyr)
CBAcc.vector$ind <- revalue(CBAcc.vector$ind, c("AccFlex1"="Selection1",
                                                "AccFlex2"="Selection2",
                                                "AccFlex3"="Selection3"))

#change column names
colnames(CBAcc.vector) <- c("Accuracy", "Selection")

#create violin pltot
library(ggplot2)
p <- ggplot(CBAcc.vector, aes(x=Selection, y=Accuracy)) +
  geom_boxplot(width=.1, fill="white", position=position_dodge(.5)) +
  theme_classic() + 
  labs(x="", y = "Accuracy") +
  ggtitle("Computer-Based Task Accuracy") +
  theme(plot.title = element_text(hjust = .5, size=16),
        axis.text=element_text(size=14), axis.title.y=element_text(size=14), axis.title.x=element_text(size=14))
p

ggsave(p, 
       filename = "plots/computer based task/CB_Task_acc.pdf",
       device=cairo_pdf,
       width=5,
       height=5,
       units = "in")

dev.off()

## =====Histogram of total correct==============================================
# Supplementary Figure 2

summary(acc.dat$TotalCorrect)
library(psych)
describe(acc.dat$TotalCorrect)
p <- ggplot(acc.dat, aes(x=TotalCorrect)) +
  theme_classic() +
  geom_histogram(binwidth=1, color="black", fill="gray87") +
  geom_vline(aes(xintercept=mean(TotalCorrect)),
            color="blue", linetype="dashed", size=1) +
  scale_x_continuous(breaks=seq(min(acc.dat$TotalCorrect), max(acc.dat$TotalCorrect), by = 1)) +
  scale_y_continuous(breaks=seq(0,22,2)) +
  labs(x="Total Selections Correct", y = "Frequency") +
  ggtitle("Computer-Based Task Accuracy") +
  theme(plot.title = element_text(hjust = .5, size=16), 
        axis.text=element_text(size=12), axis.title.y=element_text(size=14),
        axis.title.x=element_text(size=14))
p
ggsave(p, 
       filename = "plots/computer based task/CB_selectionacc_hist.pdf",
       device=cairo_pdf,
       width=5,
       height=5,
       units = "in")

dev.off()

#geom_histogram(aes(y=..density..), binwidth=1, color="black", fill="white") +  geom_density(alpha=.2, fill = "#FF6666") +

## =====Frequency of error types by selection===================================

# Reshape data, Errortype1 never occurred for any participant, so I removed it from the graph
CBError.vector<-stack(acc.dat, select=c( 
                                      "ErrorType2", 
                                      "ErrorType3",
                                      "ErrorType4"))
#change factor names
library(plyr)
CBError.vector$ind <- revalue(CBError.vector$ind, 
                              c("ErrorType2"="Error Type 1", 
                                "ErrorType3"="Error Type 2", 
                                "ErrorType4"="Error Type 3"))

colnames(CBError.vector) <- c("ErrorFrequency", "ErrorType")

#Error bar information
# library(dplyr)
# CBError.vector %>%
#   group_by(ErrorType) %>% # the grouping variable
#   summarize(n())
# 
# 
#   summarise(count = n()) 
frequency_by_errortype <- aggregate(CBError.vector$ErrorFrequency, by=list(CBError.vector$ErrorType), FUN=sum)
#frequency of error type 1 (ie error type 2 according to acc.dat)
frequency_by_errortype$x <- sprintf("%1.2f", 100*frequency_by_errortype$x/29) 

p <- ggplot(CBError.vector, aes(x=ErrorType, y=ErrorFrequency)) +
  theme_classic() + 
  geom_col(fill="gray45") +
  geom_text(aes(label = ErrorFrequency), vjust=-0.3, size=3.5) +
  labs(x="", y = "Frequency") +
  ggtitle("Error Type Frequency") +
  theme(plot.title = element_text(hjust = .5))
p

p <- ggplot(frequency_by_errortype, aes(x=Group.1, y=x)) +
  geom_col(color = "black", fill = "gray87") +
  geom_text(aes(label = x), vjust=-0.5, size=3.5) +
  labs(x="", y = "Proportion of All Errors (%)") +
  ggtitle("Error Type Frequency") +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5))
p

## =====Frequency of dimensions (4) by selection (3)============================

library(psych)
describe(acc.dat[,11:30])
# Reshape data
CBDim1.vector<-stack(acc.dat, select=c( 
  "Dim1_Color", 
  "Dim1_Shape",
  "Dim1_Size",
  "Dim1_Number"))
label <- rep(1,128)
label.f <-factor(label, labels="Selection 1")
CBDim1.vector <-cbind(CBDim1.vector, label.f)
CBDim1.vector$ind <- revalue(CBDim1.vector$ind, c("Dim1_Color"="Color", 
                                                  "Dim1_Shape"="Shape", 
                                                  "Dim1_Size" = "Size", 
                                                  "Dim1_Number"="Number"))

CBDim2.vector<-stack(acc.dat, select=c( 
  "Dim2_Color", 
  "Dim2_Shape",
  "Dim2_Size",
  "Dim2_Number"))
label <- rep(1,128)
label.f <-factor(label, labels="Selection 2")
CBDim2.vector <-cbind(CBDim2.vector, label.f)
CBDim2.vector$ind <- revalue(CBDim2.vector$ind, c("Dim2_Color"="Color", 
                                                  "Dim2_Shape"="Shape", 
                                                  "Dim2_Size" = "Size", 
                                                  "Dim2_Number"="Number"))

CBDim3.vector<-stack(acc.dat, select=c( 
  "Dim3_Color", 
  "Dim3_Shape",
  "Dim3_Size",
  "Dim3_Number"))
label <- rep(1,128)
label.f <-factor(label, labels="Selection 3")
CBDim3.vector <-cbind(CBDim3.vector, label.f)
CBDim3.vector$ind <- revalue(CBDim3.vector$ind, c("Dim3_Color"="Color", 
                                                  "Dim3_Shape"="Shape", 
                                                  "Dim3_Size" = "Size", 
                                                  "Dim3_Number"="Number"))

CBDim_by_selection <- rbind(CBDim1.vector, CBDim2.vector, CBDim3.vector)

colnames(CBDim_by_selection) <- c("DimensionFrequency", "Dimension", "Selection")

library(plyr)
p_meds <- ddply(CBDim_by_selection, .(Selection, Dimension), summarise, med = median(DimensionFrequency))

# Graphs dimensions by selection, with dimension type as grouping variable

p <- ggplot(CBDim_by_selection, aes(x=Selection, y=DimensionFrequency, group=interaction(Selection, Dimension))) +
  #scale_fill_manual(values=c("azure1","#999999")) +
  geom_boxplot(width=.5, aes(fill=Dimension), position=position_dodge(.5)) +
  theme_classic() + 
  labs(x="", y = "Frequency") +
  ggtitle("Dimension Choice") +
  theme(plot.title = element_text(hjust = .5, size=16),
        axis.text=element_text(size=12), axis.title.y=element_text(size=14),
        axis.title.x=element_text(size=14)) +
  geom_text(data = p_meds, aes(x=Selection, y=med, label = med), vjust=-0.5, size=3.5, position=position_dodge(.5))
p

ggsave(p, 
       filename = "plots/computer based task/CB_selection_by_dim.pdf",
       device=cairo_pdf,
       width=6,
       height=5,
       units = "in")

dev.off()

# Graphs selection by dimension, with selection as grouping variable

p_meds <- ddply(CBDim_by_selection, .(Selection, Dimension), summarise, med = median(DimensionFrequency))
p <- ggplot(CBDim_by_selection, aes(x=Dimension, y=DimensionFrequency, group=interaction(Selection, Dimension))) +
  #scale_fill_manual(values=c("azure1","#999999")) +
  geom_boxplot(width=.5, aes(fill=Selection), position=position_dodge(.5)) +
  theme_classic() + 
  labs(x="", y = "Frequency") +
  ggtitle("Dimension Choice") +
  theme(plot.title = element_text(hjust = .5, size=16),
        axis.text=element_text(size=12), axis.title.y=element_text(size=14))
p

## =====Frequency of dimensions collapsed across trials=========================
#         **Adding selection number information**

library(psych)
describe(acc.dat[,23:26])

# Reshape data
CBDim.vector<-stack(acc.dat, select=c( 
  "Dim_Color", 
  "Dim_Shape",
  "Dim_Number",
  "Dim_Size"
  ))
#change factor names
#library(plyr)
CBDim.vector$ind <- revalue(CBDim.vector$ind, 
                              c("Dim_Color"="Color", 
                                "Dim_Shape"="Shape", 
                                "Dim_Size"="Size",
                                "Dim_Number"="Number"))

colnames(CBDim.vector) <- c("DimensionPerc", "Dimension")
CBdim_all <- cbind(CBDim.vector[,1], CBDim_by_selection)
colnames(CBdim_all) <- c("DimensionPerc", "DimensionFrequency", "Dimension", "Selection")
CBdim_all$DimensionPerc <- CBdim_all$DimensionPerc*6

# Descriptives for dimension choice at trial-level (collapsed across selections)
describeBy((CBdim_all$DimensionPerc)/6, CBdim_all$Dimension)

q <- ggplot(CBDim_by_selection, aes(x=Dimension, y=DimensionFrequency)) +
  theme_classic() + 
  geom_boxplot(data = CBdim_all, aes(x=Dimension, y=DimensionPerc),
               width=.6, fill="gray87", color="black", position=position_dodge(.8)) +
  geom_boxplot(width=.5, aes(fill=Selection), 
               position=position_dodge(.5),
               coef = 0, outlier.shape=NA,
               alpha = .75) +
  labs(x="", y = "Trials with identified dimension (no.)") +
  ylim(c(0,6)) +
  ggtitle("Dimension Choice") +
  theme(plot.title = element_text(hjust = .5, size = 16),
        axis.text=element_text(size=12), axis.title.y=element_text(size=14),
        axis.title.x=element_text(size=14),
        axis.title=element_text(size=14))
q
ggsave(q, 
       filename = "plots/computer based task/Dimension_by_Selection.pdf",
       device=cairo_pdf,
       width=6,
       height=5,
       units = "in")
dev.off() 

## =====Percentage of each excluded dimension==================================

# Reshape data
CBDim.vector<-stack(acc.dat, select=c( 
  "Perc_excluded_color", 
  "Perc_excluded_shape",
  "Perc_excluded_size",
  "Perc_excluded_number"))
#change factor names
library(plyr)
library(ggplot2)
CBDim.vector$ind <- revalue(CBDim.vector$ind, 
                            c("Perc_excluded_color"="Color", 
                              "Perc_excluded_shape"="Shape", 
                              "Perc_excluded_size"="Size",
                              "Perc_excluded_number"="Number"))

colnames(CBDim.vector) <- c("DimensionPercExcluded", "Dimension")

p_meds <- ddply(CBDim.vector, .(Dimension), summarise, med = median(DimensionPercExcluded))
#p_meds$med[3:4] <-sprintf("%1.2f", p_meds$med[3:4])
p <- ggplot(CBDim.vector, aes(x=Dimension, y=DimensionPercExcluded)) +
  theme_classic() + 
  geom_boxplot(width=.75, fill="gray87", color="black") +
  labs(x="", y = "Proportion of Trials Excluding Dimension") +
  ylim(c(0,1)) +
  ggtitle("Excluded Dimension") +
  theme(plot.title = element_text(hjust = .5, size=20), axis.text=element_text(size=12), axis.title.y=element_text(size=14)) +
  geom_text(data = p_meds, aes(x=Dimension, y=med, label = sprintf("%1.2f", round(med, digits = 2))), vjust=-0.5, size=3.5)
p
## =====Histogram of unique dimension patterns==================================

library(psych)
describe(acc.dat$Perc_unique_dim_pattern)
p <- ggplot(acc.dat, aes(x=Perc_unique_dim_pattern)) +
  theme_classic() +
  geom_histogram(binwidth=.15, color="black", fill="gray87") +
  geom_vline(aes(xintercept=mean(Perc_unique_dim_pattern)),
             color="blue", linetype="dashed", size=1) +
  labs(x="Percentage of trials with unique selection pattern", y = "Frequency") +
  ggtitle("Computer-Based Task Selection Patterns") +
  theme(plot.title = element_text(hjust = .5, size=16), axis.text=element_text(size=12), axis.title.y=element_text(size=14))
p

# Histogram of frequency of repeated selection patterns

acc.dat[is.na(acc.dat)] <- 0

library(plyr)
repeated_dimpattern <-count(acc.dat, 'Repeated_dim_pattern')
repeated_dimpattern[,2] <- repeated_dimpattern[,2]/32
repeated_dimpattern[order(repeated_dimpattern[,2], decreasing=TRUE),]

frequency_by_uniquedimpattern <- count(acc.dat, 'Freq_repeated_dim_pattern')
frequency_by_uniquedimpattern[,2] <- frequency_by_uniquedimpattern[,2]/32
frequency_by_uniquedimpattern

sump <- ggplot(acc.dat, aes(x=Freq_repeated_dim_pattern)) +
  theme_classic() +
  geom_histogram(binwidth = .8, color="black", fill="gray87") +
  geom_vline(aes(xintercept=mean(Freq_repeated_dim_pattern)),
             color="blue", linetype="dashed", size=1) +
  labs(x="Percentage of trials with repeated selection pattern", y = "Frequency") +
  ggtitle("Computer-Based Task Selection Patterns") +
  theme(plot.title = element_text(hjust = .5, size=16), axis.text=element_text(size=12), axis.title.y=element_text(size=14))
p


