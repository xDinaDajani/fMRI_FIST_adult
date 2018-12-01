# GIMME Analyses Flexible Item Selection Task (FIST)
rm(list=ls())
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
script_version <- 'v43*'
out <- Sys.glob(paste(path, '/Pegasus/1Output/8ROIs/', script_version, sep = ""))
setwd(out)

load("workspace.Rdata")
rm(list=setdiff(ls(), "fit"))

#Print names in list
#names(fit)
library(psych)
library(ggplot2)

#=======Summary fit stats=======================================================================
# Check residual matrix to determine why fit is poor
# Good fit does not ensure model is correct, only that it is plausible
# If no degrees of freedom are available, then the model is saturated and fit will always be perfect; with few degrees of freedom, fit is likely to be very good; if many dof are available, fit could be quite bad, so good fit is more impressive; this is why fit indices are adjusted for dof.

df <- sum(fit$fit[,3]==0)
df1 <- sum(fit$fit[,3]==1)
table(fit$fit[,3]) # frequency of dof across participants
n <- nrow(fit$fit)
cat(sprintf("%s out of %s Participants have 0 degrees of freedom.\n%s Participants have 1 dof: %s and %s and %s", df, n, df1, fit$fit[which(fit$fit[,3]==1),1][1], fit$fit[which(fit$fit[,3]==1),1][2], fit$fit[which(fit$fit[,3]==1),1][3]))
# participants with 0 dof
fit$fit[which(fit$fit[,3]==0),1]


chisq.stats <- as.data.frame(describe(fit$fit[, "pvalue"]))
fit$fit$chisq_poorfit <- ifelse(fit$fit[, "pvalue"]<.05, 1,0)
cat(sprintf("%d out of %s participant(s) (with >0 dof) had poor chisquare fit: pvalue %0.2f", sum(fit$fit$chisq_poorfit, na.rm = TRUE), n, mean(fit$fit$pvalue[fit$fit$chisq_poorfit==1], na.rm = TRUE)))

#### ==Relative fit indices (do not vary much with sample size)========================
# Relative to null model, which usually is assuming no correlation between all variables
# Katie's search-stopping criteria is 2/4 have excellent fit (=< .05 for RMSEA and SRMR & >= .95 for NNFI and CFI)


# NNFI will be worse for models where variables are not highly correlated with one another
# NNFI aka TLI 
# Adjusts for df
nnfi <- fit$fit[, "nnfi"]
fit$fit[, "nnfi"][fit$fit[, "nnfi"]==1] <- NA 
nnfi[nnfi==1] <- NA 
nnfi.stats <- as.data.frame(describe(nnfi))
nnfi.mean <-nnfi.stats[3]
cat(sprintf("Mean (SD) NNFI for participants with >0 dof: %0.2f (%0.3f) \nMarginal fit: >.90 Excellent fit: >.95", nnfi.mean, nnfi.stats[4]))
nnfi[fit$fit[, "df"]==1] <- NA 
nnfi.stats <- as.data.frame(describe(nnfi))
nnfi.mean <-nnfi.stats[3]
cat(sprintf("Mean (SD) NNFI for participants with >1 dof (n=%s): %0.2f (%0.3f) \nMarginal fit: >.90 Excellent fit: >.95", nnfi.stats[2], nnfi.mean, nnfi.stats[4]))
fit$fit$nnfi_poorfit <- ifelse(fit$fit[, "nnfi"]<.95 | fit$fit[, "nnfi"]<0 | is.na(fit$fit[, "nnfi"])==TRUE, 1,0)
cat(sprintf("%d participant(s) had not excellent NNFI fit: %0.2f", sum(fit$fit$nnfi_poorfit, na.rm = TRUE), mean(fit$fit$nnfi[fit$fit$nnfi_poorfit==1], na.rm = TRUE)))
cat(sprintf("%s had max NNFI: %0.2f", fit$fit[which.max(fit$fit[,"nnfi"]),1], fit$fit[which.max(fit$fit[,"nnfi"]),"nnfi"]))


# CFI and NNFI are highly correlated
cfi <- fit$fit[, "cfi"]
cfi.stats <-as.data.frame(describe(cfi))
cfi.mean <- cfi.stats[3]
cat(sprintf("Mean (SD) CFI for participants: %0.2f (%0.3f) \nMarginal fit: >.90 Excellent fit: >.95", cfi.mean, cfi.stats[4]))
fit$fit$cfi_poorfit <- ifelse(cfi<.95  | is.na(cfi)==TRUE, 1,0)
cat(sprintf("%d participant(s) had not excellent CFI fit: %0.2f", sum(fit$fit$cfi_poorfit, na.rm=TRUE), mean(fit$fit$cfi[fit$fit$cfi_poorfit==1], na.rm=TRUE)))
cat(sprintf("%s had max CFI: %0.2f", fit$fit[which.max(fit$fit[,"cfi"]),1], fit$fit[which.max(fit$fit[,"cfi"]),"cfi"]))

#### Indices based on residuals matrix. Absolute fit indices
# Looks at discrepancy between observed and predicted covariances
# Are larger with smaller sample sizes (length of timeseries)

# RMSEA 
# Models with small dof and low N can have artificially large values of RMSEA; therefore this index may not be informative for low dof models
rmsea <- fit$fit[, "rmsea"]
fit$fit[, "rmsea"][fit$fit[, "rmsea"]==0] <- NA 
rmsea[rmsea==0] <- NA 
rmsea.stats <- as.data.frame(describe(rmsea))
rmsea.mean <-rmsea.stats[3]
# Excellent fit taken from Gates et al 2017 MBR and acceptable fit from Browne and Cudeck 1993 Testing Structural equation models.
cat(sprintf("Mean (SD) RMSEA for participants: %0.2f (%0.3f) \nAcceptable fit <.10, Excellent fit: <.05", rmsea.mean, rmsea.stats[4]))

#removing people with only 1 dof
rmsea[fit$fit[, "df"]==1] <- NA 
rmsea.stats <- as.data.frame(describe(rmsea))
rmsea.mean <-rmsea.stats[3]
cat(sprintf("Mean (SD) RMSEA for participants with >1 dof (n=%s): %0.2f (%0.3f) \nAcceptable fit <.10, Excellent fit: <.05", rmsea.stats[2], rmsea.mean, rmsea.stats[4]))

fit$fit$rmsea_poorfit <- ifelse(fit$fit$rmsea>.05 | is.na(fit$fit$rmsea)==TRUE, 1,0)
cat(sprintf("%d participant(s) had not excellent RMSEA fit: %0.2f", sum(fit$fit$rmsea_poorfit, na.rm = TRUE), mean(fit$fit$rmsea[fit$fit$rmsea_poorfit==1], na.rm = TRUE)))
cat(sprintf("%s had max RMSEA: %0.2f", fit$fit[which.min(fit$fit[,"rmsea"]),1], fit$fit[which.min(fit$fit[,"rmsea"]),"rmsea"]))


#SRMR. Standardized difference between the observed correlation and the predicted correlation
# Positive bias is greater for small N and low dof. No penalty for model complexity
srmr <- fit$fit[, "srmr"]
srmr.stats <- as.data.frame(describe(srmr))
srmr.mean <-srmr.stats[3]
# < or equal to .08 is from Hu and Bentler (1999)
cat(sprintf("Mean (SD) SRMR for participants: %0.2f (%0.3f) \nGood fit: =<.08 Excellent fit: <.05", srmr.mean, srmr.stats[4]))


fit$fit$srmr_poorfit <- ifelse(fit$fit[, "srmr"]>.05 | is.na(fit$fit$srmr)==TRUE, 1,0)
cat(sprintf("%d participant(s) had poor SRMR fit: %0.2f", sum(fit$fit$srmr_poorfit, na.rm=TRUE), mean(fit$fit$srmr[fit$fit$srmr_poorfit==1], na.rm=TRUE)))
cat(sprintf("%s had max SRMR: %0.2f", fit$fit[which.min(fit$fit[,"srmr"]),1], fit$fit[which.min(fit$fit[,"srmr"]),"srmr"]))


fit$fit$poorfit <- rowSums(cbind(fit$fit$srmr_poorfit, fit$fit$rmsea_poorfit, fit$fit$cfi_poorfit, fit$fit$nnfi_poorfit))
cat(sprintf("%d participant(s) had poor fit by 4 indices \n%d participant(s) had poor fit by 3 indices \n%d participant(s) had poor fit by 2 indices \n%d participant(s) had poor fit by 1 index \n%d participant(s) good fit by all indices except chisq", 
            sum(fit$fit$poorfit==4, na.rm = TRUE), sum(fit$fit$poorfit==3, na.rm = TRUE), sum(fit$fit$poorfit==2, na.rm = TRUE), sum(fit$fit$poorfit==1, na.rm = TRUE), sum(fit$fit$poorfit==0, na.rm = TRUE)))

cat(sprintf("%d participant(s) had poor fit by 4 indices \n%d participant(s) had poor fit by 3 indices \n%d participant(s) had excellent fit by at least 2 indices", 
            sum(fit$fit$poorfit==4, na.rm = TRUE), sum(fit$fit$poorfit==3, na.rm = TRUE), sum(fit$fit$poorfit<=2, na.rm = TRUE)))

write.csv(fit$fit, file="summaryFit_poorfit.csv", row.names = FALSE)


fit$fit <-read.csv("summaryFit_poorfit.csv")
# people who had poor fit (did not reach excellent fit for at least 2 indices)
poorfit1 <- fit$fit[which(fit$fit$poorfit>2),1]
poorfit1
length(poorfit1)
fit$fit[match(poorfit1, fit$fit$file),]

#people who had good fit by all 4 indices
goodfit <- fit$fit[which(fit$fit$poorfit<=2),1]
length(goodfit)
goodfit.data <-subset(fit$fit, fit$fit$file %in% goodfit)
goodfit.stats <- describe(goodfit.data[,c("cfi", "nnfi", "rmsea", "srmr")])
goodfit.stats <- as.data.frame(goodfit.stats)
goodfit.stats
#fit stats for those with good fit
cat(sprintf("CFI: M(SD)= %0.2f (%0.3f), NNFI: M(SD)= %0.2f (%0.3f), RMSEA: M(SD)= %0.2f (%0.3f), SRMR: M(SD)= %0.2f (%0.3f)", goodfit.stats[1,3], goodfit.stats[1,4], 
            goodfit.stats[2,3], goodfit.stats[2,4],
            goodfit.stats[3,3], goodfit.stats[3,4],
            goodfit.stats[4,3], goodfit.stats[4,4]))


#=======Cluster validity======================================================================

#Similarity matrix for walktrap subgrouping
sim_matrix <- fit$sim_matrix
#save(sim_matrix, file="sim_matrix_Runs34.Rdata")

## Testing stability of cluster solution
#install.packages("perturbR")
# re-installed on 9.27.18
library(perturbR)
dev.off()
stability <- perturbR(sim_matrix, reps=100) # this takes a long time
save(stability, file="perturbR.Rdata")

