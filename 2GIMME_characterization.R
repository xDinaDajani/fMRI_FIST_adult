# GIMME Analyses Flexible Item Selection Task (FIST)
rm(list=ls())
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
script_version <- 'v45*'
out <- Sys.glob(paste(path, '/Pegasus/1Output/8ROIs/', script_version, sep = ""))
setwd(out)

load("workspace.Rdata")
rm(list=setdiff(ls(), "fit"))

#Print names in list
#names(fit)
library(psych)
library(ggplot2)

#=======Summary fit stats=====================================================================
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
goodfit.stats <- as.data.frame(describe(goodfit.data[,c("cfi", "nnfi", "rmsea", "srmr")]))
goodfit.stats
#fit stats for those with good fit
cat(sprintf("n=%s, CFI: M(SD)= %0.2f (%0.3f), NNFI: M(SD)= %0.2f (%0.3f), RMSEA: M(SD)= %0.2f (%0.3f), SRMR: M(SD)= %0.2f (%0.3f)", goodfit.stats[1,2], goodfit.stats[1,3], goodfit.stats[1,4], 
            goodfit.stats[2,3], goodfit.stats[2,4],
            goodfit.stats[3,3], goodfit.stats[3,4],
            goodfit.stats[4,3], goodfit.stats[4,4]))

# Subgroup characterization ===================================================================
num_subgroups <- max(goodfit.data[,"sub_membership"])
sprintf("There are %i subgroups", num_subgroups)
for (subgroup in 1:num_subgroups) {
  n <- sum(fit$fit$sub_membership==subgroup)
  if (subgroup==1) {
    cat(sprintf("\n Total n=%i\n------------------------", nrow(fit$fit)))
  }
  cat(sprintf("\nSubgroup %i: n=%i", subgroup,n))
}
for (subgroup in 1:num_subgroups) {
  n <- sum(goodfit.data$sub_membership==subgroup)
  if (subgroup==1) {
    cat(sprintf("\n Good fit total n=%i\n------------------------", nrow(goodfit.data)))
  }
  cat(sprintf("\nSubgroup %i: n=%i", subgroup,n))
}

