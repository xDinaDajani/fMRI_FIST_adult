# GIMME Analyses Flexible Item Selection Task (FIST)

#======INDICATE GIMME MODELS TO ANALYZE=======================================================
rm(list=ls())
script_version <- c('v39*', 'v43*') # identify folder names for runs1/2 and runs3/4

#======Read in data===========================================================================
# GIMME data
for (outdir in 1:2) {
  path <- dirname(rstudioapi::getActiveDocumentContext()$path)
  out <- Sys.glob(paste(path, '/Pegasus/1Output/8ROIs/', script_version[outdir], sep = ""))
  setwd(out)
  load("workspace.Rdata")
  if (outdir==1) {
    fit_Runs12 <- fit
    fit_Runs12$fit<-read.csv("summaryFit_poorfit.csv")
  } else {
    fit_Runs34 <- fit
    fit_Runs34$fit<-read.csv("summaryFit_poorfit.csv")
  }
  rm(fit)
}
rm(list=setdiff(ls(), c("fit_Runs12", "fit_Runs34", "script_version")))
if (all.equal(fit_Runs12$varnames,fit_Runs34$varnames)==TRUE) {
} else {
  cat(paste("Variable names are not identical for Runs1/2 and Runs3/4...", "Please check your script_version variable", sep='\n'))
}

## Attach Packages ===========================================================================
library(psych)
library(ggplot2)
library(igraph)
library(dplyr)
library(gdata)
library(glmnet)
library(crayon)

#=======Summary fit stats=====================================================================
# people who had poor fit (did not reach excellent fit for at least 2 indices)
poorfit12 <- fit_Runs12$fit[which(fit_Runs12$fit$poorfit>2),1]
poorfit34 <- fit_Runs34$fit[which(fit_Runs34$fit$poorfit>2),1]

#people who had good fit by all 4 indices
goodfit12 <- fit_Runs12$fit[which(fit_Runs12$fit$poorfit<=2),1]
goodfit12.data <-subset(fit_Runs12$fit, fit_Runs12$fit$file %in% goodfit12)
goodfit34 <- fit_Runs34$fit[which(fit_Runs34$fit$poorfit<=2),1]
goodfit34.data <-subset(fit_Runs34$fit, fit_Runs34$fit$file %in% goodfit34)
length(goodfit12)
length(goodfit34)

# For subjects that have good model fit at both timepoints
goodfit_bothtimepoints <- intersect(goodfit12,goodfit34)
length(goodfit_bothtimepoints)

## Build dataframe with all contemporaneous path estimates ===================================
# Include number of contemporaneous paths ====================================================

# Grab names of ROIs, Task EVs, and interaction (modulating) effects
example <- fit_Runs12["path_est_mats"][[1]][1]
path_rownames <-dimnames(example[[1]])[[1]]
path_colnames <-dimnames(example[[1]])[[2]]
modulating_indx <- grep("by", path_rownames)
modulating_names <- path_rownames[modulating_indx]
roi_names <-path_rownames[-modulating_indx]
roi_names <- roi_names[1:(length(roi_names)-2)] #minus 2 to remove Flex and Control EVs
not_DVs <- c("Flexibility", "Control", modulating_names)

rm("allpaths.list")
for (runs in 1:2) {
  if (runs==1) {
    subjs <- goodfit12
    fit <-fit_Runs12
  } else {
    subjs <- goodfit34
    fit <-fit_Runs34
  }
  for (subj_indx in 1:length(subjs)) {
    subj <- subjs[subj_indx]
    print(sprintf("Timepoint: %s     Subj: %s", runs, subj))
    subj.df <-as.data.frame(fit$path_est_mats[[subj]])
    subj.df <- subj.df %>% dplyr:: select(-matches("^.*[l][a][g]$"))
    # Remove rows with Flexibility, Control, and interaction effects as DVs
    subj.df <- subj.df[! row.names(subj.df) %in% not_DVs,]
    # Initialize empty dataframe with PID, 88 columns, plus num_paths variable
    if (runs==1 & subj_indx==1) {
      paths.df <-data.frame(matrix(nrow=length(subjs), ncol=(dim(subj.df)[1]*dim(subj.df)[2]+2)))
      colnames(paths.df)[1:2] <- c("PID", "Num_paths")
      allpaths.list <- list("Runs12"=paths.df, "Runs34"=NULL)
    }
    if (runs==2 & subj_indx==1) {
      paths.df <-data.frame(matrix(nrow=length(subjs), ncol=(dim(subj.df)[1]*dim(subj.df)[2]+2)))
      colnames(paths.df)[1:2] <- c("PID", "Num_paths")
      allpaths.list[[2]] <- paths.df
    }
    allpaths.list[[runs]][subj_indx, 1] <- as.character(subj)
    # Number of contemporaneous paths
    allpaths.list[[runs]][subj_indx, 2] <- sum(subj.df!=0)
    # Beta estimates for each path
    beta <- 1
    for (row in 1:nrow(subj.df)) {
      for (col in 1:ncol(subj.df)) {
        # Insert column name. Name is IV_DV. x-->y.
        if (subj_indx==1) {
          colnames(allpaths.list[[runs]])[beta+2] <- paste0(colnames(subj.df)[col], "_", rownames(subj.df)[row])
        }
        # Insert beta estimate for particular path
        allpaths.list[[runs]][subj_indx, beta+2] <- subj.df[row, col]
        beta <- beta +1
      }
    }
  }
}

cat(sprintf("----Runs 1and2----\nThere are %s subjects\nThere are %s features\n----Runs 3and4----\nThere are %s subjects\nThere are %s features", nrow(allpaths.list[[1]]), length(allpaths.list[[1]])-1, nrow(allpaths.list[[2]]), length(allpaths.list[[2]])-1))

save(file= "allpaths.RData", allpaths.list)

# HISTOGRAMS ===========================================================================
# Check normality of path features and do not use variables that are non-normal (to avoid 0-inflated variables)
# create list and dataframe to identify which features are normal

## Exclude any features that are 0 for all participants
for (i in 1:2) {
  allpaths.list[[i]] <- allpaths.list[[i]][,!(sapply(allpaths.list[[i]], function(x)all(x==0)))]
  #print(ncol(allpaths.list[[i]]))
}


options(warn=-1)
for (runs in 1:2) { 
  for (i in 1:(length(allpaths.list[[runs]])-1)) {
    if (runs==1 & i==1) {
      normal.df <- data.frame(matrix(nrow=length(allpaths.list[[runs]])-1, ncol=2))
      colnames(normal.df)[1:2] <- c("Variable", "Normal_1")
      normal.df[,2] <- 1
      normal.list <- list("Runs12"=normal.df, "Runs34"=NULL)
    } 
    if (runs==2 & i==1) {
      normal.df <- data.frame(matrix(nrow=length(allpaths.list[[runs]])-1, ncol=2))
      colnames(normal.df)[1:2] <- c("Variable", "Normal_1")
      normal.df[,2] <- 1
      normal.list[[2]] <- normal.df
    } 
    #print(sprintf("%s", colnames(allpaths.list[[runs]])[i+1]))
    #  print(
    #   plot <- ggplot(allpaths.list[[runs]], aes(allpaths.list[[runs]][,i+1])) + 
    #           geom_histogram() +
    #           xlab(colnames(allpaths.list[[runs]])[i+1]) +
    #           theme_classic())
      stats<-describe(allpaths.list[[runs]][i+1])
      normal.list[[runs]][i,1] <- colnames(allpaths.list[[runs]])[i+1]
      if (abs(stats[11])>=3 |  abs(stats[12])>=3) {
       #cat(red("-------Non-normal data-------\n"))
       normal.list[[runs]][i,2] <- 0
      } 
      #print(stats)
      #cat(sprintf("Number of 0's = %s", sum(allpaths.list[[runs]][i+1]==0)))
    #readline(prompt="Press [enter] to continue")
    #dev.off()
  }
} 

## Exclude any features that are non-normal
for (runs in 1:2) {
  # grab features that are normal
  normal.only <- normal.list[[runs]][normal.list[[runs]][,2]==1,1]
  print(length(normal.only))
  normal.only[length(normal.only) +1] <- "PID"
  normal.paths[[runs]] <- allpaths.list[[runs]][,
          names(allpaths.list[[runs]])
          %in% 
          normal.only 
  ]
  #names(normal.paths)
}

cat(sprintf("----Runs 1and2----\nThere are %s subjects\nThere are %s features\n----Runs 3and4----\nThere are %s subjects\nThere are %s features", nrow(normal.paths[[1]]), length(normal.paths[[1]])-1, nrow(normal.paths[[2]]), length(normal.paths[[2]])-1))

save(file= "normalpaths.RData", normal.paths)


