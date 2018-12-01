# GIMME Analyses Flexible Item Selection Task (FIST)

#======INDICATE GIMME MODELS TO ANALYZE=======================================================
rm(list=ls())
script_version <- c('v39*', 'v43*') # identify folder names for runs1/2 and runs3/4

#======Read in data===========================================================================

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
  print(cat(paste("Variable names are not identical for Runs1/2 and Runs3/4...", "Please check your script_version variable", sep='\n')))
  }

## Attach Packages ===========================================================================
library(psych)
library(ggplot2)
library(igraph)
library(dplyr)
library(gdata)

#=======Summary fit stats=====================================================================

# people who had poor fit (did not reach excellent fit for at least 2 indices)
poorfit12 <- fit_Runs12$fit[which(fit_Runs12$fit$poorfit>2),1]
poorfit34 <- fit_Runs34$fit[which(fit_Runs34$fit$poorfit>2),1]
poorfit12
poorfit34

#people who had good fit by all 4 indices
goodfit12 <- fit_Runs12$fit[which(fit_Runs12$fit$poorfit<=2),1]
goodfit12.data <-subset(fit_Runs12$fit, fit_Runs12$fit$file %in% goodfit12)
goodfit34 <- fit_Runs34$fit[which(fit_Runs34$fit$poorfit<=2),1]
goodfit34.data <-subset(fit_Runs34$fit, fit_Runs34$fit$file %in% goodfit34)
length(goodfit12)
length(goodfit34)

goodfit.stats <- describe(goodfit12.data[,c("cfi", "nnfi", "rmsea", "srmr")])
goodfit.stats <- describe(goodfit34.data[,c("cfi", "nnfi", "rmsea", "srmr")])
goodfit.stats <- as.data.frame(goodfit.stats)
#fit stats for those with good fit
cat(sprintf("CFI: M(SD)= %0.2f (%0.3f), NNFI: M(SD)= %0.2f (%0.3f), RMSEA: M(SD)= %0.2f (%0.3f), SRMR: M(SD)= %0.2f (%0.3f)", goodfit.stats[1,3], goodfit.stats[1,4], 
            goodfit.stats[2,3], goodfit.stats[2,4],
            goodfit.stats[3,3], goodfit.stats[3,4],
            goodfit.stats[4,3], goodfit.stats[4,4]))

# For subjects that have good model fit at both timepoints
goodfit_bothtimepoints <- intersect(goodfit12,goodfit34)
length(goodfit_bothtimepoints)

#list of 1 #list[[1]][1] to index first participant in list
example <- fit_Runs12["path_est_mats"][[1]][1]
path_rownames <-dimnames(example[[1]])[[1]]
path_colnames <-dimnames(example[[1]])[[2]]
modulating_indx <- grep("by", path_rownames)
modulating_names <- path_rownames[modulating_indx]
roi_names <-path_rownames[-modulating_indx]
roi_names <- roi_names[1:(length(roi_names)-2)] #minus 2 to remove Flex and Control EVs
not_DVs <- c("Flexibility", "Control", modulating_names)

# Subgroup characterization ==================================================================
goodfit12.data.bothtp <-subset(fit_Runs12$fit, fit_Runs12$fit$file %in% goodfit12)
goodfit34.data.bothtp <-subset(fit_Runs34$fit, fit_Runs34$fit$file %in% goodfit34)
timepoint <- list(goodfit12.data.bothtp, goodfit34.data.bothtp)
for (i in 1:2) {
  cat(sprintf("\n----Timepoint %i----", i))
  num_subgroups <- max(timepoint[[i]][,"sub_membership"])
  #cat(sprintf("There are %i subgroups", num_subgroups))
  for (subgroup in 1:num_subgroups) {
    n <- sum(timepoint[[i]]$sub_membership==subgroup)
    cat(sprintf("\nSubgroup %i: n=%i", subgroup,n))
  }
}

## See which paths occur across both time points =============================================

for (outdir in 1:2) {
  path <- dirname(rstudioapi::getActiveDocumentContext()$path)
  out <- Sys.glob(paste(path, '/Pegasus/1Output/8ROIs/', script_version[outdir], sep = ""))
  setwd(out)
  if (outdir==1) {
    paths_Runs12<-read.xls("summaryPathCounts12.xlsx")
    pvalues.12 <-read.csv("indivPathEstimates.csv")
  } else {
    paths_Runs34<-read.xls("summaryPathCounts.xlsx")
    pvalues.34 <-read.csv("indivPathEstimates.csv")
  }
}

# Contemporaneous group- and subgroup-level paths that are stable across timepoints 1 and 2
level_string <- c("group-level", "subgroup-level")
paths <- list("Runs12_group"=NULL, "Runs34_group"=NULL, "Runs12_subgroup"=NULL, "Runs34_subgroup"=NULL)
timepoints <- list(paths_Runs12, paths_Runs34)
run_string <- c("Runs 1 and 2", "Runs 3 and 4")

for (level in 1:2) {
  # Level 1, group level
  # Level 2, subgroup level
  for (i in 1:2) {
    if (level==1) {
      count <- i-1
    } else {
      count <-i
    }
    paths[[level+count]] <- timepoints[[i]][timepoints[[i]]$group_1_subgroup_2_indiv_3==level,1:2]
    paths[[level+count]] <- paths[[level+count]][!grepl("[l][a][g]$", paths[[level+count]]$iv),]
    EVs <- c("Flexibility", "Control")
    paths.Flex <-paths[[level+count]][grepl(EVs[1], paths[[level+count]]$iv),]
    paths.Control <-paths[[level+count]][grepl(EVs[2], paths[[level+count]]$iv),]
    num_paths.EVs <- dim(paths.Flex)[1]+dim(paths.Control)[1]
    cat(sprintf("\n     %s   \n-----============-----\n%i %s intrinsic paths\n%i %s main effects of Flexibility trials\n%i %s main effects of Control trials", run_string[i], nrow(paths[[level+count]])-num_paths.EVs, level_string[level], dim(paths.Flex)[1], level_string[level], dim(paths.Control)[1], level_string[level]))
  }
  count <- level-1
  common_paths.group <- inner_join(paths[[level+count]], paths[[level+count+1]], by=c("dv", "iv"))
  common_paths.group.intrinsic <-common_paths.group[!grepl(paste(EVs, collapse="|"), common_paths.group$iv),]
  common_paths.group.Flex <-common_paths.group[grepl(EVs[1], common_paths.group$iv),]
  common_paths.group.Control <-common_paths.group[grepl(EVs[2], common_paths.group$iv),]
  num_common_paths.group.EVs <- dim(common_paths.group.Flex)[1]+dim(common_paths.group.Control)[1]
  cat(sprintf("\n\n Common %s paths   \n-----============-----\n%i common %s intrinsic paths\n%i common %s main effects of Flexibility trials\n%i common %s main effects of Control trials", level_string[level], dim(common_paths.group)[1]-num_common_paths.group.EVs, level_string[level], dim(common_paths.group.Flex)[1], level_string[level], dim(common_paths.group.Control)[1], level_string[level]))
  cat(sprintf("\nCommon %s instrinsic paths\n-----============-----\n", level_string[level]))
  print(common_paths.group.intrinsic)
  if (dim(common_paths.group.Flex)[1]!=0) {
    cat(sprintf("\nCommon Flex paths\n-----============-----\n", level_string[level]))
    print(common_paths.group.Flex)
  }
  if (dim(common_paths.group.Control)[1]!=0) {
    cat(sprintf("\nCommon Control paths\n-----============-----\n",level_string[level]))
    print(common_paths.group.Control)
  }
  
  unique.12 <-anti_join(paths[[level+count]], paths[[level+count+1]], by=c("dv", "iv"))
  unique.34 <-anti_join(paths[[level+count+1]], paths[[level+count]], by=c("dv", "iv"))
  if (dim(unique.12)[1]!=0) {
    cat(sprintf("\n%s paths unique to Runs1/2\n-----============-----\n", level_string[level]))
    print(unique.12)
  }
  if (dim(unique.34)[1]!=0) {
    cat(sprintf("\n%s paths unique to Runs3/4\n-----============-----\n",level_string[level]))
    print(unique.34)
  }
}

# Interaction effects
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
out <- Sys.glob(paste(path, '/Pegasus/1Output/8ROIs/', script_version[1], sep = ""))
setwd(out)
sum_matrix12 <- read.csv(file="summaryPathCountsMatrix_goodfit_n26.csv", row.names=1)
interaction.12 <- sum_matrix12 %>% dplyr:: select(matches("^LIFJby*"))

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
out <- Sys.glob(paste(path, '/Pegasus/1Output/8ROIs/', script_version[2], sep = ""))
setwd(out)
sum_matrix34 <- read.csv(file="summaryPathCountsMatrix_goodfit_n26_Runs34.csv", row.names=1)
interaction.34 <- sum_matrix34 %>% dplyr:: select(matches("^LIFJby*"))

# Characterize group-level paths for each timepoint===========================================

# Group level paths for first timepoint is paths[[1]]
path_estimates_tp1 <-data.frame(matrix(nrow=length(goodfit12), 
                                       ncol=(nrow(paths[[1]])+1)))
path_estimates_tp2 <-data.frame(matrix(nrow=length(goodfit34), 
                                       ncol=(nrow(paths[[2]])+1)))
path_estimates_tp1.sub <-data.frame(matrix(nrow=length(goodfit12), 
                                           ncol=(nrow(paths[[3]])+1)))
path_estimates_tp2.sub <-data.frame(matrix(nrow=length(goodfit34), 
                                           ncol=(nrow(paths[[4]])+1)))
path_estimates <- list(path_estimates_tp1, path_estimates_tp1.sub, path_estimates_tp2, 
                       path_estimates_tp2.sub)
colnames(path_estimates[[1]])[1] <- "PID"
colnames(path_estimates[[2]])[1] <- "PID"
colnames(path_estimates[[3]])[1] <- "PID"
colnames(path_estimates[[4]])[1] <- "PID"
paths <- paths[c("Runs12_group", "Runs12_subgroup", "Runs34_group", "Runs34_subgroup")]
pvalues <- list(pvalues.12, pvalues.34)
path_pvalues <-path_estimates
names(path_estimates) <- names(paths)[1:4]
names(path_pvalues) <- names(paths)[1:4]
datasets <- list(fit_Runs12, fit_Runs34)
subjs <- list(goodfit12, goodfit34)

for (timepoint in 1:2) {
  # Timepoint 1 is Runs1/2
  # Timepoint 2 is Runs3/4
  for (subj in 1:nrow(path_estimates[[timepoint]])) {
    subj_num <- subjs[[timepoint]][subj]
    subj_est <- datasets[[timepoint]]["path_est_mats"][[1]][subj_num]
    subj_p <-pvalues[[timepoint]][pvalues[[timepoint]]$file==subj_num,]
    if (is.na(subj_est)) {
      next
    }
    print(names(subj_est))
    # grab both group-level and subgroup-level paths
    for (j in 1:2) {
      # Level 1 is group (j=1)
      # Level 2 is subgroup (j=2)
      if (timepoint==1) {
        element <- j + (timepoint-1)
      } else {
        element <- j + timepoint
      }
      # insert beta estimate for each of the group level paths 
      for (path_est in 1:nrow(paths[[element]])) {
        # find row and column indices for each group-level path
        row <- which(rownames(subj_est[[1]])==paths[[element]][path_est,1]) #row is DV
        col <-which(colnames(subj_est[[1]])==paths[[element]][path_est,2])
        path_estimates[[element]][subj,path_est+1]<-subj_est[[1]][row,col]
        path_estimates[[element]][subj,1] <- names(subj_est)
        
        #find row in subj_p that has correct dv AND iv
        tmp <- subj_p[as.character(subj_p$dv)==as.character(paths[[element]][path_est,1]) & 
                 as.character(subj_p$iv)==as.character(paths[[element]][path_est,2]),]
        if (dim(tmp)[1]!=0) {
          path_pvalues[[element]][subj,path_est+1] <- tmp$pval
        }
        path_pvalues[[element]][subj,1] <- as.character(subj_p$file)[1]
        if (subj ==1) {
          # naming column
          colnames(path_estimates[[element]])[path_est+1] <-paste0(paths[[element]][path_est,2], "->", paths[[element]][path_est,1])
          colnames(path_pvalues[[element]])[path_est+1] <-paste0(paths[[element]][path_est,2], "->", paths[[element]][path_est,1])
        }
      }
    }
  }
}

 # Descriptives for group-level paths
for (timepoint in c(1,3)){
  if (timepoint==3) {
    string<-2
  } else {
    string<-timepoint
    cat("\n---------Group-level paths-----------")
  }
  cat(sprintf("\n-----%s------", run_string[string]))
  for (col in 2:ncol(path_estimates[[timepoint]])) {
    sum(path_pvalues[[timepoint]][,col]<.05)
    sig.p <- path_pvalues[[timepoint]][which(path_pvalues[[timepoint]][,col]<.05),c(1,col)]
    path_est_sig <- path_estimates[[timepoint]][path_estimates[[timepoint]][,1]  %in% sig.p[,1],c(1,col)]
    stats.sig <-as.data.frame(describe(path_est_sig[,2]))
    cat(sprintf("\n%s: n=%s, M(SD)=%0.2f(%0.2f), range=[%0.2f,%0.2f]", colnames(path_estimates[[timepoint]])[col], stats.sig[2], stats.sig[3], stats.sig[4], stats.sig[8], stats.sig[9]))
    hist(path_estimates[[timepoint]][,col], main=colnames(path_estimates[[timepoint]])[col])
  }
}

# Group level path Flex --> lIFJ Runs 1/2
path_estimates[[1]][,c(1,11)]
# LUDD_FST_00052 had negative estimate

# Runs 1/2 Subgroup-level paths
# grab stats for participants who had the path estimated AND path was significant
  cat("\n---------Subgroup-level paths-----------")
  cat(sprintf("\n-----%s------", run_string[1]))
  row_num <-which(colnames(path_estimates[[2]])=="Flexibility->visual")
  vector <- path_estimates[[2]][,row_num]
  path_index <-which(colnames(path_pvalues[[2]])=="Flexibility->visual")
  sum(path_pvalues[[2]][,path_index]<.05, na.rm=TRUE)
  sig.p <- path_pvalues[[2]][which(path_pvalues[[2]][,path_index]<.05),c(1,path_index)]
  col_name <-which(colnames(path_estimates[[2]])=="Flexibility->visual")
  path_est_sig <- path_estimates[[2]][path_estimates[[2]][,1]  %in% sig.p[,1],col_name]
  stats.sig <-as.data.frame(describe(path_est_sig))
  cat(sprintf("\nMain effect of %s: n=%s, M(SD)=%0.2f(%0.2f), range=[%0.2f,%0.2f]", "Flexibility->visual", stats.sig[2], stats.sig[3], stats.sig[4], stats.sig[8], stats.sig[9]))
  
  row_num <-which(colnames(path_estimates[[2]])=="Flexibility->Langulargyrus")
  vector <- path_estimates[[2]][,row_num]
  path_index <-which(colnames(path_pvalues[[2]])=="Flexibility->Langulargyrus")
  sum(path_pvalues[[2]][,path_index]<.05, na.rm=TRUE)
  sig.p <- path_pvalues[[2]][which(path_pvalues[[2]][,path_index]<.05),c(1,path_index)]
  col_name <-which(colnames(path_estimates[[2]])=="Flexibility->Langulargyrus")
  path_est_sig <- path_estimates[[2]][path_estimates[[2]][,1]  %in% sig.p[,1],col_name]
  stats.sig <-as.data.frame(describe(path_est_sig))
  cat(sprintf("\nMain effect of %s: n=%s, M(SD)=%0.2f(%0.2f), range=[%0.2f,%0.2f]", "Flexibility->Langulargyrus", stats.sig[2], stats.sig[3], stats.sig[4], stats.sig[8], stats.sig[9]))
  hist(path_est_sig)
  
# Runs 3/4
  row_num <-which(colnames(path_estimates[[4]])=="Flexibility->LIFJ")
  vector <- path_estimates[[4]][,row_num]
  path_index <-which(colnames(path_pvalues[[4]])=="Flexibility->LIFJ")
  col_name <-which(colnames(path_estimates[[4]])=="Flexibility->LIFJ")
  sum(path_pvalues[[4]][,path_index]<.05, na.rm=TRUE)
  sig.p <- path_pvalues[[4]][which(path_pvalues[[4]][,path_index]<.05),c(1,path_index)]
  path_est_sig <- path_estimates[[4]][path_estimates[[4]][,1]  %in% sig.p[,1],col_name]
  stats.sig <-as.data.frame(describe(path_est_sig))
  cat(sprintf("\nMain effect of %s: n=%s, M(SD)=%0.2f(%0.2f), range=[%0.2f,%0.2f]", "Flexibility->LIFJ", stats.sig[2], stats.sig[3], stats.sig[4], stats.sig[8], stats.sig[9]))
  hist(path_est_sig)
  
  
# Characterize interaction paths =============================================================
  
  #=Read in list of features I am using=
  # These are all paths for participants with good fit (n=28 for both timepoints.)
  # object was created with script GIMME_allpaths_betas
  out <- Sys.glob(paste(path, '/Pegasus/1Output/8ROIs/', script_version[2], sep = ""))
  load(paste0(out, '/allpaths.RData'))
  cat(sprintf("----Runs 1and2----\nThere are %s subjects\nThere are %s features\n----Runs 3and4----\nThere are %s subjects\nThere are %s features", nrow(allpaths.list[[1]]), length(allpaths.list[[1]])-1, nrow(allpaths.list[[2]]), length(allpaths.list[[2]])-1))
  
  # Subset with interaction paths only
  int.paths.list <- lapply(allpaths.list, function(x) {
    select(x, matches("^LIFJby*|PID"))
  })
  ## Exclude any features that are 0 for all participants
  for (i in 1:2) {
    int.paths.list[[i]] <- int.paths.list[[i]][,!(sapply(int.paths.list[[i]], function(x)all(x==0)))]
    print(ncol(int.paths.list[[i]]))
  }
  int.path.pvalues <- int.paths.list
  int.path.pvalues[[1]][,] <- NA
  int.path.pvalues[[2]][,] <- NA
  ## Grab p-values for these interaction paths
  for (timepoint in 1:2) {    # Timepoint 1 is Runs1/2   # Timepoint 2 is Runs3/4
    for (subj in 1:nrow(int.paths.list[[timepoint]])) {
      subj_num <- subjs[[timepoint]][subj]
      subj_p <-pvalues[[timepoint]][pvalues[[timepoint]]$file==subj_num,]
      print(as.character(subj_p[subj, "file"]))
        # insert beta estimate for each of the interaction paths
        for (path_est in 1:ncol(int.paths.list[[timepoint]])-1) {
          # Split column name into IV and DV
          IV_DV <-strsplit(colnames(int.paths.list[[timepoint]])[path_est+1], "_")
          IV <- IV_DV[[1]][1]
          DV <- IV_DV[[1]][2] 
          #find row in subj_p that has correct dv AND iv
          tmp <- subj_p[as.character(subj_p$dv)==DV & 
                          as.character(subj_p$iv)==IV,]
          int.path.pvalues[[timepoint]][subj,1] <- as.character(subj_p[subj, "file"])
          if (dim(tmp)[1]!=0) {
            int.path.pvalues[[timepoint]][subj,path_est+1] <- tmp$pval
          }
        }
      }
    }

  for (i in 1:2) {
    cat(sprintf("--------%s---------\n", run_string[i]))
    for (x in 2:ncol(int.paths.list[[i]])) {
      if (!colnames(int.path.pvalues[[i]])[x]==colnames(int.paths.list[[i]])[x]) {
        print("Warning! Column names do not match")
      }
      print(plot <- ggplot(int.paths.list[[i]], aes(int.paths.list[[i]][,x])) + 
              geom_histogram(bins=30) +
              xlab(colnames(int.paths.list[[i]])[x]) +
              theme_classic())
      print(colnames(int.paths.list[[i]])[x])
      #print(describe(int.paths.list[[i]][,x]))
      print(sprintf("%d participants with estimated path", (sum(int.paths.list[[i]][,x]!=0))))
      print(sprintf("%d participants with SIG. estimated path", sum(int.path.pvalues[[i]][,x]<.05, na.rm=TRUE)))
      sig.p <- int.paths.list[[i]][which(int.path.pvalues[[i]][,x]<.05),c(1,x)]
      print(sig.p[1])
      if (!(dim(sig.p)[1]==0)) {
        path_est_sig <- sig.p[,2]
        stats.sig <-as.data.frame(describe(path_est_sig))
        cat(sprintf("Main effect of %s: n=%s, M(SD)=%0.2f(%0.2f), range=[%0.2f,%0.2f]", colnames(int.paths.list[[i]])[x], stats.sig[2], stats.sig[3], stats.sig[4], stats.sig[8], stats.sig[9]))
        hist(path_est_sig)
        dev.off()
      }
      readline(prompt="Press [enter] to continue")
    }
  }  

# Investigate lIFJ*Flex-->visual path more
  for (i in 1:2) {
    cat(sprintf("--------%s---------\n", run_string[i]))
    #col_index <- which(colnames(int.paths.list[[i]])=="LIFJbyFlexibility_visual")
    col_index <- which(colnames(int.path.pvalues[[i]])=="LIFJbyFlexibility_visual")
    sig.p <- int.paths.list[[i]][which(int.path.pvalues[[i]][,col_index]<.05),c(1,col_index)]
    print(sig.p[1])
    #Flex--->visual?
    col_index <- which(colnames(allpaths.list[[i]])=="Flexibility_visual")
    beta <- allpaths.list[[i]][allpaths.list[[i]][,1] %in% sig.p[,1],c(1,col_index)]
    if (i==1) {
      p_runs12 <- path_pvalues[[2]][path_pvalues[[2]][,1] %in% sig.p[,1],c(1, which(colnames(path_pvalues[[2]])=="Flexibility->visual"))]
      print(cbind(beta, p_runs12))
    } 
    if (i==2) {
      print(beta)
    }
    #IFJ-->visual?
    col_index <- which(colnames(allpaths.list[[i]])=="LIFJ_visual")
    beta <- allpaths.list[[i]][allpaths.list[[i]][,1] %in% sig.p[,1],c(1,col_index)]
    print(beta)
  }
  
  # Compare task performance for subjects with lIFJ*Flex-->visual estimated versus subjects without path estimated FOR TIME POINT 1
  # Behavioral data
  path <- dirname(rstudioapi::getActiveDocumentContext()$path)
  path.behavioral_data <- paste(dirname(path), 'Data', 'eprime_task_data', sep='/')
  acc.dat <- read.csv(paste0(path.behavioral_data, "/Acc_data_fMRI_task_newDV_final4.23.18.csv"), header = T) 
  # accRT DV for runs 1-4 are columns 14:17
  acc.dat.accRT <- acc.dat[,c(1, 14:17)]
  acc.dat.accRT_avg.by.timepoint <- data.frame(matrix(NA, ncol = 3, nrow = nrow(acc.dat.accRT)))
  colnames(acc.dat.accRT_avg.by.timepoint) <- c(colnames(acc.dat)[1], "AccRT_Runs12","AccRT_Runs34")
  acc.dat.accRT_avg.by.timepoint[,1] <- acc.dat.accRT[,1]
  #Averages values for Runs 1 and 2
  acc.dat.accRT_avg.by.timepoint[,2] <-rowMeans(acc.dat.accRT[,2:3])
  acc.dat.accRT_avg.by.timepoint[,3] <-rowMeans(acc.dat.accRT[,4:5])
  hist(acc.dat.accRT_avg.by.timepoint[,2])
  hist(acc.dat.accRT_avg.by.timepoint[,3])
  describe(acc.dat.accRT_avg.by.timepoint[,2:3])
  accRT.goodfit12<-subset(acc.dat.accRT_avg.by.timepoint[,1:3], sapply(strsplit(as.character(acc.dat.accRT_avg.by.timepoint$PID), "_"), "[[", 1) %in% sapply(strsplit(as.character(goodfit12), "_"), "[[", 3))
  nrow(accRT.goodfit12)
  accRT.goodfit34<-subset(acc.dat.accRT_avg.by.timepoint[,1:3], sapply(strsplit(as.character(acc.dat.accRT_avg.by.timepoint$PID), "_"), "[[", 1) %in% sapply(strsplit(as.character(goodfit34), "_"), "[[", 3))
  # participants with path estimated
  col_index <- which(colnames(int.path.pvalues[[1]])=="LIFJbyFlexibility_visual")
  sig.p <- int.paths.list[[i]][which(int.path.pvalues[[i]][,col_index]<.05),c(1,col_index)]
  accRT.goodfit12$Mod.path.estimated <- ifelse(sapply(strsplit(as.character(accRT.goodfit12$PID), "_"), "[[", 1) %in% sapply(strsplit(sig.p[,1], "_"), "[[", 3), 1, 0)
  
  #stats
  stats1<- describeBy(accRT.goodfit12[,2], accRT.goodfit12$Mod.path.estimated)
  stats1
  # t.test
  ttest <- t.test(accRT.goodfit12[accRT.goodfit12$Mod.path.estimated==0,2], accRT.goodfit12[accRT.goodfit12$Mod.path.estimated==1,2])
  cat(sprintf("Difference in performance by estimationg of lIFJ*Flex->visual path:\nPath not estimated: n=%s, M(SD)=%0.2f(%0.2f), range=[%0.2f,%0.2f]; Path estimated: n=%s, M(SD)=%0.2f(%0.2f), range=[%0.2f,%0.2f], t(%0.2f)=%0.2f, p=%0.3f",  stats1[[1]][2], stats1[[1]][3], stats1[[1]][4], stats1[[1]][8], stats1[[1]][9], stats1[[2]][2], stats1[[2]][3], stats1[[2]][4], stats1[[2]][8], stats1[[2]][9], ttest$parameter, ttest$statistic, ttest$p.value))
  
#=======================================dACC-->lIPL====================================================
# Compare performance for participants with and without Time point 2 path dACC-->lIPL
  # participants with path estimated
  col_index <- which(colnames(allpaths.list[[2]])=="dACC_Linferiorparietallobule")
  beta <- allpaths.list[[2]][which(allpaths.list[[2]][,col_index]!=0),c(1,col_index)]
  accRT.goodfit34$Mod.path.estimated <- ifelse(sapply(strsplit(as.character(accRT.goodfit34$PID), "_"), "[[", 1) %in% sapply(strsplit(beta[,1], "_"), "[[", 3), 1, 0)
  
  #stats
  stats1<- describeBy(accRT.goodfit34[,3], accRT.goodfit34$Mod.path.estimated)
  stats1
  # t.test
  ttest <- t.test(accRT.goodfit34[accRT.goodfit34$Mod.path.estimated==0,3], accRT.goodfit34[accRT.goodfit34$Mod.path.estimated==1,3])
  cat(sprintf("Difference in performance by estimationg of lIFJ*Flex->visual path:\nPath not estimated: n=%s, M(SD)=%0.2f(%0.2f), range=[%0.2f,%0.2f]; Path estimated: n=%s, M(SD)=%0.2f(%0.2f), range=[%0.2f,%0.2f], t(%0.2f)=%0.2f, p=%0.3f",  stats1[[1]][2], stats1[[1]][3], stats1[[1]][4], stats1[[1]][8], stats1[[1]][9], stats1[[2]][2], stats1[[2]][3], stats1[[2]][4], stats1[[2]][8], stats1[[2]][9], ttest$parameter, ttest$statistic, ttest$p.value))
  
# == Compare number of paths between timepoints ==============================================

  describe(allpaths.list[[1]]$Num_paths)
  hist(allpaths.list[[1]]$Num_paths)
  describe(allpaths.list[[2]]$Num_paths)
  hist(allpaths.list[[2]]$Num_paths)
  subset1_n26 <-allpaths.list[[1]][allpaths.list[[1]][,1] %in% goodfit_bothtimepoints,]
  subset2_n26 <-allpaths.list[[2]][allpaths.list[[2]][,1] %in% goodfit_bothtimepoints,]
  #subset1_n26$PID==subset2_n26$PID
  stats1<- describe(subset1_n26$Num_paths)
  stats2<-describe(subset2_n26$Num_paths)
  ttest <- t.test(subset1_n26$Num_paths, subset2_n26$Num_paths, paired=TRUE)
  cat(sprintf("Change across time in number of paths: n=%s, timepoint 1: M(SD)=%0.2f(%0.2f), range=[%s,%s], timepoint 2: M(SD)=%0.2f(%0.2f), range=[%s,%s], t(%s)=%0.2f, p=%0.3f", stats1[2], stats1[3], stats1[4], stats1[8], stats1[9], stats2[3], stats2[4], stats2[8], stats2[9], ttest$parameter, ttest$statistic, ttest$p.value))
