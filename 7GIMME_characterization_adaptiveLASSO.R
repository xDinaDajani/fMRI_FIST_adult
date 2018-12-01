# GIMME Analyses Flexible Item Selection Task (FIST)

#======INDICATE GIMME MODELS TO ANALYZE=======================================================
rm(list=ls())
script_version <- c('v39*', 'v43*') # identify folder names for runs1/2 and runs3/4
library(gdata)

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
    paths_Runs12<-read.xls("summaryPathCounts12.xlsx")
    pvalues.12 <-read.csv("indivPathEstimates.csv")
  } else {
    fit_Runs34 <- fit
    fit_Runs34$fit<-read.csv("summaryFit_poorfit.csv")
    paths_Runs34<-read.xls("summaryPathCounts.xlsx")
    pvalues.34 <-read.csv("indivPathEstimates.csv")
  }
  rm(fit)
}

rm(list=setdiff(ls(), c("fit_Runs12", "fit_Runs34", "paths_Runs12", "paths_Runs34",
                        "pvalues.12", "pvalues.34",
                        "script_version")))
timepoints <- list(paths_Runs12, paths_Runs34)
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
packageVersion("glmnet")

#======Read in data===========================================================================
# Behavioral data
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
path.behavioral_data <- paste(dirname(path), 'Data', 'eprime_task_data', sep='/')
acc.dat <- read.csv(paste0(path.behavioral_data, "/Acc_data_fMRI_task_newDV_final4.23.18.csv"), header = T) 
# accuracy DV are columns 2:5
acc.dat.accRT <- acc.dat[,c(1, 2:5)]
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

# Behavioral data for participants with good fit
acc.dat.accRT_avg.by.timepoint
accRT.goodfit12<-subset(acc.dat.accRT_avg.by.timepoint[,1:2], sapply(strsplit(as.character(acc.dat.accRT_avg.by.timepoint$PID), "_"), "[[", 1) %in% sapply(strsplit(as.character(goodfit12), "_"), "[[", 3))
nrow(accRT.goodfit12)
accRT.goodfit34<-subset(acc.dat.accRT_avg.by.timepoint[,c(1,3)], sapply(strsplit(as.character(acc.dat.accRT_avg.by.timepoint$PID), "_"), "[[", 1) %in% sapply(strsplit(as.character(goodfit34), "_"), "[[", 3))
nrow(accRT.goodfit34)
describe(accRT.goodfit12[,2])
describe(accRT.goodfit34[,2])


#====Read in list of features I am using======================================================
# These are all paths that were normally distributed (abs(skew) and abs(kurtosis) <3)
out <- Sys.glob(paste(path, '/Pegasus/1Output/8ROIs/', script_version[2], sep = ""))
load(paste0(out, '/normalpaths.RData'))
cat(sprintf("----Runs 1and2----\nThere are %s subjects\nThere are %s features\n----Runs 3and4----\nThere are %s subjects\nThere are %s features", nrow(normal.paths[[1]]), length(normal.paths[[1]])-1, nrow(normal.paths[[2]]), length(normal.paths[[2]])-1))

### ====Adaptive LASSO =======================================================================
# Following example: https://rpubs.com/kaz_yos/alasso

# Runs 1 and 2
    x <- as.matrix(normal.paths[[1]][-1]) # Removes ID var
    colnames(x)
    y <- as.matrix(accRT.goodfit12[,2]) # grabs accRT DV
    
    ## Ridge Regression to create the Adaptive Weights Vector
    set.seed(999)
    cv.ridge12 <- cv.glmnet(x, y, family='gaussian', 
                          ## ‘alpha = 1’ is the lasso penalty, 
                          ## and ‘alpha = 0’ the ridge penalty.
                          alpha=0, standardize=TRUE, 
                          ## type.measure: loss to use for cross-validation.
                          type.measure = "mse",
                          ## K = 10 is the default.
                          nfold = 10)
    plot(cv.ridge12)
    ## Extract coefficients at the error-minimizing lambda
    cv.ridge12$lambda.min
    ## s: Value(s) of the penalty parameter ‘lambda’ at which
    ##    predictions are required. Default is the entire sequence used
    ##    to create the model.
    coef(cv.ridge12, s = cv.ridge12$lambda.min)
    # The intercept estimate should be dropped.
    best_ridge_coef <- as.numeric(coef(cv.ridge12, s = cv.ridge12$lambda.min))[-1]
    ## Adaptive Lasso
    set.seed(999)
    ## Perform adaptive LASSO
    alasso12 <- glmnet(x, y,
                      ## type.measure: loss to use for cross-validation.
                      ## ‘alpha = 1’ is the lasso penalty, and ‘alpha = 0’ the ridge penalty.
                      alpha = 1,
                      ##
                      ## penalty.factor: Separate penalty factors can be applied to each
                      ##           coefficient. This is a number that multiplies ‘lambda’ to
                      ##           allow differential shrinkage. Can be 0 for some variables,
                      ##           which implies no shrinkage, and that variable is always
                      ##           included in the model. Default is 1 for all variables (and
                      ##           implicitly infinity for variables listed in ‘exclude’).                          Note:
                      ##           the penalty factors are internally rescaled to sum to nvars,
                      ##           and the lambda sequence will reflect this change.
                      penalty.factor = 1 / abs(best_ridge_coef))
    plot(alasso12, xvar = "lambda")
    #print(alasso1)
    ## Perform adaptive LASSO with 10-fold CV
    alasso12_cv <- cv.glmnet(x, y,
                            ## type.measure: loss to use for cross-validation.
                            type.measure = "mse",
                            ## K = 10 is the default.
                            nfold = 10,
                            alpha = 1,
                            penalty.factor = 1 / abs(best_ridge_coef),
                            ## prevalidated array is returned
                            keep = TRUE)
    ## Penalty vs CV MSE plot
    plot(alasso12_cv)
    ## Extract coefficients at the error-minimizing lambda
    #lambda.min is the value of λ that gives minimum mean cross-validated error
    alasso12_cv$lambda.min
    ## s: Value(s) of the penalty parameter ‘lambda’ at which
    ##    predictions are required. Default is the entire sequence used
    ##    to create the model.
    best_alasso_coef12 <- coef(alasso12_cv, s = alasso12_cv$lambda.min)
    best_alasso_coef12
    selected_attributes <- (best_alasso_coef12@i[-1]+1) 
    ## Function for R^2
    ## https://en.wikipedia.org/wiki/Coefficient_of_determination
    r_squared <- function(y, yhat) {
      ybar <- mean(y)
      ## Total SS
      ss_tot <- sum((y - ybar)^2)
      ## Residual SS
      ss_res <- sum((y - yhat)^2)
      ## R^2 = 1 - ss_res/ ss_tot
      1 - (ss_res / ss_tot)
    }
    ## Function for Adjusted R^2
    ## n sample size, p number of prameters
    adj_r_squared <- function(r_squared, n, p) {
      1 - (1 - r_squared) * (n - 1) / (n - p - 1)
    }
    ## Obtain R^2
    r_squared_alasso12 <- r_squared(as.vector(y), as.vector(predict(alasso12, newx = x, s = alasso12_cv$lambda.min)))
    ## Obtain adjusted R^2
    r_squared_alasso12.adj <- adj_r_squared(r_squared_alasso12, n = nrow(y), p = sum(as.vector(coef(alasso12, s = alasso12_cv$lambda.min))!=0))
    sprintf("The R-squared is %0.2f and adjusted R-squared is %0.2f", r_squared_alasso12, r_squared_alasso12.adj)
    ## Cross-validated test-set R^2
    ## alasso1_cv$cvm[1] is the cross-validated test set mean squared error of the intercept-only model.
    1 - alasso12_cv$cvm[alasso12_cv$lambda == alasso12_cv$lambda.min] / alasso12_cv$cvm[1]
    xlab_names <-expression(lIPL%->%cerebellum, ldlPFC%->%lIPL, dACC%->%lAG, Flexibility%->%lAG, dACC%->%visual, lIFJ*Flexibility%->%visual)
    out <- Sys.glob(paste(path, '/Pegasus/1Output/8ROIs/', script_version[1], sep = ""))
    setwd(out)
    level_string = c("Group", "Subgroup", "Individual")
    dev.off()
    par(mfrow=c(2,3))
    if (!(is.null(length(selected_attributes)))) {
      for (i in 1:length(selected_attributes)) {
        print(rownames(best_alasso_coef12)[selected_attributes][i])
        cat(sprintf("Beta=%0.2f\n", best_alasso_coef12[selected_attributes][i]))
        # is path group, subgroup, or individual?
        IV_DV <-strsplit(rownames(best_alasso_coef12)[selected_attributes][i], "_")
        IV <- IV_DV[[1]][1]
        DV <- IV_DV[[1]][2] 
        tmp <- timepoints[[1]][as.character(timepoints[[1]]$dv)==DV & 
                        as.character(timepoints[[1]]$iv)==IV,7]
        cat(sprintf("Path is %s-level\n", level_string[tmp]))
        # find correct column in x that matches with selected_attribute
        j <- which(colnames(x) == rownames(best_alasso_coef12)[selected_attributes][i])
        # plot
        print(plot(x[,j], y, xlab=xlab_names[[i]], ylab= "FIST efficiency", main=sprintf("b=%0.2f", best_alasso_coef12[selected_attributes][i]), xlim=c(-.4, .8), ylim=c(0, 10)))
        readline(prompt="Press [enter] to continue")
      } 
    }
    dev.print(pdf, "LASSO_plots.pdf")
    dev.off()
    
# Runs 3 and 4
    x <- as.matrix(normal.paths[[2]][-1]) # Removes class
    colnames(x)
    y <- as.matrix(accRT.goodfit34[,2]) # Only class
    
    ## Ridge Regression to create the Adaptive Weights Vector
    set.seed(999)
    cv.ridge <- cv.glmnet(x, y, family='gaussian', 
                          ## ‘alpha = 1’ is the lasso penalty, 
                          ## and ‘alpha = 0’ the ridge penalty.
                          alpha=0, standardize=TRUE, 
                          ## type.measure: loss to use for cross-validation.
                          type.measure = "mse",
                          ## K = 10 is the default.
                          nfold = 10)
    plot(cv.ridge)
    ## Extract coefficients at the error-minimizing lambda
    cv.ridge$lambda.min
    ## s: Value(s) of the penalty parameter ‘lambda’ at which
    ##    predictions are required. Default is the entire sequence used
    ##    to create the model.
    coef(cv.ridge, s = cv.ridge$lambda.min)
    # The intercept estimate should be dropped.
    best_ridge_coef <- as.numeric(coef(cv.ridge, s = cv.ridge$lambda.min))[-1]
    
    ## Adaptive Lasso
    set.seed(999)
    ## Perform adaptive LASSO
    alasso1 <- glmnet(x, y,
                      ## type.measure: loss to use for cross-validation.
                      ## ‘alpha = 1’ is the lasso penalty, and ‘alpha = 0’ the ridge penalty.
                      alpha = 1,
                      ##
                      ## penalty.factor: Separate penalty factors can be applied to each
                      ##           coefficient. This is a number that multiplies ‘lambda’ to
                      ##           allow differential shrinkage. Can be 0 for some variables,
                      ##           which implies no shrinkage, and that variable is always
                      ##           included in the model. Default is 1 for all variables (and
                      ##           implicitly infinity for variables listed in ‘exclude’). Note:
                      ##           the penalty factors are internally rescaled to sum to nvars,
                      ##           and the lambda sequence will reflect this change.
                      penalty.factor = 1 / abs(best_ridge_coef))
    plot(alasso1, xvar = "lambda")
    #print(alasso1)
    ## Perform adaptive LASSO with 10-fold CV
    alasso1_cv <- cv.glmnet(x, y,
                            ## type.measure: loss to use for cross-validation.
                            type.measure = "mse",
                            ## K = 10 is the default.
                            nfold = 10,
                            alpha = 1,
                            penalty.factor = 1 / abs(best_ridge_coef),
                            ## prevalidated array is returned
                            keep = TRUE)
    ## Penalty vs CV MSE plot
    plot(alasso1_cv)
    ## Extract coefficients at the error-minimizing lambda
    #lambda.min is the value of λ that gives minimum mean cross-validated error
    alasso1_cv$lambda.min
    ## s: Value(s) of the penalty parameter ‘lambda’ at which
    ##    predictions are required. Default is the entire sequence used
    ##    to create the model.
    best_alasso_coef1 <- coef(alasso1_cv, s = alasso1_cv$lambda.min)
    best_alasso_coef1
    selected_attributes <- (best_alasso_coef1@i[-1]+1) 
    ## Obtain R^2
    r_squared_alasso1 <- r_squared(as.vector(y), as.vector(predict(alasso1, newx = x, s = alasso1_cv$lambda.min)))
    r_squared_alasso1
    ## Obtain adjusted R^2
    r_squared_alasso1.adj <- adj_r_squared(r_squared_alasso1, n = nrow(y), p = sum(as.vector(coef(alasso1, s = alasso1_cv$lambda.min))!=0))
    sprintf("The R-squared is %0.2f and adjusted R-squared is %0.2f", r_squared_alasso1, r_squared_alasso1.adj)
    ## Cross-validated test-set R^2
    ## alasso1_cv$cvm[1] is the cross-validated test set mean squared error of the intercept-only model.
    1 - alasso1_cv$cvm[alasso1_cv$lambda == alasso1_cv$lambda.min] / alasso1_cv$cvm[1]
    
    xlab_names <-expression(ldlPFC%->%lIPL, lAG%->%ldlPFC, dACC%->%visual, lIFJ%->%visual, Control%->%visual)
    out <- Sys.glob(paste(path, '/Pegasus/1Output/8ROIs/', script_version[2], sep = ""))
    level_string = c("Group", "Subgroup", "Individual")
    setwd(out)
    dev.off()
    par(mfrow=c(2,3))
    #par(mfrow=c(2,3))
    if (!(is.null(length(selected_attributes)))) {
      for (i in 1:length(selected_attributes)) {
        print(rownames(best_alasso_coef1)[selected_attributes][i])
        cat(sprintf("Beta=%0.2f\n", best_alasso_coef1[selected_attributes][i]))
        IV_DV <-strsplit(rownames(best_alasso_coef1)[selected_attributes][i], "_")
        IV <- IV_DV[[1]][1]
        DV <- IV_DV[[1]][2] 
        tmp <- timepoints[[2]][as.character(timepoints[[2]]$dv)==DV & 
                                 as.character(timepoints[[2]]$iv)==IV,7]
        cat(sprintf("Path is %s-level\n", level_string[tmp]))
        # find correct column in x that matches with selected_attribute
        j <- which(colnames(x) == rownames(best_alasso_coef1)[selected_attributes][i])
        # plot
        print(plot(x[,j], y, xlab=xlab_names[[i]], ylab= "FIST efficiency", main=sprintf("b=%0.2f", best_alasso_coef1[selected_attributes][i]), ylim=c(0, 10)))
        readline(prompt="Press [enter] to continue")
      } 
    }
    dev.print(pdf, "LASSO_plots.pdf")
    dev.off()
        