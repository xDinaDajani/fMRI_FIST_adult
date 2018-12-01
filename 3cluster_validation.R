# GIMME Analyses KKI PerturbR Cluster validity

rm(list=ls())
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
script_version <- 'v39*'
out <- Sys.glob(paste(path, '/Pegasus/1Output/8ROIs/', script_version, sep = ""))
setwd(out)

getwd()
load("workspace.Rdata")
load("perturbR.Rdata")
rm(list=setdiff(ls(), c("fit", "stability")))

 
# cluster solution modularity
fit$fit$modularity[1]
# # 95% cutoff for random graph modularity
stability$cutoff
stability$vi20mark


# ===Plot for VI=============================================================================

#find stability$percent where meanVI == ari20mark
meanVI <-colMeans(stability$VI)
meanVI.rando <-colMeans(stability$VI.rando)
plot_VI <- as.data.frame(cbind(stability$percent, meanVI, meanVI.rando))
VI20_edges_perturbed <- plot_VI[which.min(abs(meanVI-stability$vi20mark)), 1]
VI20_edges_perturbed

if (VI20_edges_perturbed>.20) {
  cat(sprintf('Graph is stable according to VI (Percent edges perturbed for 20%% of nodes in different cluster: %0.3f)', VI20_edges_perturbed))
} else {
  cat(sprintf('Graph is NOT stable according to VI (Percent edges perturbed for 20%% of nodes in different cluster: %0.3f)',VI20_edges_perturbed))
}

colnames(plot_VI) <- c("Edgesperturbed", "MeanVI", "MeanVI_random")
datalong <- reshape(data=plot_VI, 
                    varying =2:3, 
                    v.names = "MeanVI", 
                    timevar = "Original_random",
                    idvar = "Edgesperturbed",
                    direction = "long")
datalong$Original_random <- as.factor(datalong$Original_random)
library(ggplot2)
pVI <- ggplot(datalong, aes(x=Edgesperturbed, y=MeanVI, color=Original_random)) +
  geom_point(fill="grey40") +
  scale_color_manual(values=c("black", "red")) +
  geom_hline(yintercept=stability$vi20mark, color='black') +
  #geom_hline(yintercept=stability$vi10mark) +
  geom_vline(xintercept = VI20_edges_perturbed, alpha=.5, linetype="dashed") +
  labs(x="\nEdges perturbed (%)", y = "Mean VI\n") +
  theme_classic() +
  theme(axis.text=element_text(size=12), 
        axis.title.y=element_text(size=14), 
        axis.title.x=element_text(size=14),
        plot.margin = margin(.5, .5, .5, .5, "cm"),
        legend.position="none")
pVI
dev.off()
#setwd('/Users/ddajani/Box Sync/Lab/Writing/EFclassesConnectivity/JAACAP submission/Revision/figures')
# #ggsave(p, 
#        filename = "PerturbR.pdf",
#        width=5,
#        height=5,
#        units = "in")


# ARI
meanARI <-colMeans(stability$ARI)
meanARI.rando <-colMeans(stability$ARI.rando)
plot_ARI <- as.data.frame(cbind(stability$percent, meanARI, meanARI.rando))
ARI20_edges_perturbed <- plot_ARI[which.min(abs(meanARI-stability$ari20mark)), 1]
ARI20_edges_perturbed

if (ARI20_edges_perturbed>.20) {
  cat(sprintf('Graph is stable according to ARI (Percent edges perturbed for 20%% of nodes in different cluster: %0.3f)', ARI20_edges_perturbed))
} else {
  cat(sprintf('Graph is NOT stable according to ARI (Percent edges perturbed for 20%% of nodes in different cluster: %0.3f)',ARI20_edges_perturbed))
}

colnames(plot_ARI) <- c("Edgesperturbed", "MeanARI", "MeanARI_random")
datalong <- reshape(data=plot_ARI, 
                    varying =2:3, 
                    v.names = "MeanARI", 
                    timevar = "Original_random",
                    idvar = "Edgesperturbed",
                    direction = "long")
datalong$Original_random <- as.factor(datalong$Original_random)
pARI <- ggplot(datalong, aes(x=Edgesperturbed, y=MeanARI, color=Original_random)) +
  geom_point(fill="grey40") +
  scale_color_manual(values=c("black", "red")) +
  geom_hline(yintercept=stability$ari20mark, color='black') +
  #geom_hline(yintercept=stability$ari10mark) +
  geom_vline(xintercept = ARI20_edges_perturbed, alpha=.5, linetype="dashed") +
  labs(x="\nEdges perturbed (%)", y = "Mean ARI\n") +
  theme_classic() +
  theme(axis.text=element_text(size=12), 
        axis.title.y=element_text(size=14), 
        axis.title.x=element_text(size=14),
        plot.margin = margin(.5, .5, .5, .5, "cm"),
        legend.position="none")
pARI 

#modularity histogram
mod <-as.data.frame(stability$modularity.rando[,100])
z <- as.numeric(fit$fit$modularity[1])
colnames(mod) <- "mod"
s <- as.numeric(stability$cutoff)
if (z>s) {
  cat(sprintf("Modularity (%0.3f) is better than a random graph's (95ile:%0.3f)",z,s))
} else {
  cat(sprintf("Modularity (%0.3f) is not better than expected by chance (95ile: %0.3f)",z,s))
}

mod_cutoffs <-rbind(s, z)
colnames(mod_cutoffs) <- "modvalues"
labels <- as.data.frame(c(paste0("Q.95=\n", format(s, digits=2)), paste0("Qoriginal=\n", format(z, digits=2))))
colnames(labels) <- "labels"
lines <- cbind(mod_cutoffs, labels)
pMod <- ggplot(mod, aes(x=mod)) +
  geom_histogram(bins=35, color="black", fill="grey40") +
  geom_vline(xintercept = z, color="red") +
  geom_vline(xintercept = s, alpha=.8, linetype="dashed") +
  #geom_text(data = lines, aes(x=modvalues, label=labels, y=15), hjust = "middle", parse=TRUE) +
  geom_label(data = lines, aes(x=modvalues, label=labels, y=c(8.2, 8.2)), label.size=NA,
             size=3.7, label.r=unit(1, "lines")) +
  #scale_x_discrete(labels= round(c(0, .02, .05, .08, .10), digits=2)) +
  labs(x="\nModularity (Q)", y = "Frequency\n") +
  ylim(0,10) +
  #xlim(0,.11) +
  theme_classic() +
  theme(axis.text=element_text(size=12), 
        axis.title.y=element_text(size=14), 
        axis.title.x=element_text(size=14),
        plot.margin = margin(.5, .5, .5, .5, "cm"),
        legend.position="none")
pMod
#install.packages("gridExtra")
#install.packages("cowplot")
#install.packages("gridGraphics")
library(cowplot)
bothplots <- plot_grid(pVI, pARI, pMod, nrow=1, labels = "AUTO",
                       align="h")
ggplot2::ggsave(bothplots, 
                filename = "PerturbR.pdf",
                width=13,
                height=5,
                units = "in")

