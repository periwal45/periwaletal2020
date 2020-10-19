setwd("/Users/vinitaperiwal/DrugvsDrug/Paper_Data/Figures/")
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plyr) #for count
library(matchBox) #CAT plots BiocManager::install("matchBox")

# FigureS2 CAT plots
mycolors<-c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02")

data<-read.table("FigureS2/drugs_fp", header = TRUE, sep = '\t')
dim(data)
head(data)

morgan<-comb[order(-comb$f1_morgan),]
head(morgan)
subset_data=morgan[1:1000,]
morgan_compute.cat<-computeCat(subset_data, idCol = 1, ref = "f1_morgan", method = "equalRank", decreasing = TRUE)
fp_morgan<-plotCat(morgan_compute.cat, whichToPlot = 1:length(morgan_compute.cat), legend = T, spacePts = 30, lwd = 2, pch = 4, size = 100, col = mycolors, cexPts = 1, lty = 6, plotLayout=layout(matrix(c(1,2,1,2,1,2), 2, 2, byrow = TRUE), widths = c(0.1, 0.1), heights = c(2,1)))


fm<-comb[order(-comb$f1_featmorgan),]
head(fm)
subset_data=fm[1:1000,]
featmorgan_compute.cat<-computeCat(subset_data, idCol = 1, ref = "f1_featmorgan", method = "equalRank", decreasing = TRUE)
fp_fm<-plotCat(featmorgan_compute.cat, whichToPlot = 1:length(featmorgan_compute.cat), legend = T, spacePts = 30, lwd = 2, pch = 4, size = 100, col = mycolors, cexPts = 1, lty = 6, plotLayout=layout(matrix(c(1,2,1,2,1,2), 2, 2, byrow = TRUE), widths = c(0.1, 0.1), heights = c(2,1)))


ap<-comb[order(-comb$f1_atompair),]
head(ap)
nrow(ap)
subset_data=ap[1:1000,]
ap_compute.cat<-computeCat(subset_data, idCol = 1, ref = "f1_atompair", method = "equalRank", decreasing = TRUE)
fp_ap<-plotCat(ap_compute.cat, whichToPlot = 1:length(ap_compute.cat), legend = T, spacePts = 30, lwd = 2, pch = 4, size = 100, col = mycolors, cexPts = 1, lty = 6, plotLayout=layout(matrix(c(1,2,1,2,1,2), 2, 2, byrow = TRUE), widths = c(0.1, 0.1), heights = c(2,1)))


rdkit<-comb[order(-comb$f1_rdkit),]
head(rdkit)
subset_data=rdkit[1:1000,]
rdkit_compute.cat<-computeCat(subset_data, idCol = 1, ref = "f1_rdkit", method = "equalRank", decreasing = TRUE)
fp_rdkit<-plotCat(rdkit_compute.cat, whichToPlot = 1:length(rdkit_compute.cat), legend = T, spacePts = 30, lwd = 2, pch = 4, size = 100, col = mycolors, cexPts = 1, lty = 6, plotLayout=layout(matrix(c(1,2,1,2,1,2), 2, 2, byrow = TRUE), widths = c(0.1, 0.1), heights = c(2,1)))

torsion<-comb[order(-comb$f1_torsion),]
head(torsion)
subset_data=torsion[1:1000,]
torsion_compute.cat<-computeCat(subset_data, idCol = 1, ref = "f1_torsion", method = "equalRank", decreasing = TRUE)
fp_torsion<-plotCat(torsion_compute.cat, whichToPlot = 1:length(torsion_compute.cat), legend = T, spacePts = 30, lwd = 2, pch = 4, size = 100, col = mycolors, cexPts = 1, lty = 6, plotLayout=layout(matrix(c(1,2,1,2,1,2), 2, 2, byrow = TRUE), widths = c(0.1, 0.1), heights = c(2,1)))

layered<-comb[order(-comb$f1_layered),]
head(layered)
subset_data=layered[1:1000,]
layered_compute.cat<-computeCat(subset_data, idCol = 1, ref = "f1_layered", method = "equalRank", decreasing = TRUE)
fp_layered<-plotCat(layered_compute.cat, whichToPlot = 1:length(layered_compute.cat), legend = T, spacePts = 30, lwd = 2, pch = 4, size = 100, col = mycolors, cexPts = 1, lty = 6, plotLayout=layout(matrix(c(1,2,1,2,1,2), 2, 2, byrow = TRUE), widths = c(0.1, 0.1), heights = c(2,1)))

maccs<-comb[order(-comb$f1_maccs),]
head(maccs)
subset_data=maccs[1:1000,]
maccs_compute.cat<-computeCat(subset_data, idCol = 1, ref = "f1_maccs", method = "equalRank", decreasing = TRUE)
fp_maccs<-plotCat(maccs_compute.cat, whichToPlot = 1:length(maccs_compute.cat), legend = T, spacePts = 30, lwd = 2, pch = 4, size = 100, col = mycolors, cexPts = 1, lty = 6, plotLayout=layout(matrix(c(1,2,1,2,1,2), 2, 2, byrow = TRUE), widths = c(0.1, 0.1), heights = c(2,1)))
