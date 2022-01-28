library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci)
library(Cairo)
library(mlr)
library(plyr)
library(ggrepel) #non-overlapping text labels

# set global theme for all plots

th<-theme(plot.title = element_text(size = 12, face = "bold"),axis.title=element_text(size=12,color = "black"),
          axis.text.x = element_text(size=10, color = "black"),axis.text.y = element_text(size=10, color = "black"))

#read files
data<-read.table("1410_fing_mcs", header = TRUE, sep = ' ') 
head(data)
dim(data)

select_cols1<-na.omit(data[,c(1:8)])
data.reshape1<-melt(select_cols1, id.vars = "Final_dataset.drug_id")
head(data.reshape1)
nrow(data.reshape1)

#Fig1B 

CairoSVG(file="Fig1B.svg", width = 3.2, height = 3.5, bg = "white")
ggplot(data.reshape1,aes(x=variable, y=value))+geom_violin(adjust=3,fill="#bdbdbd", lwd=0.3)+
  geom_boxplot(width=0.1, outlier.shape = NA, fill="white", lwd=0.3)+
  scale_x_discrete(name = "Fingerprints", labels = c("Morgan", "FeatMorgan", "AtomPair","RDKit","Torsion","Layered","MACCS")) + 
  scale_y_continuous(name="Tanimoto Score") + theme_minimal() + 
  theme(plot.title = element_text(size = 14, face = "bold"),axis.title=element_text(size=14,color = "black"),axis.text.x = element_text(size=12, color = "black"),axis.text.y = element_text(size=12, color = "black"))+
  guides(fill=FALSE)+coord_flip()
dev.off()

#Fig1C
#MCS and overlap coefficient
select_cols2<-na.omit(data[,c(1,9,10)])
head(select_cols2)
nrow(select_cols2)

data.reshape2<-melt(select_cols2, id.vars = "Final_dataset.drug_id")
head(data.reshape2)
nrow(data.reshape2)

CairoSVG(file="Fig1C.svg", width = 3, height = 3.4, bg = "white")
ggplot(data.reshape2,aes(x=variable, y=value, fill=variable))+geom_violin(fill="#bdbdbd", lwd=0.3)+
  geom_boxplot(width=0.1, outlier.shape = NA, fill="white", lwd=0.3)+coord_flip()+
  scale_x_discrete(name = "Maximum Common Substructure", labels = c("TS", "OC")) + 
  scale_y_continuous(name="Value") + theme_minimal() + 
  theme(plot.title = element_text(size = 14, face = "bold"),axis.title=element_text(size=14,color = "black"),axis.text.x = element_text(size=12, color = "black"),axis.text.y = element_text(size=12, color = "black"))+
  guides(fill=FALSE)
dev.off()


#Fig 1D
# performance metrics
all<-read.table(file = "perf_measures", sep = '\t', header = TRUE)
head(all)

CairoSVG(file="Fig1D.svg", width = 3.6, height = 4, bg = "white")
all %>% melt() %>% filter(variable %in% c("f1","mcc","ppv","auc")) %>%
  ggplot(aes(x=variable,y=value,fill=classifier)) +
  geom_boxplot(lwd = 0.3, position = position_dodge2()) + scale_fill_jco() + 
  th + scale_x_discrete(name="measure") + theme(legend.position="bottom")
dev.off()


#Fig 1E
#Precision/recall curve
load("pr_recall.rds")
head(pr_recall)

CairoSVG(file="Fig1E.svg", width = 5, height = 5, bg = "white")
plotROCCurves(pr_recall, diagonal = FALSE)
dev.off()


#Fig 1F
#ROC curve
load("roc.rds")
head(roc)

CairoSVG(file="Fig1F.svg", width = 5, height = 5, bg = "white")
plotROCCurves(roc, diagonal = FALSE)
dev.off()

#Fig 1G
# feature importance
##### Feature importance
feat_result<-read.table(file = "feat_importance", sep = '\t', header = TRUE)
head(feat_result)

CairoSVG(file="Fig1G.svg", width = 6, height = 4, bg = "white")
feat_result %>% melt() %>% group_by(set) %>% top_n(20) %>%
  ggplot(aes(reorder(FP, -value), value)) + geom_boxplot(lwd=0.3) +
  xlab("Top ranking features") + ylab("Feature importance") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.line = element_line(size=0.3, 
  colour = "black"), panel.grid = element_blank(), panel.background = element_blank(), axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 14, colour = "black"))
dev.off()

