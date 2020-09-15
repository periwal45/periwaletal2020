setwd("/Users/vinitaperiwal/periwaletal2020/Figures")
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci)

# set global theme for all plots

th<-theme(plot.title = element_text(size = 12, face = "bold"),axis.title=element_text(size=12,color = "black"),
          axis.text.x = element_text(size=10, color = "black"),axis.text.y = element_text(size=10, color = "black"))

#read files
data<-read.table("Figure1/1410_fing_mcs", header = TRUE, sep = ' ') 
head(data)
ncol(data) #11

fv = data.frame(read.csv("Figure1/FV_filter_values.csv", header = TRUE, sep = ','))
head(fv)
colnames(fv)<-c("col_id", "name", "type", "feature_importance")

select_cols1<-na.omit(data[,c(1:8)])

head(select_cols1)
nrow(select_cols1) #993,345

data.reshape1<-melt(select_cols1, id.vars = "Final_dataset.drug_id")
head(data.reshape1)
nrow(data.reshape1) #6,953,415

#Fig1B 

CairoSVG(file="/Figure1/Fig1B.svg", width = 3.2, height = 3.5, bg = "white")
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
nrow(select_cols2) #993,345

data.reshape2<-melt(select_cols2, id.vars = "Final_dataset.drug_id")
head(data.reshape2)
nrow(data.reshape2) #1,986,690

CairoSVG(file="/Figure1/Fig1C.svg", width = 3, height = 3.4, bg = "white")
ggplot(data.reshape2,aes(x=variable, y=value, fill=variable))+geom_violin(fill="#bdbdbd", lwd=0.3)+
  geom_boxplot(width=0.1, outlier.shape = NA, fill="white", lwd=0.3)+coord_flip()+
  scale_x_discrete(name = "Maximum Common Substructure", labels = c("TS", "OC")) + 
  scale_y_continuous(name="value") + theme_minimal() + 
  theme(plot.title = element_text(size = 14, face = "bold"),axis.title=element_text(size=14,color = "black"),axis.text.x = element_text(size=12, color = "black"),axis.text.y = element_text(size=12, color = "black"))+
  guides(fill=FALSE)
dev.off()

##Fig 1D
## plotting only top 25 features
fv_filter<-filter(fv, fv$feature_importance > 32)
head(fv_filter)
nrow(fv_filter)

CairoSVG(file="/Figure1/Fig1D.svg", width = 5, height = 4, bg = "white")
ggplot(fv_filter, aes(reorder(name, -feature_importance), feature_importance)) + geom_bar(stat = "identity", fill = "#bdbdbd") +
  xlab("Features (top 20)") + ylab("Features importance") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.line = element_line(size=0.3, colour = "black"), panel.grid = element_blank(), panel.background = element_blank(), axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 14, colour = "black"))
dev.off()

##Fig 1E
hyp<-data.frame(read.table(file="Models_TrainingMeasures", header = TRUE, sep = '\t'))
head(hyp)
melt.hyp<-melt(hyp, id.vars = c("Model","TPR","FNR","FPR","TNR","ACC","MMCE","Kappa"), measure.vars = c("TPR","FNR","FPR","TNR","ACC","MMCE","Kappa","MCC"))
head(melt.hyp)

CairoSVG(file="/Figure1/Fig1E.svg", width = 3.4, height = 3, bg = "white")
melt.hyp %>% dplyr::group_by(Model) %>% ggplot(aes(x=value,y=variable,fill=Model)) + 
  geom_boxplot(lwd=0.3) + scale_fill_jama() + scale_y_discrete(name = "Measure") +
  th + theme(legend.position = "bottom")
dev.off()

## Fig1F and 1G
#load predictions from test set
load("Figure1/default_pred.rda")
load("Figure1/hyp1_pred.rda")
load("Figure1/hyp2_pred.rda")
load("Figure1/hyp3_pred.rda")

mlr::performance(default_pred, measures = list(mcc,tpr,fpr,fnr,tnr,ppv,acc,bac,mmce,kappa,auc))
mlr::performance(hyp1_pred, measures = list(mcc,tpr,fpr,fnr,tnr,ppv,acc,bac,mmce,kappa,auc))
mlr::performance(hyp2_pred, measures = list(mcc,tpr,fpr,fnr,tnr,ppv,acc,bac,mmce,kappa,auc))
mlr::performance(hyp3_pred, measures = list(mcc,tpr,fpr,fnr,tnr,ppv,acc,bac,mmce,kappa,auc))

## performance of test set
df = generateThreshVsPerfData(list(Default = default_pred, Hyp1 = hyp1_pred, Hyp2 = hyp2_pred, Hyp3 = hyp3_pred),measures = list(ppv,tpr,fpr))

#Fig 1F
#Precision/recall curve
CairoSVG(file="/Figure1/Fig1F.svg", width = 3.2, height = 3.5, bg = "white")
plotROCCurves(df, measures = list(tpr,ppv), diagonal = FALSE)
dev.off()

#Fig 1G
#ROC curve
CairoSVG(file="/Figure1/Fig1G.svg", width = 3.2, height = 3.5, bg = "white")
plotROCCurves(df, measures = list(fpr,tpr))
dev.off()

#Fig S3
ggplot(fv, aes(x=name, y=feature_importance)) + geom_point(fill = "#bdbdbd", shape = 21) +
  xlab("Features") + ylab("Feature importance") + geom_hline(aes(yintercept=mean(feature_importance)), color="red", linetype="dashed", size=1) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line = element_line(size=0.3, colour = "black"), panel.grid = element_blank(), panel.background = element_blank(), axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 14, colour = "black"))

