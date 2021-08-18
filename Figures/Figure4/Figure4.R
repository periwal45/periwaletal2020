# creating a scatter plot to show predictions from best fingerprint and ML model
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci)
library(ggpubr)
library(Cairo)
library(ggrepel) #non-overlapping text labels

# set global theme for all plots

th<-theme(plot.title = element_text(size = 12, face = "bold"),axis.title=element_text(size=12,color = "black"),
          axis.text.x = element_text(size=10, color = "black"),axis.text.y = element_text(size=10, color = "black"))

#load feat morgan outcomes and food predictions
food_preds<-read.table("drug_food_preds.csv", header = TRUE, sep = ',') 
View(food_preds)
nrow(food_preds)

names(food_preds)[1]<-"Final_dataset.drug_id"

feat<-read.table("feat_drugfood", header = TRUE, sep = ' ') 
head(feat)
nrow(feat)

merged<-merge(feat,food_preds,by="Final_dataset.drug_id")
head(merged)
nrow(merged)

merged<-na.omit(merged)
nrow(merged)
head(merged)

CairoSVG(file="scatter_plot.svg", width = 5, height = 4, bg = "white")
merged %>% filter(prob.Match > 0.5) %>% 
  ggscatter(x="f1_featmorgan",y="prob.Match",add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",shape = 20,size = 0.5) + geom_point(aes(color=response)) + th + scale_x_continuous(name = "FeatMorgan (TS score)") + 
  scale_y_continuous(name="Predicted probability") + theme_minimal() +
  scale_color_jama()
dev.off()
