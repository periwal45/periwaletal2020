# creating a scatter plot to show predictions from best fingerprint and ML model
library(reshape2)
library(dplyr)
library(data.table)
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
# predictions
set1_food<-data.frame(read.csv(file = "set1rf_drug_food_preds.csv", header = TRUE)) #refer file on mendeley link
set1_food<-na.omit(set1_food)
set2_food<-data.frame(read.csv(file = "set2rf_drug_food_preds.csv", header = TRUE)) #refer file on mendeley link
set2_food<-na.omit(set2_food)
set3_food<-data.frame(read.csv(file = "set3rf_drug_food_preds.csv", header = TRUE)) #refer file on mendeley link
set3_food<-na.omit(set3_food)
set4_food<-data.frame(read.csv(file = "set4rf_drug_food_preds.csv", header = TRUE)) #refer file on mendeley link
set4_food<-na.omit(set4_food)
set5_food<-data.frame(read.csv(file = "set5rf_drug_food_preds.csv", header = TRUE)) #refer file on mendeley link
set5_food<-na.omit(set5_food)

names(set1_food)[1]<-"Final_dataset.drug_id"
names(set2_food)[1]<-"Final_dataset.drug_id"
names(set3_food)[1]<-"Final_dataset.drug_id"
names(set4_food)[1]<-"Final_dataset.drug_id"
names(set5_food)[1]<-"Final_dataset.drug_id"

# taking average probability values of all sets
options(datatable.verbose=TRUE)
dt<-rbindlist(list(set1_food[,c(1:2)],set2_food[,c(1:2)],set3_food[,c(1:2)],set4_food[,c(1:2)],set5_food[,c(1:2)]))[,lapply(.SD,mean), Final_dataset.drug_id]
View(dt)

feat<-read.table("feat_drugfood", header = TRUE, sep = ' ') 
head(feat)
nrow(feat)

merged<-merge(feat,dt,by="Final_dataset.drug_id")
View(merged)
nrow(merged)

merged$Colour="#000000"
merged$Colour[merged$Final_dataset.drug_id=="Drug-1464:F11962"]="#FF0000"

CairoSVG(file="scatter_plot.svg", width = 5, height = 4, bg = "white")
merged %>% filter(prob.Match > 0.5) %>% 
  ggscatter(x="f1_featmorgan",y="prob.Match",add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",shape = 20,size = 0.5, alpha=0.3) + 
  th + scale_x_continuous(name = "FeatMorgan (TS score)") + 
  scale_y_continuous(name="Predicted probability") + theme_minimal() +
  geom_point(aes(color=Colour))
  
dev.off()

# sel<-merged %>% filter(prob.Match > 0.4)
# write.csv(sel, file = "for_ranks.csv", row.names = FALSE)

