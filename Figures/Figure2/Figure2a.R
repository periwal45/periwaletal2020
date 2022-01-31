# drug food predictions by all 5 models
library(reshape2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggsci)
library(Cairo)

# set global theme for all plots

th<-theme(plot.title = element_text(size = 12, face = "bold"),axis.title=element_text(size=12,color = "black"),
          axis.text.x = element_text(size=10, color = "black"),axis.text.y = element_text(size=10, color = "black"))


# predictions
set1_food<-data.frame(read.csv(file = "set1_drug_food.csv", header = FALSE))
set2_food<-data.frame(read.csv(file = "set2_drug_food.csv", header = FALSE))
set3_food<-data.frame(read.csv(file = "set3_drug_food.csv", header = FALSE))
set4_food<-data.frame(read.csv(file = "set4_drug_food.csv", header = FALSE))
set5_food<-data.frame(read.csv(file = "set5_drug_food.csv", header = FALSE))

names(set1_food)[1]<-"Final_dataset.drug_id"
names(set2_food)[1]<-"Final_dataset.drug_id"
names(set3_food)[1]<-"Final_dataset.drug_id"
names(set4_food)[1]<-"Final_dataset.drug_id"
names(set5_food)[1]<-"Final_dataset.drug_id"

sets<-c("set1","set2","set3","set4","set5")
value<-c(676,826,911,735,617)
df<-data.frame(sets,value)
head(df)

CairoSVG(file="Fig2a.svg", width = 3, height = 2, bg = "white")
df %>% ggplot(aes(x=sets,y=value)) + geom_bar(stat = "identity") +
  th + scale_x_discrete(name = "set") + 
  scale_y_continuous(name = "count") +
  geom_text(aes(label=value), color="black", size = 3, nudge_y = 40)
dev.off()


# command line count of each pair
#sort all_set_DvF | uniq -c > count_of_pairs
