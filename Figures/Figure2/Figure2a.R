# creating a scatter plot to show predictions from best fingerprint and ML model
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci)
library(Cairo)

# plotting food predictions different threshold

food_preds<-data.frame(read.table(file = "drug_food_preds.csv", header = TRUE, sep = ','))
head(food_preds)
nrow(food_preds)

prob.5<-nrow(food_preds %>% filter(prob.Match > 0.5))
prob.6<-nrow(food_preds %>% filter(prob.Match > 0.6))
prob.7<-nrow(food_preds %>% filter(prob.Match > 0.7))
prob.8<-nrow(food_preds %>% filter(prob.Match > 0.8))
prob.9<-nrow(food_preds %>% filter(prob.Match > 0.9))

counts<-data.frame(prob.5,prob.6,prob.7,prob.8,prob.9)
counts

CairoSVG(file="prob.svg", width = 3, height = 2, bg = "white")
counts %>% melt %>% ggplot(aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  th + scale_x_discrete(name = "probability cut off", labels=c("prob.5" = "> 0.5","prob.6" = "> 0.6","prob.7" = "> 0.7","prob.8" = "> 0.8","prob.9" = "> 0.9")) + 
  scale_y_continuous(name = "count") +
  geom_text(aes(label=value), color="black", size = 3, nudge_y = 30)
dev.off()

