library(mlr)
library(dplyr)

set.seed(4)

DvF<-data.frame(read.csv("drug_food.csv", header=TRUE, sep=","))
dim(DvF)

#Remove high scoring pairs
DvF<-DvF %>% filter(f1_morgan < 0.9 & f1_featmorgan < 0.9 & f1_atompair < 0.9 & f1_rdkit < 0.9 & f1_torsion < 0.9 & f1_layered < 0.9 & f1_maccs < 0.9)
dim(DvF)

# load model
load("wt_rforest.rda")

pred_DvF.set = predict(wt_rforest, newdata = DvF, type = "prob")
pred_set = setThreshold(pred_DvF.set,0.6)
predictions<-data.frame(DvF$Final_dataset.drug_id,as.data.frame(pred_set))
write.csv(predictions, file="drug_food_preds.csv", row.names = FALSE)