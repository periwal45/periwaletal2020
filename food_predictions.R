library(mlr)
library(dplyr)

set.seed(4)

DvF<-data.frame(read.csv("drug_food.csv", header=TRUE, sep=","))
dim(DvF)

#Remove high scoring pairs
DvF<-DvF %>% filter(f1_morgan < 0.9 & f1_featmorgan < 0.9 & f1_atompair < 0.9 & f1_rdkit < 0.9 & f1_torsion < 0.9 & f1_layered < 0.9 & f1_maccs < 0.9)
dim(DvF)

# load model
wt300_rf<-load("train_dist_desc_set1_wt_rforestT.rda") #files on mendeley link
print("set1 model")
get(wt300_rf)

pred_DvF.set1 = predict(get(wt300_rf), newdata = DvF, type = "prob")
pred_DvF<-data.frame(DvF$Final_dataset.drug_id,as.data.frame(pred_DvF.set1))
write.csv(pred_DvF, file="set1rf_drug_food_preds.csv", row.names = FALSE)
