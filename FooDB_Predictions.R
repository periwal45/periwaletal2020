library(mlr)
library(dplyr)

#################################### FooDB predictions #########################################
cols<-read.table("188_col_names.csv", check.names = FALSE, header = F, quote = "", stringsAsFactors = F)$V1
cols

## Load default model
model.default<-load("rforest_default.rda") # model files available at: http://dx.doi.org/10.17632/7ft539gwf3.1

## loading foodb files
DvF_0<-data.frame(read.csv("DvF_0", header = TRUE, sep = '\t'))
DvF_0<-DvF_0[,cols]
dim(DvF_0)

DvF_1<-data.frame(read.csv("DvF_1", header = TRUE, sep = '\t'))
DvF_1<-DvF_1[,cols]
DvF_2<-data.frame(read.csv("DvF_2", header = TRUE, sep = '\t'))
DvF_2<-DvF_2[,cols]
DvF_3<-data.frame(read.csv("DvF_3", header = TRUE, sep = '\t'))
DvF_3<-DvF_3[,cols]
DvF_4<-data.frame(read.csv("DvF_4", header = TRUE, sep = '\t'))
DvF_4<-DvF_4[,cols]
DvF_5<-data.frame(read.csv("DvF_5", header = TRUE, sep = '\t'))
DvF_5<-DvF_5[,cols]
DvF_6<-data.frame(read.csv("DvF_6", header = TRUE, sep = '\t'))
DvF_6<-DvF_6[,cols]
DvF_7<-data.frame(read.csv("DvF_7", header = TRUE, sep = '\t'))
DvF_7<-DvF_7[,cols]
DvF_8<-data.frame(read.csv("DvF_8", header = TRUE, sep = '\t'))
DvF_8<-DvF_8[,cols]

# Default model predictions
pred_DvF_0 = predict(get(model.default), newdata = DvF_0, type = "prob")
pred_DvF_0
pred_DvF_1 = predict(get(model.default), newdata = DvF_1, type = "prob")
pred_DvF_2 = predict(get(model.default), newdata = DvF_2, type = "prob")
pred_DvF_3 = predict(get(model.default), newdata = DvF_3, type = "prob")
pred_DvF_4 = predict(get(model.default), newdata = DvF_4, type = "prob")
pred_DvF_5 = predict(get(model.default), newdata = DvF_5, type = "prob")
pred_DvF_6 = predict(get(model.default), newdata = DvF_6, type = "prob")
pred_DvF_7 = predict(get(model.default), newdata = DvF_7, type = "prob")
pred_DvF_8 = predict(get(model.default), newdata = DvF_8, type = "prob")

tot<-rbind(data.frame(pred_DvF_0),data.frame(pred_DvF_1),data.frame(pred_DvF_2),data.frame(pred_DvF_3),data.frame(pred_DvF_4),data.frame(pred_DvF_5),data.frame(pred_DvF_6),data.frame(pred_DvF_7),data.frame(pred_DvF_8))
nrow(tot)


# Hyp1 model predictions
model.hyp1<-load("rforest_hyp1.rda")

pred_DvF_0 = predict(get(model.hyp1), newdata = DvF_0, type = "prob")
pred_DvF_1 = predict(get(model.hyp1), newdata = DvF_1, type = "prob")
pred_DvF_2 = predict(get(model.hyp1), newdata = DvF_2, type = "prob")
pred_DvF_3 = predict(get(model.hyp1), newdata = DvF_3, type = "prob")
pred_DvF_4 = predict(get(model.hyp1), newdata = DvF_4, type = "prob")
pred_DvF_5 = predict(get(model.hyp1), newdata = DvF_5, type = "prob")
pred_DvF_6 = predict(get(model.hyp1), newdata = DvF_6, type = "prob")
pred_DvF_7 = predict(get(model.hyp1), newdata = DvF_7, type = "prob")
pred_DvF_8 = predict(get(model.hyp1), newdata = DvF_8, type = "prob")

tot_hyp1<-rbind(data.frame(pred_DvF_0),data.frame(pred_DvF_1),data.frame(pred_DvF_2),data.frame(pred_DvF_3),data.frame(pred_DvF_4),data.frame(pred_DvF_5),data.frame(pred_DvF_6),data.frame(pred_DvF_7),data.frame(pred_DvF_8))
nrow(tot_hyp1)


# Hyp2 model predictions
model.hyp2<-load("rforest_hyp2.rda")

pred_DvF_0 = predict(get(model.hyp2), newdata = DvF_0, type = "prob")
pred_DvF_1 = predict(get(model.hyp2), newdata = DvF_1, type = "prob")
pred_DvF_2 = predict(get(model.hyp2), newdata = DvF_2, type = "prob")
pred_DvF_3 = predict(get(model.hyp2), newdata = DvF_3, type = "prob")
pred_DvF_4 = predict(get(model.hyp2), newdata = DvF_4, type = "prob")
pred_DvF_5 = predict(get(model.hyp2), newdata = DvF_5, type = "prob")
pred_DvF_6 = predict(get(model.hyp2), newdata = DvF_6, type = "prob")
pred_DvF_7 = predict(get(model.hyp2), newdata = DvF_7, type = "prob")
pred_DvF_8 = predict(get(model.hyp2), newdata = DvF_8, type = "prob")

tot_hyp2<-rbind(data.frame(pred_DvF_0),data.frame(pred_DvF_1),data.frame(pred_DvF_2),data.frame(pred_DvF_3),data.frame(pred_DvF_4),data.frame(pred_DvF_5),data.frame(pred_DvF_6),data.frame(pred_DvF_7),data.frame(pred_DvF_8))
nrow(tot_hyp2)


# Hyp3 model predictions
model.hyp3<-load("rforest_hyp3.rda")

pred_DvF_0 = predict(get(model.hyp3), newdata = DvF_0, type = "prob")
pred_DvF_1 = predict(get(model.hyp3), newdata = DvF_1, type = "prob")
pred_DvF_2 = predict(get(model.hyp3), newdata = DvF_2, type = "prob")
pred_DvF_3 = predict(get(model.hyp3), newdata = DvF_3, type = "prob")
pred_DvF_4 = predict(get(model.hyp3), newdata = DvF_4, type = "prob")
pred_DvF_5 = predict(get(model.hyp3), newdata = DvF_5, type = "prob")
pred_DvF_6 = predict(get(model.hyp3), newdata = DvF_6, type = "prob")
pred_DvF_7 = predict(get(model.hyp3), newdata = DvF_7, type = "prob")
pred_DvF_8 = predict(get(model.hyp3), newdata = DvF_8, type = "prob")

tot_hyp3<-rbind(data.frame(pred_DvF_0),data.frame(pred_DvF_1),data.frame(pred_DvF_2),data.frame(pred_DvF_3),data.frame(pred_DvF_4),data.frame(pred_DvF_5),data.frame(pred_DvF_6),data.frame(pred_DvF_7),data.frame(pred_DvF_8))
nrow(tot_hyp3)


DvF_Ids<-read.table("Ids", header = TRUE)

tot<-data.frame(DvF_Ids, tot)
head(tot)
tot_hyp1<-data.frame(DvF_Ids, tot_hyp1)
tot_hyp2<-data.frame(DvF_Ids, tot_hyp2)
tot_hyp3<-data.frame(DvF_Ids, tot_hyp3)

write.csv(tot, file = "Default_pred_DvF")
write.csv(tot_hyp1, file = "Hyp1_pred_DvF")
write.csv(tot_hyp2, file = "Hyp2_pred_DvF")
write.csv(tot_hyp3, file = "Hyp3_pred_DvF")

################# Find consensus 'Match' predictions from all 4 models ################################

head(tot)
head(tot_hyp1)
head(tot_hyp2)
head(tot_hyp3)

mtot<-subset(data.frame(tot$drug_id.x.drug_id.y,tot$response), tot$response == "Match")
colnames(mtot)<-c("DvFID","response")
head(mtot)
nrow(mtot)
mtot_hyp1<-subset(data.frame(tot_hyp1$drug_id.x.drug_id.y,tot_hyp1$response), tot_hyp1$response == "Match")
colnames(mtot_hyp1)<-c("DvFID","response")
head(mtot_hyp1)
nrow(mtot_hyp1)

mtot_hyp2<-subset(data.frame(tot_hyp2$drug_id.x.drug_id.y,tot_hyp2$response), tot_hyp2$response == "Match")
colnames(mtot_hyp2)<-c("DvFID","response")
head(mtot_hyp2)
nrow(mtot_hyp2)

mtot_hyp3<-subset(data.frame(tot_hyp3$drug_id.x.drug_id.y,tot_hyp3$response), tot_hyp3$response == "Match")
colnames(mtot_hyp3)<-c("DvFID","response")
head(mtot_hyp3)
nrow(mtot_hyp3)

int1<-union(mtot,mtot_hyp1)
nrow(int1)

int2<-union(mtot_hyp2,mtot_hyp3)
nrow(int2)
head(int2)

final_int<-union(int1,int2)
nrow(final_int)

#write.csv(final_int, file = "Consensus_DvF")
write.csv(final_int, file = "Consenses_Matches")


####################### Merge annotations with predictions #############################################

food_order<-read.table(file = "Consenses_Matches", header = TRUE, sep = ',')
head(food_order)

annot<-read.table(file = "11k_FooDB", header = TRUE, sep = '\t')
head(annot)

drug_annot<-read.table(file = "Drugs_Master", header = TRUE, sep = '\t', check.names = "FALSE")
head(drug_annot)

nrow(food_order)
nrow(annot)
nrow(drug_annot)

merged<-merge(food_order, annot, by='FID')
head(merged)
nrow(merged)

merged1<-merge(merged, drug_annot, by='Drug_ID')
head(merged1)
nrow(merged1)

write.table(merged1, file = "DvF_drug_food_annot", sep = '\t', quote = FALSE, row.names = FALSE)

