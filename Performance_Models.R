library(mlr)
library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ROCR)

#################################### Load the models and test performance #########################################

############# Performance on independent test data (New data)
test.set.dist.desc<-data.frame(read.csv("data_dist_desc_test", header = TRUE, sep = ','))
head(test.set.dist.desc)
test.set<-subset(test.set.dist.desc, select = -c(1))

#remove same feature columns from test set also
test.set<-subset(test.set, select = c(2,365,6,1,364,181,180,9,3,179,4,5,7,157,17,191,363,229,298,156,210,341,56,58,115,330,163,316,53,151,375,187,186,378,237,125,321,167,202,242,252,10,194,124,133,123,240,297,308,119,335,11,45,146,323,132,116,299,25,328,170,322,18,21,370,340,362,201,209,193,300,114,8,355,90,353,225,317,139,205,313,57,138,169,309,314,126,52,68,315,224,80,152,306,71,172,310,66,347,164,247,352,131,243,121,59,162,98,129,336,41,171,257,165,264,320,236,26,223,168,301,39,20,198,307,296,143,351,188,128,63,141,16,50,304,371,354,51,200,356,147,305,359,195,348,49,22,230,113,325,62,214,331,311,122,127,274,24,155,117,319,75,259,235,232,161,231,30,241,46,367,204,339,189,107,312,360,108,183,48,27,142,15,208,250,47,12,54,379))

coln<-colnames(test.set)
head(coln)
write.csv(coln, file = "188_col_names.csv")
rm(bmr)
## Load default model
# 1. no tuning (default values: ntree=500, nodesize=1, mtry=13)
default<-load("rforest_default.rda");
head(default)

# 2. with hypertuning (ntree=70, nodesize=1, classwt = 8000,1)
model.nonode<-load("rforest_hyp1.rda")
head(model.nonode)

# 3. with hypertuning (ntree=95, nodesize=35, classwt = 26000,1)
model.hyp<-load("rforest_hyp3.rda")
head(model.hyp)

# 4. with hypertuning (ntree=85, nodesize=40)
model.nowt<-load("rforest_hyp2.rda")

Default = predict(rforest_dist_desc, newdata = test.set)
head(Default)
write.csv(Default, file="Default.csv") 

Hyp1 = predict(rforest_nonode, newdata = test.set)
write.csv(Hyp1, file="Hyp1.csv") 

Hyp2 = predict(rforest_nowt, newdata = test.set)
write.csv(Hyp2, file="Hyp2.csv") 

Hyp3 = predict(rforest_highcost, newdata = test.set)
write.csv(Hyp3, file="Hyp3.csv") 

mlr::performance(Default, measures = list(mcc,tpr,fpr,fnr,tnr,ppv,acc,bac,mmce,kappa,auc))
mlr::performance(Hyp3, measures = list(mcc,tpr,fpr,fnr,tnr,ppv,acc,bac,mmce,kappa,auc))
mlr::performance(Hyp1, measures = list(mcc,tpr,fpr,fnr,tnr,ppv,acc,bac,mmce,kappa,auc))
mlr::performance(Hyp2, measures = list(mcc,tpr,fpr,fnr,tnr,ppv,acc,bac,mmce,kappa,auc))


## Plotting performance of test set
df = generateThreshVsPerfData(list(Default = Default, Hyp1 = Hyp1, Hyp2 = Hyp2, Hyp3 = Hyp3),measures = list(ppv,tpr,fpr))

#Precision/recall curve
plotROCCurves(df, measures = list(tpr,ppv), diagonal = FALSE)

#ROC curve
plotROCCurves(df, measures = list(fpr,tpr))

