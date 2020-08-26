library(mlr)
library(readr)
library(dplyr)
	 
################################################################################################################

train.set<-data.frame(read.csv("data_dist_desc_train", header = TRUE, sep = ','))
train.set<-subset(train.set, select = -c(1))

## select columns from feature importance step (taken from feature selection file)

train.set<-subset(train.set, select = c(2,365,6,1,364,181,180,9,3,179,4,5,7,157,17,191,363,229,298,156,210,341,56,58,115,330,163,316,53,151,375,187,186,378,237,125,321,167,202,242,252,10,194,124,133,123,240,297,308,119,335,11,45,146,323,132,116,299,25,328,170,322,18,21,370,340,362,201,209,193,300,114,8,355,90,353,225,317,139,205,313,57,138,169,309,314,126,52,68,315,224,80,152,306,71,172,310,66,347,164,247,352,131,243,121,59,162,98,129,336,41,171,257,165,264,320,236,26,223,168,301,39,20,198,307,296,143,351,188,128,63,141,16,50,304,371,354,51,200,356,147,305,359,195,348,49,22,230,113,325,62,214,331,311,122,127,274,24,155,117,319,75,259,235,232,161,231,30,241,46,367,204,339,189,107,312,360,108,183,48,27,142,15,208,250,47,12,54,379))

# try different classifiers
lrns = list(makeLearner("classif.ksvm"), makeLearner("classif.nnet"), makeLearner("classif.randomForest"))
rdesc = makeResampleDesc("CV", iters=3, stratify = TRUE)

## Create classification task
train_task = makeClassifTask(id = "Benchmark", data = train.set, target = "Outcome")
train_task

############################# 2. Benchmark experiment #####################################
bmr <- benchmark(lrns,train_task,rdesc)
 
# save the model
save(bmr, file = "bmr.rda")


