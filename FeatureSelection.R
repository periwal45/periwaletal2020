options(java.parameters = "-Xmx12000m")
library(mlr)
library(readr)
library(dplyr)

############################## 1. Load data and define a learning task #######################################################

## Directly load train and test from disk (files generated using DataProcessing.R)
train.set<-data.frame(read.csv("data_dist_desc_train", header = TRUE, sep = ','))
train.set<-subset(train.set, select = -c(1))

## Create classification task
train_task_dist_desc = makeClassifTask(id = "Benchmark_dist_desc", data = train.set, target = "Outcome")
train_task_dist_desc


fv = generateFilterValuesData(train_task_dist_desc, method = "randomForest.importance")

write.csv(fv$data, file = "FV_filter_values.csv")


