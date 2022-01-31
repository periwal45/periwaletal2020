args<-commandArgs(TRUE)
setwd("/g/scb/patil/periwal/ML/revision_ML/ML_revision_2/")
library(mlr)
library(readr)
library(dplyr)
library(LiblineaR)
library(dataPreparation) #filter out useless variables
library(parallelMap)
library(parallel)
library(caret)

################################################################################################################
set.seed(44)

##load train set
data_train<-data.frame(read.table(args[1], header = TRUE, sep = ','))
print("Train set loaded")
print(args[1])

### Filter constant variables
constant_cols <- whichAreConstant(data_train)
constant_cols

train_sub<-subset(data_train, select = -c(constant_cols))
print("dimensions of data after removing constant columns")
dim(train_sub)

## remove rows or pairs with any NA's
data_dist_desc<-na.omit(train_sub)
print("dimensions of data after removing rows with NAs")
dim(data_dist_desc)

## remove Inf values
data_dist_desc <- data_dist_desc %>% filter_all(all_vars(!is.infinite(.)))
print("removed infinite values")
dim(data_dist_desc)

train.set<-data.frame(subset(data_dist_desc, select = -c(1)))

#head(datap)
#print("dimensions of matrix datap")
#dim(datap)

#remove columns with all 0's (these columns aren't picked by which_are_constant)
zero<-train.set[,apply(train.set == 0, 2, all)]
#head(zero) #remove these columns from test set later
colnames(zero)
grep("f1_topological.khs.sssB.x",colnames(train.set)) # col 70
grep("f1_topological.khs.sssB.y",colnames(train.set)) # col 254

train.set<-train.set[,!apply(train.set == 0, 2, all)]

# make naive bayes learner
bayes<-makeLearner("classif.naiveBayes", predict.type = "prob")
print("Naive learner")
bayes

train_task = makeClassifTask(id = "bayes", data = train.set, target = "Outcome", positive = "Match")

## train a model
bayes_model <- mlr::train(bayes, train_task)

## save the models !
save(bayes_model, file = paste0(args[1],"bayes.rda"))

#### Testing
#load test set
test.data<-data.frame(read.table(args[2], header = TRUE, sep = ','))
dim(test.data)
print("Test set loaded")
print(args[2])

test.data<-data.frame(subset(test.data, select = -c(constant_cols)))
print("dimensions of test data")
dim(test.data)

# remove all NA's
test.data<-na.omit(test.data)
dim(test.data)

write.csv(test.data, file = paste0(args[2],"test_set_after_const_cols.csv"))

## 7. Make predictions
preds <- predict(bayes_model, newdata = test.data)
write.csv(as.data.frame(preds), file=paste0(args[2],"predictions_bayes.csv"))

#save prediction object
save(preds, file=paste0(args[2],"predictobject_bayes.rda"))

perf<-mlr::performance(preds, measures = list(f1,mcc,tpr,fpr,fnr,tnr,ppv,acc,bac,mmce,kappa,auc))
print("performance on test set")
perf







