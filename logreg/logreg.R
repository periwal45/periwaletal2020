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

## L1R
#outcome column as factor
datar<-train.set[,ncol(train.set)]
# use class weights for data imbalance
wt <- 1-(table(datar)/length(datar))
print("class weights for imbalanced data")
wt

#predictors as matrix
datap<-as.matrix(train.set[, -c(1,ncol(train.set))])
#head(datap)
print("dimensions of matrix datap")
dim(datap)

#remove columns with all 0's (these columns aren't picked by which_are_constant)
zero<-datap[,apply(datap == 0, 2, all)]
#head(zero) #remove these columns from test set later
colnames(zero)
grep("f1_topological.khs.sssB.x",colnames(datap)) # col 70
grep("f1_topological.khs.sssB.y",colnames(datap)) # col 254

datap<-datap[,!apply(datap == 0, 2, all)]
print("after removing all 0's dimesnions of datap")
dim(datap)

# # Center and scale data
#meantr <- apply(datap, 2, mean)
#sdtr <- apply(datap, 2, sd)
#strain<-scale(datap,center=meantr,scale=sdtr)
#print("scaled data dimensions")
#dim(strain)

# tune cost 
# first get some orientation using heuristicC, better to use balanced sample for this
heurInd <- createDataPartition(datar, p = .5, list = FALSE, times = 1)
heurInd <- c(heurInd[datar[heurInd]=="Match"], heurInd[sample(which(datar[datar[heurInd]]=="Nomatch"), sum(datar[heurInd]=="Match"))])
co <- heuristicC(datap[heurInd,])
print("C heuristics obtained from  balanced dataset")
co

# make l1r
## Create classification task
train_task_dist_desc = makeClassifTask(id = "l1r", data = train.set, target = "Outcome", positive = "Match")
print("Task description")
train_task_dist_desc
l1r<-makeLearner("classif.LiblineaRL1LogReg",predict.type = "prob", wi = wt, cost = co)
print("L1R learner")
l1r

l1r_model<-mlr::train(l1r,train_task_dist_desc)
save(l1r_model, file = paste0(args[1],"l1r.rda"))

# make l2r
## Create classification task
train_task_dist_desc = makeClassifTask(id = "l2r", data = train.set, target = "Outcome", positive = "Match")
print("Task description")
train_task_dist_desc
l2r<-makeLearner("classif.LiblineaRL2LogReg",predict.type = "prob", wi = wt, cost = co)
print("L2R learner")
l2r

l2r_model<-mlr::train(l2r,train_task_dist_desc)
save(l2r_model, file = paste0(args[1],"l2r.rda"))

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

#test.data<-valid
#dim(test.data)
print("Test set dimensions")
dim(test.data)

# Make prediction
preds_l1r <- predict(l1r_model, newdata = test.data)
save(preds_l1r, file = paste0(args[2],"l1r_pred.rda"))

perf<-mlr::performance(preds_l1r, measures = list(f1,mcc,tpr,fpr,fnr,tnr,ppv,acc,bac,mmce,kappa,auc))
print("L1R: performance on test set")
perf


#l2r_p=predict(l2r,newx=stest,proba=TRUE,decisionValues=TRUE,se.fit=TRUE)
preds_l2r <- predict(l2r_model, newdata = test.data)
save(preds_l2r, file = paste0(args[2],"l2r_pred.rda"))

perf<-mlr::performance(preds_l2r, measures = list(f1,mcc,tpr,fpr,fnr,tnr,ppv,acc,bac,mmce,kappa,auc))
print("L2R: performance on test set")
perf

