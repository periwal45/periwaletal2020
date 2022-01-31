args<-commandArgs(TRUE)
# Building a random forest model
library(mlr)
library(readr)
library(dplyr)
library(dataPreparation) #filter out useless variables
library(parallelMap)
library(parallel)

##################################################################
set.seed(44)

##load train set
data_train<-data.frame(read.table(args[1], header = TRUE, sep = ','))
print("Train set loaded")
print(args[1])
#data<-data.frame(read.csv("train_dist_desc", header = TRUE, sep = ','))

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

train.set<-data.frame(subset(data_dist_desc, select = -c(1)))

## Create classification task
train_task_dist_desc = makeClassifTask(id = "rf", data = train.set, target = "Outcome", positive = "Match")
print("Task description")
train_task_dist_desc

## Constructing a learner (RandomForest)

rf.prob <- makeLearner("classif.randomForest", predict.type = "prob",
                       par.vals = list(importance = TRUE))

##view tunable hyperparameters
#rf.prob$par.set

## handling class imbalance by introducing class weight using weighted wrapper
## single value denotes weight of positive class, negative class receives weight of 1

wcw.rf.prob<-makeWeightedClassesWrapper(rf.prob, wcw.param = "classwt")

## Optimization and Training (model building) 

## set tunable parameters

## 1. grid search to find hyperparameters (RandomForest)
## number of variables to split at each node (mtry), default (i.e. mtry= √p, where p is the number of features in the input data). 
## In our ML dataset, the default mtry is (√378) 19 features
## default number of trees = 500
## default nodesize = 1
rf_param <- makeParamSet(
  makeIntegerParam("ntree",lower = 100, upper = 300),
  makeIntegerParam("nodesize", lower = 20, upper = 50),
  makeIntegerParam("mtry", lower = 15, upper = 30),
  makeIntegerParam("classwt", lower = 300, upper = 3000)
)

## 2. random search for 10 iterations, also enable tuning probability threshold
rancontrol <- makeTuneControlRandom(maxit = 10, tune.threshold = FALSE)

## 3. resampling strategy: set 5 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 5, predict="both", stratify=TRUE)

## Setting parallel backend for hypertuning
parallelStartMulticore(cpus = (detectCores()-1), show.info = T)

## 4. Performing the hypertuning
rf_tune_dist_desc <- tuneParams(learner = wcw.rf.prob, resampling = set_cv, 
                                task = train_task_dist_desc, par.set = rf_param, 
                                control = rancontrol, 
                                measures = list(f1,mcc,tpr,fnr,fpr,tnr,acc,mmce,kappa,ppv,auc)) 

## print tuning result
print(as.data.frame(rf_tune_dist_desc$opt.path))
parallelStop()

## 5. Using hyperparameters for modeling
rf.tree_dist_desc <- setHyperPars(wcw.rf.prob, par.vals = rf_tune_dist_desc$x)

## 6. train a model
wt_rforest <- train(rf.tree_dist_desc, train_task_dist_desc)
#getLearnerModel(wt_rforest)

## save the models !
save(wt_rforest, file = paste0(args[1],"_wt_rforestT.rda"))

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

## 7. Make predictions
preds <- predict(wt_rforest, newdata = test.data)
write.csv(as.data.frame(preds), file=paste0(args[2],"_predsT.csv"))

#save prediction object
save(preds, file=paste0(args[2],"_predictionsT.rda"))

perf<-mlr::performance(preds, measures = list(mcc,f1,tpr,fpr,fnr,tnr,ppv,acc,bac,mmce,kappa,auc))
print("performance on test set")
perf
















