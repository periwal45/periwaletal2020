# Building a random forest model
#options(java.parameters = "-Xmx12000m")
library(mlr)
library(readr)
library(dplyr)
library(FSelector) #important variables
library(arsenal) #for compare function
library(dataPreparation) #filter out useless variables
library(parallelMap)
library(parallel)
	 
################################################################################################################

train.set<-data.frame(read.csv("data_dist_desc_train", header = TRUE, sep = ','))
train.set<-subset(train.set, select = -c(1))


##Selecting importannt features

train.set<-subset(train.set, select = c(2,365,6,1,364,181,180,9,3,179,4,5,7,157,17,191,363,229,298,156,210,341,56,58,115,330,163,316,53,151,375,187,186,378,237,125,321,167,202,242,252,10,194,124,133,123,240,297,308,119,335,11,45,146,323,132,116,299,25,328,170,322,18,21,370,340,362,201,209,193,300,114,8,355,90,353,225,317,139,205,313,57,138,169,309,314,126,52,68,315,224,80,152,306,71,172,310,66,347,164,247,352,131,243,121,59,162,98,129,336,41,171,257,165,264,320,236,26,223,168,301,39,20,198,307,296,143,351,188,128,63,141,16,50,304,371,354,51,200,356,147,305,359,195,348,49,22,230,113,325,62,214,331,311,122,127,274,24,155,117,319,75,259,235,232,161,231,30,241,46,367,204,339,189,107,312,360,108,183,48,27,142,15,208,250,47,12,54,379))

## Create classification task
train_task_dist_desc = makeClassifTask(id = "Benchmark_dist_desc", data = train.set, target = "Outcome")
train_task_dist_desc

############################# 2. Constructing a learner (RandomForest) ######################################

rf.prob <- makeLearner("classif.randomForest", predict.type = "prob")
rf.prob
rf.prob <- makeWeightedClassesWrapper(rf.prob, wcw.param="classwt")
rf.prob

############################# 3. Optimization and Training (model building) #####################################

# set tunable parameters

# 1. Specifying the search space of learner (RandomForest)
rf_param <- makeParamSet(
   makeIntegerParam("ntree",lower = 50, upper = 151),
    makeDiscreteParam("wcw.weight", seq(1000,20000,1000))
)


# 2. Specify optimization algorithm for search space: random search for 5 iterations
rancontrol <- makeTuneControlRandom(maxit = 5L)
# 3. Performing the tuning: set 3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L, stratify=TRUE)
 
# Setting parallel backend for hypertuning
if(Sys.info()['sysname'] == "Linux") {
   parallelStartMulticore(cpus = 5, show.info = T)
} else parallelStartSocket(cpus = 5, show.info = T)

# 4. Performing the hypertuning
rf_tune_dist_desc <- tuneParams(learner = rf.prob, resampling = set_cv, task = train_task_dist_desc, par.set = rf_param, control = rancontrol, measures = list(mcc,tpr,fnr,fpr,tnr,acc,mmce,kappa)) 

# 5. Using hyperparameters for modeling
rf.tree_dist_desc <- setHyperPars(rf.prob, par.vals = rf_tune_dist_desc$x)

# train a model
rforest_nonode <- train(rf.tree_dist_desc, train_task_dist_desc)
getLearnerModel(rforest_nonode)
 
# save the model
save(rforest_nonode, file = "rforest_hyp1.rda")





			  







