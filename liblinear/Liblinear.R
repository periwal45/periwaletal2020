library(LiblineaR)
library(tidyr)
library(ggplot2)
library(dataPreparation)
library(confusionMatrix)
library(caret)
library(mlr)
set.seed(44)

#load train set
data<-read.table("train_dist_desc", sep = ",", header = TRUE, stringsAsFactors = TRUE)
dim(data)
#head(data)

### Filter constant variables
constant_cols <- which_are_constant(data, verbose = FALSE)
#constant_cols

train.data<-data.frame(subset(data, select = -c(constant_cols)))
print("dimensions of data")
dim(train.data)

# remove all NA's
train.data<-na.omit(train.data)
dim(train.data)

#outcome column as factor
datar<-train.data[,ncol(train.data)]
#head(datar)

#predictors as matrix
datap<-as.matrix(train.data[, -c(1,ncol(train.data))])
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

# Center and scale data
meantr <- apply(datap, 2, mean)
sdtr <- apply(datap, 2, sd)
strain<-scale(datap,center=meantr,scale=sdtr)
dim(strain)

# type
t0<-0 #L2R
t6<-6 #L1R

# tune cost 
# first get some orientation using heuristicC, better to use balanced sample for this
heurInd <- createDataPartition(datar, p = .5, list = FALSE, times = 1)
heurInd <- c(heurInd[datar[heurInd]=="Match"], heurInd[sample(which(datar[datar[heurInd]]=="Nomatch"), sum(datar[heurInd]=="Match"))])
co <- heuristicC(datap[heurInd,])
print("C heuristics obtained from  balanced dataset")
co

# use class weights for data imbalance
wt <- 1-(table(datar)/length(datar))
wt

# Train models with type.
l2r<-LiblineaR(data=strain,target=datar,type=t0,wi=wt,cost=co,verbose=FALSE)
l2r
l1r<-LiblineaR(data=strain,target=datar,type=t6,wi=wt,cost=co,verbose=FALSE)
l1r

save(l1r, file = "L1_libl.rda")
save(l2r, file = "L2_libl.rda")

# load test data
test.data<-read.table("test_dist_desc", sep = ",", header = TRUE, stringsAsFactors = TRUE)
dim(test.data)

test.data<-data.frame(subset(test.data, select = -c(constant_cols)))
print("dimensions of test data")
dim(test.data)

# remove all NA's
test.data<-na.omit(test.data)
dim(test.data)

#outcome column as factor
test.datar<-test.data[,ncol(test.data)]
#head(test.datar)
nrow(data.frame(test.datar))

#predictors as matrix
test.datap<-data.matrix(test.data[, -c(1,ncol(test.data))])
#head(test.datap)
dim(test.datap)

#additionally remove two columns with all zeros
test.datap<-test.datap[, -c(70,254)]
dim(test.datap)

#scale test data as train data (this will fail if columns don't match between train and test)
stest<-data.matrix(scale(test.datap,attr(strain,"scaled:center"),attr(strain,"scaled:scale")))
dim(stest)
nrow(data.frame(stest))
# Make prediction
l1r_p=predict(l1r,newx=stest,proba=TRUE,decisionValues=TRUE,se.fit=TRUE)
save(l1r_p, file = "l1r_pred.rda")

l2r_p=predict(l2r,newx=stest,proba=TRUE,decisionValues=TRUE,se.fit=TRUE)
save(l2r_p, file = "l2r_pred.rda")

# Display confusion matrix
l1r_res=table(l1r_p$predictions,test.datar)
print("confusion matrix L1R")
print(l1r_res)

l2r_res=table(l2r_p$predictions,test.datar)
print("confusion matrix L2R")
print(l2r_res)

# compute metrics
metrics_l1r<-data.frame(calc_stats(l1r_res, positive = "Match"))
metrics_l1r
metrics_l2r<-data.frame(calc_stats(l2r_res, positive = "Match"))
metrics_l2r
