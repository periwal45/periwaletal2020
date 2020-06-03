library(mlr)
library(readr)
library(dplyr)
library(FSelector) #important variables
library(arsenal) #for compare function
library(dataPreparation) #filter out useless variables
library(ggplot2)
library(ggfortify)

########################################### Read data for modeling ##############################################
data<-read.table("1410_Drugs_ML", header = TRUE, sep = ',')
head(data)
dim(data)


############################################# 1. Data pre-processing #############################################

# Filter useless variables
constant_cols <- whichAreConstant(data)
constant_cols

data_sub<-subset(data, select = -c(constant_cols))
nrow(data_sub)
ncol(data_sub)
dim(data_sub)

## remove rows or pairs with any NA's
data_dist_desc<-na.omit(data_sub)
dim(data_dist_desc)

############################################# 2. Train and Test split ###########################################

## split data into train and test set

train_index<-sample(nrow(data_dist_desc), size = 0.8 * nrow(data_dist_desc))
train.set<-data.frame(data_dist_desc[train_index,])
dim(train.set)

test.set<-data.frame(data_dist_desc[-train_index,])
dim(test.set)

write.csv(train.set, "data_dist_desc_train", row.names = F)
write.csv(test.set, "data_dist_desc_test", row.names = F)


########################## check for no overlap in training and test set (optional) #########################################

compare(train.set, test.set, by = "Final_dataset.drug_id")


