#Extra code for Fig 1G
load("train_dist_desc_set1_wt_rforestT.rda") # the model object is 'wt_rforest'
print("trained random forest model")
wt_rforest

#extract features
feat_importance1<-getFeatureImportance(wt_rforest, type = 1)

load("train_dist_desc_set2_wt_rforestT.rda") # the model object is 'wt_rforest'
print("trained random forest model")
wt_rforest

#extract features
feat_importance2<-getFeatureImportance(wt_rforest, type = 1)

load("train_dist_desc_set3_wt_rforestT.rda") # the model object is 'wt_rforest'
print("trained random forest model")
wt_rforest

#extract features
feat_importance3<-getFeatureImportance(wt_rforest, type = 1)

load("train_dist_desc_set4_wt_rforestT.rda") # the model object is 'wt_rforest'
print("trained random forest model")
wt_rforest

#extract features
feat_importance4<-getFeatureImportance(wt_rforest, type = 1)

load("train_dist_desc_set5_wt_rforestT.rda") # the model object is 'wt_rforest'
print("trained random forest model")
wt_rforest

#extract features
feat_importance5<-getFeatureImportance(wt_rforest, type = 1)


feat_result1<-data.frame(feat_importance1$res)
feat_result1$variable <- gsub('f1_', '', feat_result1$variable)
View(feat_result1)
feat_result1$set<-"set1"
names(feat_result1)[names(feat_result1) == "variable"] <- "FP"

feat_result2<-data.frame(feat_importance2$res)
feat_result2$variable <- gsub('f1_', '', feat_result2$variable)
View(feat_result2)
feat_result2$set<-"set2"
names(feat_result2)[names(feat_result2) == "variable"] <- "FP"

feat_result3<-data.frame(feat_importance3$res)
feat_result3$variable <- gsub('f1_', '', feat_result3$variable)
View(feat_result3)
feat_result3$set<-"set3"
names(feat_result3)[names(feat_result3) == "variable"] <- "FP"

feat_result4<-data.frame(feat_importance4$res)
feat_result4$variable <- gsub('f1_', '', feat_result4$variable)
View(feat_result4)
feat_result4$set<-"set4"
names(feat_result4)[names(feat_result4) == "variable"] <- "FP"

feat_result5<-data.frame(feat_importance5$res)
feat_result5$variable <- gsub('f1_', '', feat_result5$variable)
View(feat_result5)
feat_result5$set<-"set5"
names(feat_result5)[names(feat_result5) == "variable"] <- "FP"


feat_result<-rbind(feat_result1,feat_result2,feat_result3,feat_result4,feat_result5)
feat_result %>% melt()

write.table(feat_result, file = "feat_importance", sep = '\t', row.names = FALSE)
