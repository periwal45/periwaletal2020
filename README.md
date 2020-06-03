# periwaletal2020
periwaletal2020 source code

This repository contains scripts and data for manuscript periwaletal2020

1. Data_preprocessing.R

contains code to pre-process data as is described under 'Data preprocessing' section of manuscript

2. FeatureSelection.R and FV_filter_values.csv

contains code to run feature importance as is described under 'Feature selection' section of manuscript. FV_filter_values contains the list of columns with their feature importance values

3. Benchmark.R

initial classification runs using ksvm, nnet, and random forest to assess their performance. Refer to section 'Random Forest classification' of the manuscript

4. RandomForest_train_default.R

contains code for running the classifier with default settings. See manuscript section 'Random Forest classification'

5. RandomForest_train_hyp1.R 

contains code for running the classifier with hyper-parameter settings 'hyp1'. See manuscript section 'Random Forest classification'

6. RandomForest_train_hyp2.R

contains code for running the classifier with hyper-parameter settings 'hyp2'. See manuscript section 'Random Forest classification'

7. RandomForest_train_hyp3.R

contains code for running the classifier with hyper-parameter settings 'hyp3'. See manuscript section 'Random Forest classification'

8. Performance_Models.R

This code compares the performance of all the trained models using the 20% test set. Performance was assessed using various statistical measures as described in the manuscript.
