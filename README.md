# periwaletal2020
### Author: Vinita Periwal
periwaletal2020 source code

link to input data files: Periwal, Vinita (2020), “periwaletal drug-food similarity ML data”, Mendeley Data, V1, [doi: 10.17632/7ft539gwf3.1]

This repository contains scripts for manuscript periwaletal2020

1. Data_preprocessing.R

contains code to pre-process data as is described under 'Data preprocessing' section of manuscript.

- filters constant variables and removes NAs
- train and test split
- checks overlaps

2. FeatureSelection.R and FV_filter_values.csv

contains code to run feature importance as is described under 'Feature selection' section of manuscript. FV_filter_values contains the list of columns with their feature importance values

3. randomforest.R

contains code for running the random forest classifier with default and all hyper-parameter settings. The code utilizes parallel computing in cluster environment. See manuscript section 'Random Forest classification'

- default model
- hyp1
- hyp2
- hyp3

4. Performance_Models.R

This code compares the performance of all the trained models using the 20% test set. Performance was assessed using various statistical measures as described in the manuscript.

5. FooDB_predictions.R 

virtually screens the foodb compounds against the trained models and generates predictions. The FooDB dataset was split into smaller sized files.

- virtual screening
- consensus matching
- adding annotations

6. Figures folder

The folder contains scripts and underlying data to generate the manuscript main and supplementary figures

