# FILM

<!-- badges: start -->
<!-- badges: end -->

## How to install FILM package

``` r
# If you don't have devtools package
install.packages("devtools")
library(devtools)

# Install FILM package 
install_github("antoniogt/FILM")
library(FILM)
```

You need the following R packages:

readr
data.table
ggplot2
stats
DataExplorer
MLmetrics
DMwR
ROSE
dplyr
caret

## Uses of FILM

**FILM** is an R package that performs (1) a new ensemble-based technique to deal with class imbalance for a binary classification problem which ensures that during model training all samples of the minority class have been used at least once, (2) the computation of a new metric based on an integration of a set of basic evaluation metrics from the measurement set {Accuracy, Sensitivity, Specificity, Positive Predictive Value, Negative Predictive Value, Cohen's Kappa, Balanced Accuracy, F1-Score, Area under the ROC curve, Area under the Precision-Recall curve, Mathews Correlation Coefficient, geometric mean}, so that weights are associated to each metric according to the bias they have with the minority class proportion of a particular dataset, (3) a way to represent the agreements, disagreements and win ratios of a set of models trained on a dataset in a simple way, and (4) a way to implement all of the above in a simple way using a single R function.

## Available tutorials



## About

This package has developed by $$Antonio Guillén Teruel$$ as part of his PhD mentored by professor $$Juan Antonio Botía Blaya$$.

Authors: Antonio Guillén Teruel, Marcos Caracena, Jose A. Pardo, Fernando de-la-Gándara, José Palma, Juan A. Botía
