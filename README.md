# Chronic Fatigue Syndrome (CFS) Predictor Project

## Overview
This project investigates predictors of Chronic Fatigue Syndrome (CFS) using health survey data. It applies logistic regression models (GLMs) to identify significant risk factors associated with CFS diagnosis.

## Technologies Used
- R
- caret, pROC, ggplot2, corrplot, glmnet, broom, car

## Highlights
- Explored clinical and demographic predictors such as age, gender, mental health, and comorbidities.
- Built a logistic regression model to predict CFS status.
- Achieved ROC AUC of 0.842 indicating good model discrimination.
- Visualized model performance with ROC curves and feature importance plots.

## How to Run
1. Install required R packages:
    ```R
    install.packages(c("caret", "pROC", "ggplot2", "corrplot", "glmnet", "broom", "car"))
    ```
2. Open `cfs_predictor_analysis.R`
3. Run the script manually.

## Data Source
- [Health Survey Dataset on Kaggle](https://www.kaggle.com/datasets/aradhanahirapara/healthcare-survey)  

## Project Report
A full report explaining methods, results, and interpretation is available here:
[CFS_Predictor_Report.pdf](./CFS_Predictor_Report.pdf)


## Notes
- Full dataset is referenced but not uploaded due to size constraints.
