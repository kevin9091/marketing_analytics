## Data Source
https://www.kaggle.com/jackdaoud/marketing-data 

## Problem Statement
To identify people who would respond to ad campaigns(Not necessarily first or last)

## Insights 
To see insights, check out marketing_eda.md

## Solution
Define a target variable Response as 1 if any of the accepted campaign or response variables are 1. We will try to predict whether response is 1 or 0 using customer attributes like age, income, education, etc. We must not use Amount spent or Purchases information as it comes after response to ad campaign (Ad campaign is the cause and Purchase and expenses are effects). As it is a classification task, we will use logistic regression and tree-based methods to classify response as 1 or 0.

Modelling analyses are in log_model.ipynb and non_linear_models.ipynb


