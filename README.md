# Prediction of GitHub Stars (UBC STAT 301: Statistical Modelling for Data Science)

Explored predictors associated with the number of stars a GitHub repository receives, employing linear regression and model diagnostics, feature selection, and data analysis techniques in R within the UBC STAT 301 course. 


## Problem Statement <!--- do not change this line -->

GitHub is the leading social coding platform where the popularity and quality of the users' repositories serve as indicators of their capacity, skills, and experiences. Predicting its popularity, measured by the number of stars, is crucial for understanding trends in repository engagement and the importance of enabling Github's tools. 


## Key Results <!--- do not change this line -->

1. Built a regression model using stepwise selection algorithms, both forward and backward, to identify significant predictors of repository popularity. 
2. Among the features in the dataset, both selection methods selected the number of forks and issues, along with enabling GitHub's projects tool, discussions, pages, and having downloadable files as variables significantly associated with the number of stars.
3. Proposed solutions for addressing violations in linear regression modelling including feature engineering, inclusion of interaction terms, and non-linear transformations. 

## Methodologies <!--- do not change this line -->

To accomplish this, the data was cleaned and wrangled by filtering out irrelevant features and handling missing data. Visualizations such as boxplots, pairplots, and a heatmap were used to study the relationship between features and provide insight into potential multicollineairty issues and outliers. The data was then randomly split into a train and test set on a 70-30% basis such that feature selection can be performed on the training set while prediction can be made on the test set. Employing both forward and backward selection methods, the combination of features that minimized Mallow's Cp statistic were chosen. Multiple regression models were then fit and diagnoisitcs were run to validate the linear regression assumptions. Lastly, the reduced models' out-of-sample prediction performance were assessed using the Root Mean Squared Error (RMSE), where the smaller the RMSE, the better the model and its predictions.

## Data Sources <!--- do not change this line -->

Kaggle Dataset: https://www.kaggle.com/datasets/donbarbos/github-repos/data

## Technologies Used <!--- do not change this line -->

- car
- broom
- dplyr
- GGally
- leaps
- mltools
- R
- tidyverse
