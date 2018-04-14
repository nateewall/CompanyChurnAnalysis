# Predicting Company Attrition using HR Data

### Purpose
The goal of this analysis is to utilize various machine learning & data science techniques to better understand features contributing to the company's attrition. We will be using a data set with 1470 observations and 35 variables maintained by the companies HR department.

### Source Data
Our data has details on 1470 different employees including whether those employees left the company or not. Some of the variables include things like salary, employee satisfaction, time with the company, and many other potential features to be considered in our model. The entire data set is in CustomerAttritionData.csv. For additional details on any one of the variables please consult the HR department.

### Code
As part of this analysis there are two R programs in this repository.

CaseStudy2App.R: R Shiny application provides the user the ability to interactively explore the data to find any interesting patterns in the underlying data. Additionally, it provides a convenient method to assess the model's performance and inspect the data contributing to the model's prediction.

PredictingAttrition.Rmd: R Markdown file outlining the methodology used for training the algorithm. Additionally, it includes different metrics and graphs used to assess the model.

### References

R Packages used: Shiny , RCurl, tidyquant, caret, lime, ROSE, ggplot2


