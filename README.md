# Project Title

### Main Goal: 

### Project Description: 

### How to Use Your Project:

***Tab Delayed Testing - Descriptive*** *(Kate Kim)* 

*Use: Compare and contrast delay in testing since symptom onset for selected groups within each variable of Age, Contact Source, Travel History, Sex, and Pregancy Status.*

Conducted descriptive analyses of *Delay in Testing* using density curves by calculating delay of testing since symptom onset for different categories for the following variables: *Age, Previous Travel, Contact Source, Sex, and Pregnancy Steus*. The respective averages are displayed by mean lines overlayed onto density curves. An overall density curve of delayed testing in the first tab was also created for reference. Under each variable tab, a boxplot was created for delayed testing in each variable.


***Tab Delayed Testing - MLR*** *(Kate Kim)* 

*Use: A reactive calculator to predict the expected number of days to take a COVID-19 test for selected predictors of Age (Category), Contact Source, and Travel History based on a fitted Multiple Linear Regression (MLR).*

A Multiple Linear Regression model was fitted for *delayed testing* as the dependent variable, and *Age (Category), Contact Source*, and *Travel History* as the predictor variables. Based on the MLR model, the Calculator section calculates the predicted number of days for an individual to take a test after symptoms onset based on selected features for the predictor variables. Underneath the calculator, there is a summary table for the Multiple Linear Regression, codebook for reference variables, interpretations and effect plots of each variable. 





#### Variables Used: 

*Delayed Testing*: Calculated by the number of days since symptom onset to an individual's first positive result test. Negative days of delayed testing can be explained if the individual was tested before symptom onset due to a known outbreak or contact, or vulnerable settings, such as nursing homes.

#### System/Library Requirements:

Libraries: 
- library(shiny)
- library(ggplot2)
- library(dplyr)
- library(sjPlot)
- library(sjmisc)
- library(sjlabelled)
- library(ggmosaic)
- library(tidyverse)
- library(lubridate)
- library(ggrepel)
- library(car)
- library(jtools)
- library(magrittr)
- library(ggsci)
- library(MASS) 
- library(plotly) 


#### Credits: 
Kate Kim, MPH Candidate '21, Johns Hopkins University, Bloomberg School of Public Health ([https://github.com/ks00jinkim](url)) 



