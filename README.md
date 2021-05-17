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

- *Delayed Testing*: Calculated by the number of days since symptom onset to an individual's first positive result test. Negative days of delayed testing can be explained if the individual was tested before symptom onset due to a known outbreak or contact, or vulnerable settings, such as nursing homes.

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



Disclaimer
The following tool was developed for a class project. Please be aware that the predictions calculated by this tool rely soley on the available regressors and DO NOT NECESSARILY IMPLY CAUSATION by said regressors. The models are not validated and should not be used for any decision making. The data might differ from official records because a subset of participants were used.


The district of Calw
General information about the district
Calw is a district (german <Landkreis>) centrally located in Baden Wuerttemberg , Germany. The district is approximately 800 square kilometers, with approximately 160.000 people living in the 25 cities and municipalities of the district. These consist of 10 cities - Calw city, Altensteig, Bad Herrenalb, Bad Liebenzell, Bad Teinach-Zavelstein, Bad Wildbad, Haiterbach, Nagold, Neubulach and Wildberg - and 15 municipalities - Althengstett, Dobel, Ebhausen, Egenhausen, Enzkloesterle, Gechingen, Hoefen, Neuweiler, Oberreichenbach, Ostelsheim, Rohrdorf, Schoemberg, Simmersfeld, Simmozheim und Unterreichenbach.

The health department
The health department is part of the district office, which is on the one hand a municipal self-governing authority and on the other hand the lower state administrative authority with diverse tasks. For more information please see the official homepage of the district office: lra-calw.de

The health department and the COVID-19 pandemic
With the corona pandemic, a completely new structure was set up within the health department. Depending on the incidence, up to 80 people were / are employed full- time and part-time coping with the pandemic. The military <KSK Kommando Calw> also supports the tracking of contact persons with up to 20 soldiers. Early on at the beginning of the second wave, the Calw District Office decided to digitize the processes and use the Sormas software (see data source)

The Data used in this App
Data source
The data used here is data from SORMAS (Surveillance, Outbreak Response Management and Analysis System) used by the local health department of the district of Calw. SORMAS is an e-health software developed by the Helmholtz Center for Infection Research and the German Center for Infection Research for the management of measures to combat epidemics. With the support of the federal and state governments, the software is made available to the health authorities free of charge. The program is intended to enable data to be exchanged between the various bodies in the corona pandemic. It was implemented at the health department in Calw in October 2020 and full data set ares avaible starting from December 1st 2020. The health department is using SORMAS for contact tracking, case recording and outbreak management More information about SORMAS can be found on the official SORMAS homepage: https://www.sormas-oegd.de/

Data extraction and annoymisation
Case recording data was extraction and anonymized by Iris Brilhaus, MD in the Corona task force of district office Calw. Initial data extraction was performed on April 21st 2021 and was repeated on May 13th 2021.

Data cleaning and translation
Data cleaning and translation in English was performed by Alexandra Malinovska. Only the cleaned and translated data is available here. The data set might be different from official records, because some participants with missing data were excluded. Missing data could be attributed to missing laboratory test, which were captured with a different software. Since analysis should include only patients with a recorded positive laboratory test, only these participants are contained in the data set. Questions about data cleaning and translation can be directed to her.

Data dictionary

Data dictionary for Calw dataset. 
Reportdate - Date on which the health department was notified about a participant test result - used for TimeTrend
Test Result – Up to 3 test results were collected in SORMAS. For participants with more than 3 test, 4 least test was dropped. 
Test Date - – Up to 3 test dates (day-month-year) were collected in SORMAS. For participants with more than 3 test, 4 least test was dropped. 
Age - Participants Age was collected during testing and reported to the health department. Age was verified in telephone call to the participant. SORMAS collects age in 5-year bins (0-4, 5.9, .... 100-104) - was used in 5-year bins and additionally transformed into 20-year bins (0-19, 20-39, ….80+)
Sex – Participants Sex was collected during testing and reported to the health department. Sex was verified in telephone call to the participant. Sex values: “female”, “male” or “unknown”. There was no collection of gender. 
Condition – In Covid positive patients, death is a notifiable event, and must be reported to the health department. Additionally, national death registry was checked. Values: “alive”, “dead” or “unknown”
Contact Source Case – During the phone interview, participants were asked if they have had contact to a person, who was tested COVID positive. Values “yes”, “no”, “unknown”. 
Pregnancy - During the phone interview, female participants were asked if they are currently pregnant. Values “yes”, “no”, “unknown”
Symptoms: During the phone interview, participant was asked if any of the following symptoms occur: “feeling ill","fever","shivering","chills sweats","muscle pain","headache","difficulty breathing","cough","sore throat","runny nose","nausea","diarrhea","loss of smell","loss of taste". Values “yes”, “no”, “unknown”
Symptom onset: During the phone interview, participant was asked on which date symptoms occurred. 
Time to Test / Delayed – Time from positive COVID test to onset of symptoms was calculated. If symptom occurred after positive test, this value was negative. Used as numeric value in days, as well as binned into categories (before: <0, immediate: 0-2, early 3-5, late >5)
Variant – Covid 19 variant mutations. Values: other", "B.1.1.7 - 501Y.V1”, "B.1.351 - 501Y.V2",   "B.1.1.28.1 - P.1" , “no variant of concern”, unknown”

Acknowledgment
We want to thank the health department Calw for providing this interesting and real-life data. We thank Dr. Frank Wiehe, first state official and Dr. Philip-Rene' Retzbach, legal counsel for their support. A special thank you to Iris Brilhaus for the introduction into the data and health department processes and all fruitful discussions in her free time.

Maintenance
The last data extraction was performed on May 13, 2021, further data updates and maintenance of the app will be discussed with the health department Calw

