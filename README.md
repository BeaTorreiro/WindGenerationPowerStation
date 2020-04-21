# Analysis and forecasting of the power generated at a wind generation power station

This is my final project for the course "Fundamentos matemáticos para el análisis de datos".

## Dataset
A real dataset “TTOTdataEEM17v3.csv” is provided containing information about a wind generation power station. There are 30 sensors located at different locations and altitudes of the station measuring temperature, wind speed, solar radiation and direction. A measure is obtained every 15 minutes and about one year of data is provided.
In total, the dataset contains 35136 observations of 124 variables: 

- Attribute Information (31 variables):
  1. Date: Date of the measurement.
  2. Hour: Hour of the day
  3. Min: Minutes with respect to the hour
  4. TLXHY: Temperature measured at location X for altitude Y
  5. GSRLXHY: Solar radiation measured at location X for altitude Y
  6. WSLXHY: Wind speed measured at location X for altitude Y.
  7. WDLXHY: Wind direction measured at location X for altitude Y.

- Output:
  1. WG: Wind power generation of the station. [MWh]

## Description
The company providing the data is very interested in extracting information about the dataset. Understanding and forecasting wind generation is of utmost importance for trading energy and operating the power plant. Hereafter are the different problems the company want to address.

## Analysis
  1. Descriptive Analysis
  2. Bayes problem regarding defective sensors
  3. Estimation of the hourly generation of the plant fitting an ARIMA regression model
  4. Creation of a classification model capable of identifying if the plant will generate more than 10MWh for one hour
