# Energy consumption analysis using submetering systems in a smart home.

Project Goal: Build predictive models to forecast future electricity consumption in Smart Homes and demonstrate clients how data can be utilised to make effective decision regarding power usage.

Data characteristics: Multivariate Timeseries containing records of a Smart Home, thereby holding the variables Global Active Power, Power Intensity, Voltage and several submeters. The data was collected between December 2006 and November 2010 with a minutely collection frequency

Data Source: http://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption

Language used: R

# Technical Approach

Data Preprocessing:

1. SQL queries

2. Data types conversion and scaling

3. Missing values treatment: calendar heat + pad + NA interpolation

4. Group_by to visualize different data granularities

5. Filter for descriptive analysis

6. Visualisation

7. Outliers treatment

Time-Series Creation and Decomposition

Remainder Analysis & Visualisation

Forecasting

1. Model Arima
2. Model Holt Winters

Accuracy and Confidence Intervals

Predictions

# Usage:
master.R (@script folder) script contains the full analysis with Global Active Power predictions for 1 year (optionally you can select your own future prediction time period)

dashboard.R (@script folder) script contains an interactive dashboard for previous energy consumtion monitoring including total costs of energy for selected periods and an energy forecast graph. The dashboard is only a prototype, which will be worked on and improved in the future.
