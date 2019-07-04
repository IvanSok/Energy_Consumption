# Energy consumption analysis using submetering systems in a smart home.

Project Goal: Build predictive models to forecast future electricity consumption in Smart Homes and demonstrate clients how data can be utilised to make effective decision regarding power usage.

Data characteristics: Multivariate Timeseries containing records of a Smart Home, thereby holding the variables Global Active Power, Power Intensity, Voltage and several submeters. The data was collected between December 2006 and November 2010 with a minutely collection frequency

Data Source: http://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption

Language used: R

# Technical Approach

1. Data Preprocessing:

SQL queries

Data types conversion and scaling

Missing values treatment: calendar heat + pad + NA interpolation

Group_by to visualize different data granularities

Filter for descriptive analysis

Visualisation

Outliers treatment

2. Time-Series Creation and Decomposition

3. Remainder Analysis & Visualisation

4. Forecasting

Model Arima
Model Holt Winters
Model Hybrid Forecasting

5. Accuracy and Confidence Intervals

6. Predictions

# Usage:
master.R (@script folder) script contains the full analysis with Global Active Power predictions for 1 year (optionally you can select your own future prediction time period)

dashboard.R (@script folder) script contains an interactive dashboard for previous energy consumtion monitoring including total costs of energy for selected periods and an energy forecast graph. The dashboard is only a prototype, which will be worked on and improved in the future.
