# TripAdvisor-Challenge  March,2017
- not yet fully documented March,2018.

The goal is to predict the probability of hotel bookings on TripAdvisor given the user characteristics + browsing history data.


### Designed Shiny App to perform interactive exploratory data analysis and visualization of user data


### Data preprocessing 
-Extract New Variables Month, Season, Holiday from day variable 
-Impute with Random Forest on daysToCheckin & p_TotalPrice
-Integrate 9 osType levels into 6 levels 
-Create new variable age using Naive Bayes classifier using osType Variable combined with external data


### Prediction:
Predicted the probability of hotel bookings on TripAdvisor using Neural Network, Support Vector Machine, and XGBoost models, and visualized the comparison of models using R
