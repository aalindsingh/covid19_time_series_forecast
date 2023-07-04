# covid19_time_series_forecast
An application built using R, for predicting the new cases of Covid-19 in India for the next 4-weeks.

Data cleaning and manipulation
*********************************
Missing values of the variable number of tests performed in a given day need to be estimated using
a method of your choice (e.g. by interpolation of previous and posterior observations, by average of
the period/dataset, etc.). Hint: consider the seasonality observed in the data.
Using the number of tests performed every day in a chosen country, create a new time series for the
weekly and monthly number of tests performed in the given period. Discuss on the different time
series (daily, weekly and monthly).

Preliminar analysis
**********************************
• Carry out a preliminary descriptive analysis on the data. Use summary statistics and plots to describe
the dataset (using the 3 different datasets).
• Decompose the dataset and analyse the different components of the series. State what type of model
– additive or multiplicative – is more appropriate to describe this series. Help your conclusions by
using appropriate plots and summaries.
• Analyse importance of each component (if any) in the time series. Is this component conditioning the
analysis in any way? Explain how this issue could be addressed if required.
• Indicate if periodicity could be a problem in the time series.
• Only for the daily dataset, identify three periods for different waves of COVID and generate a new
time series for the period of each wave (create 3 subsets of the original time series). Repeat the
previous analysis for the different waves and discuss on the differences observed.

Time series modelling
***********************************
You should carry out this analysis using only the daily time series. Implement the time series
techniques on the four time series: the 3 different waves and the time series for the full period.
• Implement the Classical Method (SES, Holt’s, Holt-Winter’s) that you consider is more appropriate for
describing this data. Explain why do you think this is the best option.
Implement your chosen classical model and indicate the best model options for the trend and
seasonality components (additive and multiplicative). Explain which of the options fits better the time
series. Support your conclusions.
Is this conclusion consistent with what you expected in the preliminary analysis?
• Indicate whether these time series are stationary. Use the formal tests and graphical tools you
consider appropriate to support your conclusion.
In case it is not stationary, manipulate the dataset to obtain an appropriate series which meets the
stationarity conditions for further analysis using ARIMA models. Explain and justify all the process in
the transformation.
Use the correlograms for analysing the transformed time series. Based on the correlograms, what type
of ARIMA-SARIMA model do you expect to best describe these timeseries. Explain your answer.
Chose three models you think are good candidates for this dataset and analyse the residuals. Use
plots and formal tests to compare the models.
Find the model proposed by R using “auto.arima” command and discuss whether you think this is a
good option for this dataset. Do you think that any of your models would be a better option?
What model do you consider is better for this dataset, the Classical or the ARIMA model?

Forecast
************************************
Make a 4-weeks forecast for the next periods in both cases, using the Classical and the ARIMA models.
• In the Classical model, carry out the prediction for multiplicative and additive scenarios and compare
the results.
• For the ARIMA models, carry out the prediction using the auto.arima model recommended by the
software, and also the model you chose during your analysis. Compare the results.
• Make a comparison of the results in the predictions using the Classical and the ARIMA models.
• Check the power of prediction of your models by forecasting the last periods of your time series and
compare them with the actual observed values for such period you have predicted.
