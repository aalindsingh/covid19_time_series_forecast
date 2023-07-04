#Alind Singh
#R00224088

#Load the data set
library(readxl)
install.packages('lubridate')
library(lubridate)
library(dplyr)
data <- read_xlsx('TS-covid-data_Full.xlsx')
View(data)

#Filter the data set to include data of only India
india_data <- data[data$location == "India",]

#Convert date column to date format
india_data$date <- as.Date(india_data$date)

#Check for missing values
sum(is.na(india_data$new_cases))

#Estimate missing values using LOCF
india_data$new_cases <- zoo::na.locf(india_data$new_cases)

#Check for missing values again
sum(is.na(india_data$new_cases))

#Add week and month columns to the data
india_data <- india_data %>% mutate(week = week(date),month = month(date))

#Create weekly time series
weekly_india <- india_data %>% group_by(week) %>% summarise(new_cases = sum(new_cases)) 

#Create monthly time series
monthly_india <- india_data %>% group_by(month) %>% summarise(new_cases = sum(new_cases))
monthly_india$month <- month.abb[monthly_india$month]

#Summary Statistics
summary(india_data$new_cases)
summary(weekly_india$new_cases)
summary(monthly_india$new_cases)

#Plots
library(ggplot2)
library(scales)
ggplot(india_data, aes(date, new_cases)) + 
  geom_line() +
  labs(title = "Daily New Cases in India",x='Time Period',y='Cases') +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_date(labels = date_format('%b %Y'),breaks = date_breaks('2 months'),
               guide = guide_axis(n.dodge = 3))

ggplot(weekly_india, aes(week, new_cases)) + 
  geom_line() +
  labs(title = "Weekly New Cases in India",x='Weeks',y='Cases') +
  scale_y_continuous(labels = scales::label_comma())

ggplot(monthly_india, aes(month, new_cases, group = 1)) + 
  geom_line() +
  labs(title = "Monthly New Cases in India",x='Months',y='Cases') +
  scale_y_continuous(labels = scales::label_comma())

#Decompose time series for the 
ts_daily <- ts(india_data$new_cases, frequency = 7, start = min(india_data$date))
ts_weekly <- ts(weekly_india$new_cases, frequency = 53,start = 1,  end = 7)
ts_monthly <- ts(monthly_india$new_cases, frequency = 12, start = 1, end = 12)
daily_mul <- decompose(ts_daily, type = 'multiplicative')
daily_add <- decompose(ts_daily, type = 'additive')
weekly_mul <- decompose(ts_weekly, type = 'multiplicative')
weekly_add <- decompose(ts_weekly, type = 'additive')
monthly_mul <- decompose(ts_monthly, type = 'multiplicative')
monthly_add <- decompose(ts_monthly, type = 'additive')

#Plot decomposition
library(forecast)
autoplot(daily_mul,main = 'Decomposition of Daily New Cases In India (Multiplicative)') 
autoplot(daily_add, main = 'Decomposition of Daily New Cases In India (Additive)')
autoplot(weekly_mul,main = 'Decomposition of Weekly New Cases In India (Multiplicative)') 
autoplot(weekly_add, main = 'Decomposition of Weekly New Cases In India (Additive)')
autoplot(monthly_mul,main = 'Decomposition of Monthly New Cases In India (Multiplicative)') 
autoplot(monthly_add, main = 'Decomposition of Monthly New Cases In India (Additive)')

#Calculate seasonal to trend ratio
#Additive
daily_seasonal_sd_add <- sd(daily_add$seasonal, na.rm = TRUE)
daily_trend_sd_add <- sd(daily_add$trend, na.rm = TRUE)
daily_seasonal_to_trend_add <- daily_seasonal_sd_add/daily_trend_sd_add
daily_seasonal_to_trend_add

weekly_seasonal_sd_add <- sd(weekly_add$seasonal, na.rm = TRUE)
weekly_trend_sd_add <- sd(weekly_add$trend, na.rm = TRUE)
weekly_seasonal_to_trend_add <- weekly_seasonal_sd_add/weekly_trend_sd_add
weekly_seasonal_to_trend_add
format(weekly_seasonal_to_trend_add, scientific = FALSE)

monthly_seasonal_sd_add <- sd(monthly_add$seasonal, na.rm = TRUE)
monthly_trend_sd_add <- sd(monthly_add$trend, na.rm = TRUE)
monthly_seasonal_to_trend_add <- monthly_seasonal_sd_add/monthly_trend_sd_add
monthly_seasonal_to_trend_add
format(monthly_seasonal_to_trend_add, scientific = FALSE)

#Multiplicative
daily_seasonal_sd_mul <- sd(daily_mul$seasonal, na.rm = TRUE)
daily_trend_sd_mul <- sd(daily_mul$trend, na.rm = TRUE)
daily_seasonal_to_trend_mul <- daily_seasonal_sd_mul/daily_trend_sd_mul
daily_seasonal_to_trend_mul
format(daily_seasonal_to_trend_mul, scientific = FALSE)

weekly_seasonal_sd_mul <- sd(weekly_mul$seasonal, na.rm = TRUE)
weekly_trend_sd_mul <- sd(weekly_mul$trend, na.rm = TRUE)
weekly_seasonal_to_trend_mul <- weekly_seasonal_sd_mul/weekly_trend_sd_mul
weekly_seasonal_to_trend_mul

monthly_seasonal_sd_mul <- sd(monthly_mul$seasonal, na.rm = TRUE)
monthly_trend_sd_mul <- sd(monthly_mul$trend, na.rm = TRUE)
monthly_seasonal_to_trend_mul <- monthly_seasonal_sd_mul/monthly_trend_sd_mul
monthly_seasonal_to_trend_mul

#Create plot of seasonal to trend ratio over time
daily_seasonal_trend_ratio_add <- daily_add$seasonal/daily_add$trend
plot(daily_seasonal_trend_ratio_add, type = 'l', main = 'Seasonal/Trend Ratio over Time for 
     Additive decomposition (Daily)',
     xlab='Date', ylab='Ratio Value')

daily_seasonal_trend_ratio_mul <- daily_mul$seasonal/daily_mul$trend
plot(daily_seasonal_trend_ratio_mul, type = 'l', main = 'Seasonal/Trend Ratio over Time for 
     Multiplicative decomposition (Daily)',
     xlab='Date', ylab='Ratio Value')

weekly_seasonal_trend_ratio_add <- weekly_add$seasonal/weekly_add$trend
plot(weekly_seasonal_trend_ratio_add, type = 'l', main = 'Seasonal/Trend Ratio over Time for 
     Additive decomposition (Weekly)',
     xlab='Date', ylab='Ratio Value')

weekly_seasonal_trend_ratio_mul <- weekly_mul$seasonal/weekly_mul$trend
plot(weekly_seasonal_trend_ratio_mul, type = 'l', main = 'Seasonal/Trend Ratio over Time for 
     Multiplicative decomposition (Weekly)',
     xlab='Date', ylab='Ratio Value')

monthly_seasonal_trend_ratio_add <- monthly_add$seasonal/monthly_add$trend
plot(monthly_seasonal_trend_ratio_add, type = 'l', main = 'Seasonal/Trend Ratio over Time for 
     Additive decomposition (Monthly)',
     xlab='Date', ylab='Ratio Value')

monthly_seasonal_trend_ratio_mul <- monthly_mul$seasonal/monthly_mul$trend
plot(monthly_seasonal_trend_ratio_mul, type = 'l', main = 'Seasonal/Trend Ratio over Time for 
     Multiplicative decomposition (Monthly)',
     xlab='Date', ylab='Ratio Value')

#Compute periodogram to check for periodicity
spec.pgram(ts_daily,main='Periodogram for the Daily Time Series')
spec.pgram(ts_weekly,main='Periodogram for the Weekly Time Series')
spec.pgram(ts_monthly,main='Periodogram for the Monthly Time Series')

#First Wave Subset
wave1 <- subset(india_data, india_data$date >= '2020-03-03' & 
                  india_data$date <= '2020-12-15')
ts_wave1 <- ts(wave1$new_cases, frequency = 7, start = min(wave1$date))
wave1_decomp <- decompose(ts_wave1,'multiplicative')
autoplot(wave1_decomp, main = 'Decomposition of Multiplicative First Wave Time Series')

#Second Wave Subset
wave2 <- subset(india_data, india_data$date >= '2021-02-01' & 
                  india_data$date <= '2021-07-01')
ts_wave2 <- ts(wave2$new_cases, frequency = 7, start = min(wave2$date))
wave2_decomp <- decompose(ts_wave2,'multiplicative')
autoplot(wave2_decomp, main = 'Decomposition of Multiplicative Second Wave Time Series')

#Third Wave Subset
wave3 <- subset(india_data, india_data$date >= '2022-01-01' & 
                  india_data$date <= '2022-03-01')
ts_wave3 <- ts(wave3$new_cases, frequency = 7, start = min(wave3$date))
wave3_decomp <- decompose(ts_wave3,'multiplicative')
autoplot(wave3_decomp, main = 'Decomposition of Multiplicative Third Wave Time Series')

#Fit Holt-Winter's model with additive and multiplicative seasonality 
#Deal with zero values in the full daily new cases time series object
ts_daily[ts_daily == 0] <- 1

ts_daily_india_add <- hw(ts_daily, seasonal = 'additive', damped = TRUE)
ts_daily_india_mul <- hw(ts_daily, seasonal = 'multiplicative', damped = TRUE)

#Compare the plots and accuracy of the models to decide which one to use for further analysis 
summary(ts_daily_india_add)
summary(ts_daily_india_mul)
plot(ts_daily_india_add)
plot(ts_daily_india_mul)

#Additive model has better AIC & BIC values and seem to perform well on the data
#Holt Winter's model with additive trend and seasonality
hw_full <- hw(ts_daily, seasonal = 'additive',damped = TRUE)
hw_wave1 <- hw(ts_wave1, seasonal = 'additive', damped = TRUE)
hw_wave2 <- hw(ts_wave2, seasonal = 'additive', damped = TRUE)
hw_wave3 <- hw(ts_wave3, seasonal = 'additive', damped = TRUE)

#Calculate and observe the performance of the model for full period
summary(hw_full)
accuracy(hw_full)

#Calculate and observe the performance of the model for the three wave subsets
summary(hw_wave1)
accuracy(hw_wave1)

summary(hw_wave2)
accuracy(hw_wave2)

summary(hw_wave3)
accuracy(hw_wave3)

#Plot the graphs for forecasting values for the full period and the three waves
plot(hw_full,main='Forecasts for the Full Time Series')
plot(hw_wave1,main='Forecasts for the Wave 1 Time Series')
plot(hw_wave2,main='Forecasts for the Wave 2 Time Series')
plot(hw_wave3,main='Forecasts for the Wave 3 Time Series')

#Check for stationarity using stationarity test and graph plots
#ADF and KPSS tests for full time series and the three waves
library(tseries)
adf.test(ts_daily, alternative = 'stationary')
kpss.test(ts_daily)

adf.test(ts_wave1, alternative = 'stationary')
kpss.test(ts_wave1)

adf.test(ts_wave2, alternative = 'stationary')
kpss.test(ts_wave2)

adf.test(ts_wave3, alternative = 'stationary')
kpss.test(ts_wave3)

#ACF and PACF plots for the full time series and three waves
ggtsdisplay(ts_daily, main = 'ACF and PACF plots for the full time series', lag.max = 49)
ggtsdisplay(ts_wave1, main = 'ACF and PACF plots for the wave 1 time series', lag.max = 28)
ggtsdisplay(ts_wave2, main = 'ACF and PACF plots for the wave 2 time series', lag.max = 28)
ggtsdisplay(ts_wave3, main = 'ACF and PACF plots for the wave 3 time series')

#Remove variability
log_daily <- log(ts_daily)
log_wave1 <- log(ts_wave1)
log_wave2 <- log(ts_wave2)
log_wave3 <- log(ts_wave3)

#Remove Trend and Seasonality
diff_daily <- diff(log_daily,differences = 2)
diff_wave1 <- diff(log_wave1,differences = 2)
diff_wave2 <- diff(log_wave2,differences = 2)
diff_wave3 <- diff(log_wave3,differences = 2)

#Check if stationarity is achieved 
adf.test(diff_daily, alternative = 'stationary')
kpss.test(diff_daily)

adf.test(diff_wave1, alternative = 'stationary')
kpss.test(diff_wave1)

adf.test(diff_wave2, alternative = 'stationary')
kpss.test(diff_wave2)

adf.test(diff_wave3, alternative = 'stationary')
kpss.test(diff_wave3)

#Plot the ACF and PACF graphs for the transformed time series objects and visualize the pattern
library(astsa)
acf2(diff_daily, main = 'ACF and PACF plots for the full time series')
acf2(diff_wave1, main = 'ACF and PACF plots for the wave 1 time series')
acf2(ts_wave2, main = 'ACF and PACF plots for the wave 2 time series',max.lag = 30)
acf2(diff_wave3, main = 'ACF and PACF plots for the wave 3 time series',max.lag = 30)

#Fit the ARIMA(SARIMA) models based on the correlograms and compare with models suggested
#by auto.arima
fit_daily1 <- arima(diff_daily, order = c(2,2,2),seasonal = list(order = c(0,0,0),
                                                                period = 7))
summary(fit_daily1)
BIC(fit_daily1)

fit_daily2 <- arima(diff_daily, order = c(1,2,2),seasonal = list(order = c(0,0,0),
                                                                 period = 7))
summary(fit_daily2)
BIC(fit_daily2)

fit_daily3 <- arima(diff_daily, order = c(2,2,1),seasonal = list(order = c(0,0,0),
                                                                 period = 7))
summary(fit_daily3)
BIC(fit_daily3)

auto.arima(ts_daily)

checkresiduals(fit_daily1)
checkresiduals(fit_daily2)
checkresiduals(fit_daily3)
checkresiduals(auto.arima(ts_daily))
##################################################################################
fit_wave1 <- arima(diff_wave1, order = c(2,2,1),seasonal = list(order = c(0,0,0),
                                                                period = 7))
summary(fit_wave1)
BIC(fit_wave1)
auto.arima(ts_wave1)

checkresiduals(fit_wave1)
checkresiduals(auto.arima(ts_wave1))
##################################################################################
fit_wave2.1 <- arima(diff_wave2, order = c(1,2,0),seasonal = list(order = c(0,0,0),
                                                                period = 7))
fit_wave2.2 <- arima(diff_wave2, order = c(2,2,0),seasonal = list(order = c(0,0,0),
                                                                  period = 7))
summary(fit_wave2.1)
summary(fit_wave2.2)
BIC(fit_wave2.1)
BIC(fit_wave2.2)
auto.arima(ts_wave2)

checkresiduals(fit_wave2.1)
checkresiduals(fit_wave2.2)
checkresiduals(auto.arima(ts_wave2))
#################################################################################
fit_wave3 <- arima(diff_wave3, order = c(3,0,0),seasonal = list(order = c(1,2,0),
                                                                period = 7))
summary(fit_wave3)
BIC(fit_wave3)
auto.arima(ts_wave3)

checkresiduals(fit_wave3)
checkresiduals(auto.arima(ts_wave3))

#Classical v/s ARIMA
#Multiplicative classical models
hw_full_mul <- hw(ts_daily, seasonal = 'multiplicative',damped = TRUE)
hw_wave1_mul <- hw(ts_wave1, seasonal = 'multiplicative', damped = TRUE)
hw_wave2_mul <- hw(ts_wave2, seasonal = 'multiplicative', damped = TRUE)
hw_wave3_mul <- hw(ts_wave3, seasonal = 'multiplicative', damped = TRUE)
checkresiduals(hw_full_mul)
checkresiduals(hw_wave1_mul)
checkresiduals(hw_wave2_mul)
checkresiduals(hw_wave3_mul)

#Additive classical models
hw_full_add <- hw(ts_daily, seasonal = 'additive',damped = TRUE)
hw_wave1_add <- hw(ts_wave1, seasonal = 'additive', damped = TRUE)
hw_wave2_add <- hw(ts_wave2, seasonal = 'additive', damped = TRUE)
hw_wave3_add <- hw(ts_wave3, seasonal = 'additive', damped = TRUE)
checkresiduals(hw_full_add)
checkresiduals(hw_wave1_add)
checkresiduals(hw_wave2_add)
checkresiduals(hw_wave3_add)

#ARIMA models
checkresiduals(fit_daily1)
checkresiduals(fit_wave1)
checkresiduals(fit_wave2.2)
checkresiduals(fit_wave3)

#Forecast for the next 4-weeks
#Multiplicative classical models
fcast_full_mul <- forecast(hw_full_mul, h=14)
fcast_full_mul
plot(fcast_full_mul,main = 'Forecast for Full time series (Multiplicative)')

fcast_wave1_mul <- forecast(hw_wave1_mul, h=6)
fcast_wave1_mul
plot(fcast_wave1_mul,main = 'Forecast for Wave 1 time series (Multiplicative)')

fcast_wave2_mul <- forecast(hw_wave2_mul, h=14)
fcast_wave2_mul
plot(fcast_wave2_mul,main = 'Forecast for Wave 2 time series (Multiplicative)')

fcast_wave3_mul <- forecast(hw_wave3_mul, h=4)
fcast_wave3_mul
plot(fcast_wave3_mul,main = 'Forecast for Wave 3 time series (Multiplicative)')

#Additive classical models
fcast_full_add <- forecast(hw_full_add, h=14)
fcast_full_add
plot(fcast_full_add,main = 'Forecast for Full time series (Additive)')

fcast_wave1_add <- forecast(hw_wave1_add, h=14)
fcast_wave1_add
plot(fcast_wave1_add,main = 'Forecast for Wave 1 time series (Additive)')

fcast_wave2_add <- forecast(hw_wave2_add, h=14)
fcast_wave2_add
plot(fcast_wave2_add,main = 'Forecast for Wave 2 time series (Additive)')

fcast_wave3_add <- forecast(hw_wave3_add, h=14)
fcast_wave3_add
plot(fcast_wave3_add,main = 'Forecast for Wave 3 time series (Additive)')

#ARIMA models
fcast_arima_full <- forecast(fit_daily1, h=28)
fcast_arima_full
plot(fcast_arima_full,main = 'Forecast for Full time series (ARIMA)')

fcast_arima_wave1 <- forecast(fit_wave1, h=28)
fcast_arima_wave1
plot(fcast_arima_wave1,main = 'Forecast for Wave 1 time series (ARIMA)')

fcast_arima_wave2 <- forecast(fit_wave2.2, h=14)
fcast_arima_wave2
plot(fcast_arima_wave2,main = 'Forecast for Wave 2 time series (ARIMA)')

fcast_arima_wave3 <- forecast(fit_wave3, h=28)
fcast_arima_wave3
plot(fcast_arima_wave3,main = 'Forecast for Wave 3 time series (ARIMA)')

#Auto ARIMA models
fcast_auto_full <- forecast(auto.arima(ts_daily), h=28)
fcast_auto_full
plot(fcast_auto_full,main = 'Forecast for Full time series (Auto ARIMA)')

fcast_auto_wave1 <- forecast(auto.arima(ts_wave1), h=28)
fcast_auto_wave1
plot(fcast_auto_wave1,main = 'Forecast for Wave 1 time series (Auto ARIMA)')

fcast_auto_wave2 <- forecast(auto.arima(ts_wave2), h=28)
fcast_auto_wave2
plot(fcast_auto_wave2,main = 'Forecast for Wave 2 time series (Auto ARIMA)')

fcast_auto_wave3 <- forecast(auto.arima(ts_wave3), h=28)
fcast_auto_wave3
plot(fcast_auto_wave3,main = 'Forecast for Wave 3 time series (Auto ARIMA)')

#Power of prediction
accuracy(fcast_full_mul)
accuracy(fcast_wave1_mul)
accuracy(fcast_wave2_mul)
accuracy(fcast_wave3_mul)

accuracy(fcast_full_add)
accuracy(fcast_wave1_add)
accuracy(fcast_wave2_add)
accuracy(fcast_wave3_add)

accuracy(fcast_arima_full)
accuracy(fcast_arima_wave1)
accuracy(fcast_arima_wave2)
accuracy(fcast_arima_wave3)

accuracy(fcast_auto_full)
accuracy(fcast_auto_wave1)
accuracy(fcast_auto_wave2)
accuracy(fcast_auto_wave3)