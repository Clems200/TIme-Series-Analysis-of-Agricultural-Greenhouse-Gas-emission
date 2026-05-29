library(tidyr)
library(tseries)
library(forecast)
library(corrplot)
library(plotly)
library(tidyverse)
library(dplyr)
library(seasonal)
library(seastests)
library(lmtest)
library(lubridate)
library(gridExtra)
library(ggthemes)
library(readxl)
library(urca)
library(ggplot2)
library(zoo)
Agricultural_Production <- read_csv("C:/Users/USER/OneDrive/Desktop/ANALYSIS/global_greenhouse_gas_emission_from_agriculture.csv")
summary(Agricultural_Production$Value)
sd(Agricultural_Production$Value,na.rm = TRUE)
var(Agricultural_Production$Value,na.rm = TRUE)


## Cleaning the dataset
Agricultural_Production<- Agricultural_Production%>%drop_na(Value)
sum(is.na(Agricultural_Production$Value))
head(Agricultural_Production$Value)

### Filter the dataset
filtered_data <- subset(Agricultural_Production,
                        Area == "Afghanistan" &
                          Element == "Emissions (CH4)" &
                          Item == "Emissions on agricultural land")
Sortered_data<- filtered_data[order(filtered_data$Year), ]
#### check the repeated years
table(filtered_data$Year)


## Creating a time series object
Agric_ts_data <- ts(filtered_data$Value,
                      start = min(filtered_data$Year),
                      frequency = 1)
Agric_ts_data
tsoutliers(Agric_ts_data)
remove_outliers <- function(x) {
  outliers <- tsoutliers(x)
  x[outliers$index] <- outliers$replacements
  return(x)
}

autoplot(Agric_ts_data,main = "PLOT OF GLOBAL GREENHOUSE GAS EMISSION",col="blue",xlab = "Year",ylab = "Greenhouse Gas Emission")
autoplot(remove_outliers(Agric_ts_data),main = "PLOT OF GLOBAL GREENHOUSE GAS EMISSION AFTER OUTLIER REMOVAL",col="blue",xlab = "Year",ylab = "Greenhouse Gas Emission")

l### BoxCox transformation
lambda <- BoxCox.lambda(Agric_ts_data, method = "guerrero",lower = -2, upper=2)
lambda
lambda2 <- BoxCox.lambda(remove_outliers(Agric_ts_data), method = "loglik",lower = -2, upper=2)
lambda2
### Transforming the data
Transformed_Agric_ts_data <- BoxCox(Agric_ts_data, lambda)
autoplot(Transformed_Agric_ts_data,main = "PLOT OF TRANSFORMED GLOBAL GREENHOUSE GAS EMISSION",col="blue",xlab = "Year",ylab = "Greenhouse Gas Emission")


### Stationarity test

### Augmented Dickey fuller test
adf.test(Transformed_Agric_ts_data)
adf.test(remove_outliers(Agric_ts_data))
adf.test(Agric_ts_data)

###pp test
pp.test(Transformed_Agric_ts_data)
pp.test(remove_outliers(Agric_ts_data))
pp.test(Agric_ts_data)

##kpss test
kpss.test(Agric_ts_data)
kpss.test(Transformed_Agric_ts_data)
kpss.test(remove_outliers(Agric_ts_data))

### checking for the number of differences required
ndiffs(Transformed_Agric_ts_data)
ndiffs(remove_outliers(Agric_ts_data))
ndiffs(Agric_ts_data)

### Applying differencing
diff_Agric_ts_data <- diff(Transformed_Agric_ts_data)
autoplot(diff_Agric_ts_data,main = "PLOT OF DIFFERENCED AGRICULTURAL PRODUCTION GAS EMISSION",col="blue",xlab = "Year",ylab = "Greenhouse Gas Emission")
diff_remove_outliers <- diff(remove_outliers(Agric_ts_data))
diff_orig<- diff(Agric_ts_data)

##Repeating the stationarity test on the differenced data
adf.test(diff_Agric_ts_data)
kpss.test(diff_Agric_ts_data)
ndiffs(diff_Agric_ts_data)
pp.test(diff_Agric_ts_data)
kpss.test(diff_remove_outliers)
###Trend analysis Mann Kendall test
library(trend)
mk.test(diff_Agric_ts_data,continuity = TRUE)
mk.test(diff_remove_outliers,continuity = TRUE)
mk.test(diff_orig,continuity = TRUE)
## Linear trend analysis
Time <- 1:length(diff_Agric_ts_data)
trend_model <- lm(diff_Agric_ts_data ~ Time)
summary(trend_model)

#### ACF and PACF analysis (GRAPHICAL MODEL SELECTION)
par(mfrow=c(1,3))
Acf(diff_Agric_ts_data, main = "ACF of Differenced trans Agricultural Production",lag.max = 24)
Pacf(diff_Agric_ts_data, main = "PACF of Differenced trans Agricultural Production",lag.max = 24)
Acf(diff_remove_outliers, main = "ACF of Differenced (No outliers) Agricultural Production",lag.max = 24)
Pacf(diff_remove_outliers, main = "PACF of Differenced (No outliers) Agricultural Production",lag.max = 24)
Acf(diff_orig, main = "ACF of original Differenced Agricultural Production",lag.max = 24)
Pacf(diff_orig, main = "PACF of original Differenced Agricultural Production",lag.max = 24)

### Generating automatic model
diff_model<-auto.arima(diff_Agric_ts_data)
trans_model<-auto.arima(Transformed_Agric_ts_data)
orig_model<-auto.arima(Agric_ts_data)
diff_remove_outliers_model<-auto.arima(diff_remove_outliers)
outlier_remove_model<-auto.arima(remove_outliers(Agric_ts_data))
diff_remove_outliers_model
outlier_remove_model
orig_model
trans_model
diff_model

### Diagnostics of the model
checkresiduals(diff_model)
checkresiduals(trans_model)
checkresiduals(orig_model)

### Normality test (small dataset)
shapiro.test(residuals(diff_model))
shapiro.test(residuals(trans_model))
shapiro.test(residuals(orig_model))

### Jarque Bera Test (Large dataset)
jarque.bera.test(residuals(diff_model))
jarque.bera.test(residuals(trans_model))
jarque.bera.test(residuals(orig_model))

#### Serial correlation test
bgtest(residuals(diff_model) ~ 1, order = 12)
bgtest(residuals(trans_model) ~ 1, order = 12)
bgtest(residuals(orig_model) ~ 1, order = 12)

### Forecasting
forecast_diff <- forecast(diff_model, h = 4)
forecast_trans <- forecast(trans_model, h = 4)
forecast_orig <- forecast(orig_model, h = 4)
forecast_diff_out<- forecast(diff_remove_outliers_model, h = 4)
forecast_out<- forecast(outlier_remove_model, h = 4)
forecast_out
forecast_diff_out
forecast_diff
forecast_trans
forecast_orig

autoplot(forecast_diff) 
autoplot(forecast_diff_out)
autoplot(forecast_orig)
autoplot(forecast_trans)
autoplot(forecast_out)

accuracy(forecast_diff)
accuracy(forecast_diff_out)
accuracy(forecast_orig)
accuracy(forecast_diff)
accuracy(forecast_out)
