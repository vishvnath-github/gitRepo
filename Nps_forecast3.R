
dataset <- read.csv(file = "Nps_forecast.csv", header = T)
data1 <- dataset[1:18,]

####Plotting data
plot(data1[,4], col = "red", type = "l")
lines(lowess(time(data1[,4]),data1[,4]),col = "blue",lwd =2)

###Checking stationarity
## if p value is less than 0.05 then it is not stationary
library(tseries)

kpassvalue <- kpss.test(data1[,4])
kpassvalue$p.value
library(forecast)
ndiffs(data1[,4])

###To check trend, Ho: no trend, if p value is less than 0.05 it means there is a trend
library(kendall)
trtest <- MannKendall(data1[,4])
summary(trtest)

####To check seasonality

spec.ar(data1[,4])
nsdiffs(data1[,4])

# Forecasting method
#####Arma method
detach(package:forecast)
library(itsmr)
Arma_value <- arma(data1[,4],p=0, q=1)
fcast1 <- forecast(data1[,4], NULL,Arma_value,h=4, opt =2 )
forecast_value1 <- fcast1$pred

###########Holt winters code
HW <- HoltWinters(data1[,4],alpha =NULL, beta = NULL, gamma = FALSE)
detach(package:itsmr)
library(forecast)
fcast2 <- forecast(HW, h=4)
forecasted_value2 <- fcast2$mean

########Holt method
Holt <- holt(data1[,4],h= 4, exponential = T)
forecasted_value3 <- Holt$mean

##########Arima mehod
Arima <- auto.arima(data1[,4])
fcast4 <- forecast(Arima, h=4)
forecasted_value4 <- fcast4$mean

#######Exponential smoothig 
Ses <- ses(data1[,4],h=4)
forecasted_value5 <- Ses$mean

########Smoothig state space
Ets <- ets(data1[,4])
fcast6 <- forecast(Ets, h=4)
forecasted_value6 <- fcast6$mean

#####BATS(Box-Cox transformation, Arma errors, trend and seasonality)
Bats <- bats(data1[,4])
fcast7 <- forecast(Bats, h=4)
forecasted_value7 <- fcast7$mean
