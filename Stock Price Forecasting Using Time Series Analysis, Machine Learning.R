#Getting AMAZON stock dataset and loading the needed packages
if(!require(quantmod)) install.packages("quantmod")
if(!require(forecast)) install.packages("forecast")
if(!require(xlsx)) install.packages("xlsx")
if(!require(tseries)) install.packages("tseries")
if(!require(timeSeries)) install.packages("timeSeries")
if(!require(dplyr)) install.packages("dplyr")
if(!require(fGarch)) install.packages("fGarch")
if(!require(prophet)) install.packages("prophet")
library(prophet)
library(quantmod)
library(forecast)
library("xlsx")
library(tseries)
library(timeSeries)
library(dplyr)
library(fGarch)


library(readr)
new_train5 <- read_csv("C:/Users/MarkCichonski/Learn ML 2021 Grand AI Challenge/new_train5.csv")
new_train5$`Close-Stock-5`<-as.numeric(new_train5$`Close-Stock-5`)
new_train5$Date <- as.Date(new_train5$Date, "%m/%d/%Y")
plot(`Close-Stock-5` ~ Date, new_train5, xaxt = "n", type = "l")

print(adf.test(new_train$Close.Stock.1))

modelfit <- auto.arima(new_train5$`Close-Stock-5`, lambda = "auto")
modelfit

price_forecast <- forecast(modelfit, h=97)
price_forecast
pf<-as.data.frame(price_forecast)

write.csv(pf,"C:\\Users\\MarkCichonski\\Learn ML 2021 Grand AI Challenge\\resultsrar5.csv", row.names = FALSE)

#Garch
library(rugarch)
#Dataset forecast upper first 5 values
fitarfima = autoarfima(data = new_train5$`Close-Stock-5`, ar.max = 2, ma.max = 2, 
                       criterion = "AIC", method = "full")

fitarfima

#define the model
garch11closeprice=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,2)))
#estimate model 
garch11closepricefit=ugarchfit(spec=garch11closeprice, data=new_train5$`Close-Stock-5`)

#conditional volatility plot
plot.ts(sigma(garch11closepricefit), ylab="sigma(t)", col="blue")

#Model akike
infocriteria(garch11closepricefit)

#Normal residuals
garchres <- data.frame(residuals(garch11closepricefit))  
plot(garchres$residuals.garch11closepricefit.)

#Standardized residuals
garchres <- data.frame(residuals(garch11closepricefit, standardize=TRUE)) 
#Normal Q plot
qqnorm(garchres$residuals.garch11closepricefit..standardize...TRUE.)
qqline(garchres$residuals.garch11closepricefit..standardize...TRUE.)

#GARCH Forecasting
garchforecast <- ugarchforecast(garch11closepricefit, n.ahead = 97 )
garchforecast
#Cut and paste console

#prophet model application
sapply(new_train1, class)
dfp <- data.frame(ds = new_train5$Date, y = new_train5$`Close-Stock-5`)
prophetpred <- prophet(dfp)
future <- make_future_dataframe(prophetpred, periods = 97)
forecastprophet <- predict(prophetpred, future)

dataprediction <- data.frame(forecastprophet$ds,forecastprophet$yhat)
trainlen <- length(new_train5$`Close-Stock-5`)
dataprediction <- dataprediction[c(1:trainlen),]


accuracy(dataprediction$forecastprophet.yhat,dfp$y)

write.csv(forecastprophet,"C:\\Users\\MarkCichonski\\Learn ML 2021 Grand AI Challenge\\resultsrpr5.csv", row.names = FALSE)




