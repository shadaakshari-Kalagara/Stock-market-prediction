library(readxl)
library(forecast)
library(tseries)
library(tidyr)

############# ICICI BANK #############

library(readxl)
library(forecast)
library(tseries)
library(tidyr)

icici <- read.csv("D:/FALL SEMESTER_2021-22/A1 - DA&V - ECM3001/project/ICICIBANK.csv")
class(icici)

#opentime plot
icici_otime=ts(icici$Open,start =2002 ,end =2020 ,frequency = 365)
class(icici_otime)
plot(icici_otime, xlab = "Year", ylab = "Open", main = "Opening price of ICICI BANK Shares")

# Close time plot and forecast
icici_ctime=ts(icici$Close,start =2002 ,end =2020 ,frequency = 365)
class(icici_ctime)
plot(icici_ctime, xlab = "Year", ylab = "Close", main = "Closing price of ICICI BANK Shares")

# VWAP plot and forecast
icici_vwap=ts(icici$VWAP, start =2002 ,end =2020 ,frequency = 365)
class(icici_vwap)
plot(icici_vwap, xlab = "Year", ylab = "VWAP", main = "VWAP of ICICI BANK Shares")
acf(icici_vwap, xlab = "Year", ylab = "ACF of VWAP", main = "ACF on VWAP of ICICI BANK Shares")
pacf(icici_vwap, xlab = "Year", ylab = "PACF of VWAP", main = "PACF on VWAP of ICICI BANK Shares")
adf.test(icici_vwap)
icici_vwap_model=auto.arima(icici_vwap,ic="aic",trace=TRUE)
icici_vwap_model
acf(ts(icici_vwap_model$residuals))
pacf(ts(icici_vwap_model$residuals))
icici_vwap_forecast=forecast(icici_vwap_model,level=c(95),h=1*365)
icici_vwap_forecast
plot(icici_vwap_forecast, xlab = "Year", ylab = "Forecast", main = "VWAP forecast of ICICI BANK Shares")
Box.test(icici_vwap_forecast$residuals, lag=5, type= "Ljung-Box")
Box.test(icici_vwap_forecast$residuals, lag=10, type= "Ljung-Box")
Box.test(icici_vwap_forecast$residuals, lag=15, type= "Ljung-Box")

forecast1=icici_vwap_forecast[["mean"]]
actual1=ts(icici_vwap,start=2020,end=2021,frequency=365)
d = actual1-forecast1
mse = mean((d)^2)
mse
mae = mean(abs(d))
mae
rmse = sqrt(mse)
rmse
cat("VWAP MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n")

# Turnover plot and forecast
icici_t=ts(icici$Turnover, start =2002 ,end =2020 ,frequency = 365)
class(icici_t)
plot(icici_t, xlab = "Year", ylab = "Turnover", main = "Turnover of ICICI BANK Shares")
acf(icici_t, xlab = "Year", ylab = "ACF of Turnover", main = "ACF on Turnover of ICICI BANK Shares")
pacf(icici_t, xlab = "Year", ylab = "PACF of Turnover", main = "PACF on Turnover of ICICI BANK Shares")
adf.test(icici_t)
icici_t_model=auto.arima(icici_t,ic="aic",trace=TRUE)
icici_t_model
acf(ts(icici_t_model$residuals))
pacf(ts(icici_t_model$residuals))
icici_t_forecast=forecast(icici_t_model,level=c(95),h=1*365)
icici_t_forecast
plot(icici_t_forecast, xlab = "Year", ylab = "Forecast", main = "Turnover forecast of ICICI BANK Shares")
Box.test(icici_t_forecast$residuals, lag=5, type= "Ljung-Box")
Box.test(icici_t_forecast$residuals, lag=10, type= "Ljung-Box")
Box.test(icici_t_forecast$residuals, lag=15, type= "Ljung-Box")

forecast1=icici_t_forecast[["mean"]]
actual1=ts(icici_t,start=2020,end=2021,frequency=365)
d = actual1-forecast1
mse = mean((d)^2)
mse
mae = mean(abs(d))
mae
rmse = sqrt(mse)
rmse
cat("Turnover MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n")

# Deliverable volume plot and forecast
icici <- na.omit(icici)
icici_dv=ts(icici$`Deliverable Volume`, start =2002 ,end =2020 ,frequency = 365)
class(icici_dv)
plot(icici_dv, xlab = "Year", ylab = " Deliverable volume ", main = " Deliverable volume of ICICI BANK Shares")
acf(icici_dv, xlab = "Year", ylab = "ACF of Deliverable volume", main = "ACF on Deliverable volume of ICICI BANK Shares") 
pacf(icici_dv, xlab = "Year", ylab = "PACF of Deliverable volume", main = "PACF on Deliverable volume of ICICI BANK Shares") 
adf.test(icici_dv)
icici_dv_model=auto.arima(icici_dv,ic="aic",trace=TRUE)
icici_dv_model
acf(ts(icici_dv_model$residuals))
pacf(ts(icici_dv_model$residuals))
icici_dv_forecast=forecast(icici_dv_model,level=c(95),h=1*365)
icici_dv_forecast
plot(icici_dv_forecast, xlab = "Year", ylab = "Forecast", main = " Deliverable volume forecast of ICICI BANK Shares")
Box.test(icici_dv_forecast$residuals, lag=5, type= "Ljung-Box")
Box.test(icici_dv_forecast$residuals, lag=10, type= "Ljung-Box")
Box.test(icici_dv_forecast$residuals, lag=15, type= "Ljung-Box")

forecast1=icici_dv_forecast[["mean"]]
actual1=ts(icici_dv,start=2020,end=2021,frequency=365)
d = actual1-forecast1
mse = mean((d)^2)
mse
mae = mean(abs(d))
mae
rmse = sqrt(mse)
rmse
cat("Deliverable Volume MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n")


## BOILLINGER BANDS
input <- read.csv("D:/FALL SEMESTER_2021-22/A1 - DA&V - ECM3001/project/ICICI.csv")  
head(input)

TrDate=as.Date(input$Date,format="%d-%m-%Y")
ClosePrice=input$Close 
VWAP=input$VWAP
Turnover=input$Turnover
DeliverableVolume=input$Deliverable.Volume

datadf <- data.frame(TrDate, ClosePrice, VWAP, Turnover, DeliverableVolume)

abc <- datadf[order(datadf$TrDate, decreasing=F),]
rownames(abc)<-c(1:nrow(datadf))
str(abc)
head(abc)

abc$TrDate <- as.character(abc$TrDate, format="%d-%m-%Y")
head(abc)
tail(abc)

print(paste("number of trading days in the dataset = ", nrow(abc)))

library(TTR)
bb20<-BBands(abc$ClosePrice, sd=2)
xyz<-data.frame(TrDate=abc$TrDate, ClosePrice=abc$ClosePrice, VWAP=abc$VWAP, bb20)
head(xyz,24)

plot(abc$ClosePrice, xaxt="n", type="l", col=2, 
     ylim=c(min(abc$ClosePrice)*0.95, max(abc$ClosePrice)*1.05), xlab="Date", 
     main="ICICI BANK", sub="", ylab="Close Price", cex=0.6, lwd=2)
axis(1, at=1:nrow(abc), labels=abc$TrDate)
axis(2, tck=1, col.ticks="lightgray")
for (i in 0:10){
  abline(v=i*nrow(abc)/10, lty=2, col="lightgray")
}

lines(xyz$up, type="l", col='green')
lines(xyz$dn, type="l", col='green')
lines(xyz$mavg, type="l", col='blue')
legend("topleft", col=c('red', 'green', 'green', 'blue'), lty=c(1,1,1,1), 
       legend=c('Close Price', 'up', 'dn', 'mavg'), 
       bty="n", cex=0.6, lwd=c(2,1,1,1))

mean(tail(xyz$mavg,200))



############# HDFC BANK #############

library(readxl)
library(forecast)
library(tseries)

hdfc <- read_excel("D:/FALL SEMESTER_2021-22/A1 - DA&V - ECM3001/project/HDFCBANK.xlsx")
class(hdfc)

#opentime plot
hdfc_otime=ts(hdfc$Open,start =2002 ,end =2020 ,frequency = 365)
class(hdfc_otime)
plot(hdfc_otime, xlab = "Year", ylab = "Open", main = "Opening price of HDFC Bank Shares")

# Close time plot and forecast
hdfc_ctime=ts(hdfc$Close,start =2002 ,end =2020 ,frequency = 365)
class(hdfc_ctime)
plot(hdfc_ctime, xlab = "Year", ylab = "Close", main = "Closing price of HDFC Bank Shares")

# VWAP plot and forecast
hdfc_vwap=ts(hdfc$VWAP, start =2002 ,end =2020 ,frequency = 365)
class(hdfc_vwap)
plot(hdfc_vwap, xlab = "Year", ylab = "VWAP", main = "VWAP of HDFC Bank Shares")
acf(hdfc_vwap, xlab = "Year", ylab = "ACF of VWAP", main = "ACF on VWAP of HDFC Bank Shares") 
pacf(hdfc_vwap, xlab = "Year", ylab = "PACF of VWAP", main = "PACF on VWAP of HDFC Bank Shares") 
adf.test(hdfc_vwap)
hdfc_vwap_model=auto.arima(hdfc_vwap,ic="aic",trace=TRUE)
hdfc_vwap_model
acf(ts(hdfc_vwap_model$residuals))
pacf(ts(hdfc_vwap_model$residuals))
hdfc_vwap_forecast=forecast(hdfc_vwap_model,level=c(95),h=1*365)
hdfc_vwap_forecast
plot(hdfc_vwap_forecast, xlab = "Year", ylab = "Forecast", main = "VWAP forecast of HDFC Bank Shares")
Box.test(hdfc_vwap_forecast$residuals, lag=5, type= "Ljung-Box")
Box.test(hdfc_vwap_forecast$residuals, lag=10, type= "Ljung-Box")
Box.test(hdfc_vwap_forecast$residuals, lag=15, type= "Ljung-Box")

forecast1=hdfc_vwap_forecast[["mean"]]
actual1=ts(hdfc_vwap,start=2020,end=2021,frequency=365)
d = actual1-forecast1
mse = mean((d)^2)
mse
mae = mean(abs(d))
mae
rmse = sqrt(mse)
rmse
cat("VWAP MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n")

# Turnover plot and forecast
hdfc_t=ts(hdfc$Turnover, start =2002 ,end =2020 ,frequency = 365)
class(hdfc_t)
plot(hdfc_t, xlab = "Year", ylab = "Turnover", main = "Turnover of HDFC Bank Shares")
acf(hdfc_t, xlab = "Year", ylab = "ACF of Turnover", main = "ACF on Turnover of HDFC Bank Shares") 
pacf(hdfc_t, xlab = "Year", ylab = "PACF of Turnover", main = "PACF on Turnover of HDFC Bank Shares") 
adf.test(hdfc_t)
hdfc_t_model=auto.arima(hdfc_t,ic="aic",trace=TRUE)
hdfc_t_model
acf(ts(hdfc_t_model$residuals))
pacf(ts(hdfc_t_model$residuals))
hdfc_t_forecast=forecast(hdfc_t_model,level=c(95),h=1*365)
hdfc_t_forecast
plot(hdfc_t_forecast, xlab = "Year", ylab = "Forecast", main = "Turnover forecast of HDFC Bank Shares")
Box.test(hdfc_t_forecast$residuals, lag=5, type= "Ljung-Box")
Box.test(hdfc_t_forecast$residuals, lag=10, type= "Ljung-Box")
Box.test(hdfc_t_forecast$residuals, lag=15, type= "Ljung-Box")

forecast1=hdfc_t_forecast[["mean"]]
actual1=ts(hdfc_t,start=2020,end=2021,frequency=365)
d = actual1-forecast1
mse = mean((d)^2)
mse
mae = mean(abs(d))
mae
rmse = sqrt(mse)
rmse
cat("Turnover MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n")

# Deliverable volume plot and forecast
hdfc <- na.omit(hdfc)
hdfc_dv=ts(hdfc$`Deliverable Volume`, start =2002 ,end =2020 ,frequency = 365)
class(hdfc_dv)
plot(hdfc_dv, xlab = "Year", ylab = " Deliverable volume ", main = " Deliverable volume of HDFC Bank Shares")
acf(hdfc_dv, xlab = "Year", ylab = "ACF of Deliverable volume", main = "ACF on Deliverable volume of HDFC Bank Shares") 
pacf(hdfc_dv, xlab = "Year", ylab = "PACF of Deliverable volume", main = "PACF on Deliverable volume of HDFC Bank Shares") 
adf.test(hdfc_dv)
hdfc_dv_model=auto.arima(hdfc_dv,ic="aic",trace=TRUE)
hdfc_dv_model
acf(ts(hdfc_dv_model$residuals))
pacf(ts(hdfc_dv_model$residuals))
hdfc_dv_forecast=forecast(hdfc_dv_model,level=c(95),h=1*365)
hdfc_dv_forecast
plot(hdfc_dv_forecast, xlab = "Year", ylab = "Forecast", main = " Deliverable volume forecast of HDFC Bank Shares")
Box.test(hdfc_dv_forecast$residuals, lag=5, type= "Ljung-Box")
Box.test(hdfc_dv_forecast$residuals, lag=10, type= "Ljung-Box")
Box.test(hdfc_dv_forecast$residuals, lag=15, type= "Ljung-Box")

forecast1=hdfc_dv_forecast[["mean"]]
actual1=ts(hdfc_dv,start=2020,end=2021,frequency=365)
d = actual1-forecast1
mse = mean((d)^2)
mse
mae = mean(abs(d))
mae
rmse = sqrt(mse)
rmse
cat("Deliverable Volume MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n")

## BOILLINGER BANDS
input <- read.csv("D:/FALL SEMESTER_2021-22/A1 - DA&V - ECM3001/project/HDFCBANK.csv")  
head(input)

TrDate=as.Date(input$Date,format="%d-%m-%Y")
ClosePrice=input$Close 
VWAP=input$VWAP
Turnover=input$Turnover
DeliverableVolume=input$Deliverable.Volume

datadf <- data.frame(TrDate, ClosePrice, VWAP, Turnover, DeliverableVolume)

abc <- datadf[order(datadf$TrDate, decreasing=F),]
rownames(abc)<-c(1:nrow(datadf))
str(abc)
head(abc)

abc$TrDate <- as.character(abc$TrDate, format="%d-%m-%Y")
head(abc)
tail(abc)

print(paste("number of trading days in the dataset = ", nrow(abc)))

library(TTR)
bb20<-BBands(abc$ClosePrice, sd=2)
xyz<-data.frame(TrDate=abc$TrDate, ClosePrice=abc$ClosePrice, VWAP=abc$VWAP, bb20)
head(xyz,24)

plot(abc$ClosePrice, xaxt="n", type="l", col=2, 
     ylim=c(min(abc$ClosePrice)*0.95, max(abc$ClosePrice)*1.05), xlab="Date", 
     main="HDFC BANK", sub="", ylab="Close Price", cex=0.6, lwd=2)
axis(1, at=1:nrow(abc), labels=abc$TrDate)
axis(2, tck=1, col.ticks="lightgray")
for (i in 0:10){
  abline(v=i*nrow(abc)/10, lty=2, col="lightgray")
}

lines(xyz$up, type="l", col='green')
lines(xyz$dn, type="l", col='green')
lines(xyz$mavg, type="l", col='blue')
legend("topleft", col=c('red', 'green', 'green', 'blue'), lty=c(1,1,1,1), 
       legend=c('Close Price', 'up', 'dn', 'mavg'), 
       bty="n", cex=0.6, lwd=c(2,1,1,1))

mean(tail(xyz$mavg,200))


############# BAJAJ FINANCE #############

bajaj <- read.csv("D:/FALL SEMESTER_2021-22/A1 - DA&V - ECM3001/project/BAJFINANCE.csv")
class(bajaj)

#opentime plot
bajaj_otime=ts(bajaj$Open,start =2002 ,end =2020 ,frequency = 365)
class(bajaj_otime)
plot(bajaj_otime, xlab = "Year", ylab = "Open", main = "Opening price of Bajaj Finance Shares")

# Close time plot and forecast
bajaj_ctime=ts(bajaj$Close,start =2002 ,end =2020 ,frequency = 365)
class(bajaj_ctime)
plot(bajaj_ctime, xlab = "Year", ylab = "Close", main = "Closing price of Bajaj Finance Shares")

# VWAP plot and forecast
bajaj_vwap=ts(bajaj$VWAP, start =2002 ,end =2020 ,frequency = 365)
class(bajaj_vwap)
plot(bajaj_vwap, xlab = "Year", ylab = "VWAP", main = "VWAP of Bajaj Finance Shares")
acf(bajaj_vwap, xlab = "Year", ylab = "ACF of VWAP", main = "ACF on VWAP of Bajaj Finance Shares") 
pacf(bajaj_vwap, xlab = "Year", ylab = "PACF of VWAP", main = "PACF on VWAP of Bajaj Finance Shares") 
adf.test(bajaj_vwap)
bajaj_vwap_model=auto.arima(bajaj_vwap,ic="aic",trace=TRUE)
bajaj_vwap_model
acf(ts(bajaj_vwap_model$residuals))
pacf(ts(bajaj_vwap_model$residuals))
bajaj_vwap_forecast=forecast(bajaj_vwap_model,level=c(95),h=1*365)
bajaj_vwap_forecast
plot(bajaj_vwap_forecast, xlab = "Year", ylab = "Forecast", main = "VWAP forecast of Bajaj Finance Shares")
Box.test(bajaj_vwap_forecast$residuals, lag=5, type= "Ljung-Box")
Box.test(bajaj_vwap_forecast$residuals, lag=10, type= "Ljung-Box")
Box.test(bajaj_vwap_forecast$residuals, lag=15, type= "Ljung-Box")

forecast1=bajaj_vwap_forecast[["mean"]]
actual1=ts(bajaj_vwap,start=2020,end=2021,frequency=365)
d = actual1-forecast1
mse = mean((d)^2)
mse
mae = mean(abs(d))
mae
rmse = sqrt(mse)
rmse
cat("VWAP MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n")

# Turnover plot and forecast
bajaj_t=ts(bajaj$Turnover, start =2002 ,end =2020 ,frequency = 365)
class(bajaj_t)
plot(bajaj_t, xlab = "Year", ylab = "Turnover", main = "Turnover of Bajaj Finance Shares")
acf(bajaj_t, xlab = "Year", ylab = "ACF of Turnover", main = "ACF on Turnover of Bajaj Finance Shares") 
pacf(bajaj_t, xlab = "Year", ylab = "PACF of Turnover", main = "PACF on Turnover of Bajaj Finance Shares") 
adf.test(bajaj_t)
bajaj_t_model=auto.arima(bajaj_t,ic="aic",trace=TRUE)
bajaj_t_model
acf(ts(bajaj_t_model$residuals))
pacf(ts(bajaj_t_model$residuals))
bajaj_t_forecast=forecast(bajaj_t_model,level=c(95),h=1*365)
bajaj_t_forecast
plot(bajaj_t_forecast, xlab = "Year", ylab = "Forecast", main = "Turnover forecast of Bajaj Finance Shares")
Box.test(bajaj_t_forecast$residuals, lag=5, type= "Ljung-Box")
Box.test(bajaj_t_forecast$residuals, lag=10, type= "Ljung-Box")
Box.test(bajaj_t_forecast$residuals, lag=15, type= "Ljung-Box")

forecast1=bajaj_t_forecast[["mean"]]
actual1=ts(bajaj_t,start=2020,end=2021,frequency=365)
d = actual1-forecast1
mse = mean((d)^2)
mse
mae = mean(abs(d))
mae
rmse = sqrt(mse)
rmse
cat("Turnover MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n")

# Deliverable volume plot and forecast
bajaj <- na.omit(bajaj)
bajaj_dv=ts(bajaj$`Deliverable Volume`, start =2002 ,end =2020 ,frequency = 365)
class(bajaj_dv)
plot(bajaj_dv, xlab = "Year", ylab = " Deliverable volume ", main = " Deliverable volume of Bajaj Finance Shares")
acf(bajaj_dv, xlab = "Year", ylab = "ACF of Deliverable volume", main = "ACF on Deliverable volume of Bajaj Finance Shares") 
pacf(bajaj_dv, xlab = "Year", ylab = "PACF of Deliverable volume", main = "PACF on Deliverable volume of Bajaj Finance Shares") 
adf.test(bajaj_dv)
bajaj_dv_model=auto.arima(bajaj_dv,ic="aic",trace=TRUE)
bajaj_dv_model
acf(ts(bajaj_dv_model$residuals))
pacf(ts(bajaj_dv_model$residuals))
bajaj_dv_forecast=forecast(bajaj_dv_model,level=c(95),h=1*365)
bajaj_dv_forecast
plot(bajaj_dv_forecast, xlab = "Year", ylab = "Forecast", main = " Deliverable volume forecast of Bajaj Finance Shares")
Box.test(bajaj_dv_forecast$residuals, lag=5, type= "Ljung-Box")
Box.test(bajaj_dv_forecast$residuals, lag=10, type= "Ljung-Box")
Box.test(bajaj_dv_forecast$residuals, lag=15, type= "Ljung-Box")

forecast1=bajaj_dv_forecast[["mean"]]
actual1=ts(bajaj_dv,start=2020,end=2021,frequency=365)
d = actual1-forecast1
mse = mean((d)^2)
mse
mae = mean(abs(d))
mae
rmse = sqrt(mse)
rmse
cat("Deliverable Volume MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n")

## BOILLINGER BANDS
input <- read.csv("D:/FALL SEMESTER_2021-22/A1 - DA&V - ECM3001/project/BAJFINANCE.csv")  
head(input)

TrDate=as.Date(input$Date,format="%d-%m-%Y")
ClosePrice=input$Close 
VWAP=input$VWAP
Turnover=input$Turnover
DeliverableVolume=input$Deliverable.Volume

datadf <- data.frame(TrDate, ClosePrice, VWAP, Turnover, DeliverableVolume)

abc <- datadf[order(datadf$TrDate, decreasing=F),]
rownames(abc)<-c(1:nrow(datadf))
str(abc)
head(abc)

abc$TrDate <- as.character(abc$TrDate, format="%d-%m-%Y")
head(abc)
tail(abc)

print(paste("number of trading days in the dataset = ", nrow(abc)))

library(TTR)
bb20<-BBands(abc$ClosePrice, sd=2)
xyz<-data.frame(TrDate=abc$TrDate, ClosePrice=abc$ClosePrice, VWAP=abc$VWAP, bb20)
head(xyz,24)

plot(abc$ClosePrice, xaxt="n", type="l", col=2, 
     ylim=c(min(abc$ClosePrice)*0.95, max(abc$ClosePrice)*1.05), xlab="Date", 
     main="BAJAJ FINANCE", sub="", ylab="Close Price", cex=0.6, lwd=2)
axis(1, at=1:nrow(abc), labels=abc$TrDate)
axis(2, tck=1, col.ticks="lightgray")
for (i in 0:10){
  abline(v=i*nrow(abc)/10, lty=2, col="lightgray")
}

lines(xyz$up, type="l", col='green')
lines(xyz$dn, type="l", col='green')
lines(xyz$mavg, type="l", col='blue')
legend("topleft", col=c('red', 'green', 'green', 'blue'), lty=c(1,1,1,1), 
       legend=c('Close Price', 'up', 'dn', 'mavg'), 
       bty="n", cex=0.6, lwd=c(2,1,1,1))

mean(tail(xyz$mavg,200))

