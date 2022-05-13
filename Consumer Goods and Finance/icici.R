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

