library(readxl)
library(forecast)
library(tseries)

itc <- read.csv("D:/FALL SEMESTER_2021-22/A1 - DA&V - ECM3001/project/ITC.csv")
class(itc)

#opentime plot
itc_otime=ts(itc$Open,start =2002 ,end =2020 ,frequency = 365)
class(itc_otime)
plot(itc_otime, xlab = "Year", ylab = "Open", main = "Opening price of ITC Shares")

# Close time plot and forecast
itc_ctime=ts(itc$Close,start =2002 ,end =2020 ,frequency = 365)
class(itc_ctime)
plot(itc_ctime, xlab = "Year", ylab = "Close", main = "Closing price of ITC Shares")

# VWAP plot and forecast
itc_vwap=ts(itc$VWAP, start =2002 ,end =2020 ,frequency = 365)
class(itc_vwap)
plot(itc_vwap, xlab = "Year", ylab = "VWAP", main = "VWAP of ITC Shares")
acf(itc_vwap, xlab = "Year", ylab = "ACF of VWAP", main = "ACF on VWAP of ITC Shares") 
pacf(itc_vwap, xlab = "Year", ylab = "PACF of VWAP", main = "PACF on VWAP of ITC Shares") 
adf.test(itc_vwap)
itc_vwap_model=auto.arima(itc_vwap,ic="aic",trace=TRUE)
itc_vwap_model
acf(ts(itc_vwap_model$residuals))
pacf(ts(itc_vwap_model$residuals))
itc_vwap_forecast=forecast(itc_vwap_model,level=c(95),h=1*365)
itc_vwap_forecast
plot(itc_vwap_forecast, xlab = "Year", ylab = "Forecast", main = "VWAP forecast of ITC Shares")
Box.test(itc_vwap_forecast$residuals, lag=5, type= "Ljung-Box")
Box.test(itc_vwap_forecast$residuals, lag=10, type= "Ljung-Box")
Box.test(itc_vwap_forecast$residuals, lag=15, type= "Ljung-Box")

forecast1=itc_vwap_forecast[["mean"]]
actual1=ts(itc_vwap,start=2020,end=2021,frequency=365)
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
itc_t=ts(itc$Turnover, start =2002 ,end =2020 ,frequency = 365)
class(itc_t)
plot(itc_t, xlab = "Year", ylab = "Turnover", main = "Turnover of ITC Shares")
acf(itc_t, xlab = "Year", ylab = "ACF of Turnover", main = "ACF on Turnover of ITC Shares") 
pacf(itc_t, xlab = "Year", ylab = "PACF of Turnover", main = "PACF on Turnover of ITC Shares") 
adf.test(itc_t)
itc_t_model=auto.arima(itc_t,ic="aic",trace=TRUE)
itc_t_model
acf(ts(itc_t_model$residuals))
pacf(ts(itc_t_model$residuals))
itc_t_forecast=forecast(itc_t_model,level=c(95),h=1*365)
itc_t_forecast
plot(itc_t_forecast, xlab = "Year", ylab = "Forecast", main = "Turnover forecast of ITC Shares")
Box.test(itc_t_forecast$residuals, lag=5, type= "Ljung-Box")
Box.test(itc_t_forecast$residuals, lag=10, type= "Ljung-Box")
Box.test(itc_t_forecast$residuals, lag=15, type= "Ljung-Box")

forecast1=itc_t_forecast[["mean"]]
actual1=ts(itc_t,start=2020,end=2021,frequency=365)
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
itc <- na.omit(itc)
itc_dv=ts(itc$`Deliverable Volume`, start =2002 ,end =2020 ,frequency = 365)
class(itc_dv)
plot(itc_dv, xlab = "Year", ylab = " Deliverable volume ", main = " Deliverable volume of ITC Shares")
acf(itc_dv, xlab = "Year", ylab = "ACF of Deliverable volume", main = "ACF on Deliverable volume of ITC Shares") 
pacf(itc_dv, xlab = "Year", ylab = "PACF of Deliverable volume", main = "PACF on Deliverable volume of ITC Shares") 
adf.test(itc_dv)
itc_dv_model=auto.arima(itc_dv,ic="aic",trace=TRUE)
itc_dv_model
acf(ts(itc_dv_model$residuals))
pacf(ts(itc_dv_model$residuals))
itc_dv_forecast=forecast(itc_dv_model,level=c(95),h=1*365)
itc_dv_forecast
plot(itc_dv_forecast, xlab = "Year", ylab = "Forecast", main = " Deliverable volume forecast of ITC Shares")
Box.test(itc_dv_forecast$residuals, lag=5, type= "Ljung-Box")
Box.test(itc_dv_forecast$residuals, lag=10, type= "Ljung-Box")
Box.test(itc_dv_forecast$residuals, lag=15, type= "Ljung-Box")


forecast1=itc_dv_forecast[["mean"]]
actual1=ts(itc_dv,start=2020,end=2021,frequency=365)
d = actual1-forecast1
mse = mean((d)^2)
mse
mae = mean(abs(d))
mae
rmse = sqrt(mse)
rmse
cat("deliverable vol MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n")

## BOILLINGER BANDS

input <- read.csv("D:/FALL SEMESTER_2021-22/A1 - DA&V - ECM3001/project/ITV=C.csv")
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

# bollinger band - close price
library(TTR)
bb20<-BBands(abc$ClosePrice, sd=2)
xyz<-data.frame(TrDate=abc$TrDate, ClosePrice=abc$ClosePrice, VWAP=abc$VWAP, bb20)
head(xyz,24)

plot(abc$ClosePrice, xaxt="n", type="l", col=2, 
     ylim=c(min(abc$ClosePrice)*0.95, max(abc$ClosePrice)*1.05), xlab="Date", 
     main="ITC", sub="", ylab="Close Price", cex=0.6, lwd=2)
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