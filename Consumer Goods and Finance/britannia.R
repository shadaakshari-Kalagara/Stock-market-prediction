library(readxl)
library(forecast)
library(tseries)

brit <- read.csv("D:/FALL SEMESTER_2021-22/A1 - DA&V - ECM3001/project/BRITANNIA.csv")
class(brit)

#opentime plot
brit_otime=ts(brit$Open,start =2002 ,end =2020 ,frequency = 365)
class(brit_otime)
plot(brit_otime, xlab = "Year", ylab = "Open", main = "Opening price of Britannia Shares")

# Close time plot and forecast
brit_ctime=ts(brit$Close,start =2002 ,end =2020 ,frequency = 365)
class(brit_ctime)
plot(brit_ctime, xlab = "Year", ylab = "Close", main = "Closing price of Britannia Shares")

# VWAP plot and forecast
brit_vwap=ts(brit$VWAP, start =2002 ,end =2020 ,frequency = 365)
class(brit_vwap)
plot(brit_vwap, xlab = "Year", ylab = "VWAP", main = "VWAP of Britannia Shares")
acf(brit_vwap, xlab = "Year", ylab = "ACF of VWAP", main = "ACF on VWAP of Britannia Shares") 
pacf(brit_vwap, xlab = "Year", ylab = "PACF of VWAP", main = "PACF on VWAP of Britannia Shares") 
adf.test(brit_vwap)
brit_vwap_model=auto.arima(brit_vwap,ic="aic",trace=TRUE)
brit_vwap_model
acf(ts(brit_vwap_model$residuals))
pacf(ts(brit_vwap_model$residuals))
brit_vwap_forecast=forecast(brit_vwap_model,level=c(95),h=1*365)
brit_vwap_forecast
plot(brit_vwap_forecast, xlab = "Year", ylab = "Forecast", main = "VWAP forecast of Britannia Shares")
Box.test(brit_vwap_forecast$residuals, lag=5, type= "Ljung-Box")
Box.test(brit_vwap_forecast$residuals, lag=10, type= "Ljung-Box")
Box.test(brit_vwap_forecast$residuals, lag=15, type= "Ljung-Box")

forecast1=brit_vwap_forecast[["mean"]]
actual1=ts(brit_vwap,start=2020,end=2021,frequency=365)
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
brit_t=ts(brit$Turnover, start =2002 ,end =2020 ,frequency = 365)
class(brit_t)
plot(brit_t, xlab = "Year", ylab = "Turnover", main = "Turnover of Britannia Shares")
acf(brit_t, xlab = "Year", ylab = "ACF of Turnover", main = "ACF on Turnover of Britannia Shares") 
pacf(brit_t, xlab = "Year", ylab = "PACF of Turnover", main = "PACF on Turnover of Britannia Shares") 
adf.test(brit_t)
brit_t_model=auto.arima(brit_t,ic="aic",trace=TRUE)
brit_t_model
acf(ts(brit_t_model$residuals))
pacf(ts(brit_t_model$residuals))
brit_t_forecast=forecast(brit_t_model,level=c(95),h=1*365)
brit_t_forecast
plot(brit_t_forecast, xlab = "Year", ylab = "Forecast", main = "Turnover forecast of Britannia Shares")
Box.test(brit_t_forecast$residuals, lag=5, type= "Ljung-Box")
Box.test(brit_t_forecast$residuals, lag=10, type= "Ljung-Box")
Box.test(brit_t_forecast$residuals, lag=15, type= "Ljung-Box")

forecast1=brit_t_forecast[["mean"]]
actual1=ts(brit_t,start=2020,end=2021,frequency=365)
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
brit <- na.omit(brit)
brit_dv=ts(brit$`Deliverable Volume`, start =2002 ,end =2020 ,frequency = 365)
class(brit_dv)
plot(brit_dv, xlab = "Year", ylab = " Deliverable volume ", main = " Deliverable volume of Britannia Shares")
acf(brit_dv, xlab = "Year", ylab = "ACF of Deliverable volume", main = "ACF on Deliverable volume of Britannia Shares") 
pacf(brit_dv, xlab = "Year", ylab = "PACF of Deliverable volume", main = "PACF on Deliverable volume of Britannia Shares") 
adf.test(brit_dv)
brit_dv_model=auto.arima(brit_dv,ic="aic",trace=TRUE)
brit_dv_model
acf(ts(brit_dv_model$residuals))
pacf(ts(brit_dv_model$residuals))
brit_dv_forecast=forecast(brit_dv_model,level=c(95),h=1*365)
brit_dv_forecast
plot(brit_dv_forecast, xlab = "Year", ylab = "Forecast", main = " Deliverable volume forecast of Britannia Shares")
Box.test(brit_dv_forecast$residuals, lag=5, type= "Ljung-Box")
Box.test(brit_dv_forecast$residuals, lag=10, type= "Ljung-Box")
Box.test(brit_dv_forecast$residuals, lag=15, type= "Ljung-Box")

forecast1=brit_dv_forecast[["mean"]]
actual1=ts(brit_dv,start=2020,end=2021,frequency=365)
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

input <- read.csv("D:/FALL SEMESTER_2021-22/A1 - DA&V - ECM3001/project/BRITANNIA.csv")
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
     main="NESTLE INDIA", sub="", ylab="Close Price", cex=0.6, lwd=2)
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