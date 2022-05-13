library(readxl)
library(forecast)
library(tseries)

nestle <- read.csv("D:/FALL SEMESTER_2021-22/A1 - DA&V - ECM3001/project/NESTLEIND.csv")
class(nestle)

#opentime plot
nestle_otime=ts(nestle$Open,start =2010 ,end =2020 ,frequency = 365)
class(nestle_otime)
plot(nestle_otime, xlab = "Year", ylab = "Open", main = "Opening price of Nestle India Shares")

# Close time plot and forecast
nestle_ctime=ts(nestle$Close,start =2010 ,end =2020 ,frequency = 365)
class(nestle_ctime)
plot(nestle_ctime, xlab = "Year", ylab = "Close", main = "Closing price of Nestle India Shares")

# VWAP plot and forecast
nestle_vwap=ts(nestle$VWAP, start =2010 ,end =2020 ,frequency = 365)
class(nestle_vwap)
plot(nestle_vwap, xlab = "Year", ylab = "VWAP", main = "VWAP of Nestle India Shares")
acf(nestle_vwap, xlab = "Year", ylab = "ACF of VWAP", main = "ACF on VWAP of Nestle India Shares") 
pacf(nestle_vwap, xlab = "Year", ylab = "PACF of VWAP", main = "PACF on VWAP of Nestle India Shares") 
adf.test(nestle_vwap)
nestle_vwap_model=auto.arima(nestle_vwap,ic="aic",trace=TRUE)
nestle_vwap_model
acf(ts(nestle_vwap_model$residuals))
pacf(ts(nestle_vwap_model$residuals))
nestle_vwap_forecast=forecast(nestle_vwap_model,level=c(95),h=1*365)
nestle_vwap_forecast
plot(nestle_vwap_forecast, xlab = "Year", ylab = "Forecast", main = "VWAP forecast of Nestle India Shares")
Box.test(nestle_vwap_forecast$residuals, lag=5, type= "Ljung-Box")
Box.test(nestle_vwap_forecast$residuals, lag=10, type= "Ljung-Box")
Box.test(nestle_vwap_forecast$residuals, lag=15, type= "Ljung-Box")

forecast1=nestle_vwap_forecast[["mean"]]
actual1=ts(nestle_vwap,start=2020,end=2021,frequency=365)
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
nestle_t=ts(nestle$Turnover, start =2010 ,end =2020 ,frequency = 365)
class(nestle_t)
plot(nestle_t, xlab = "Year", ylab = "Turnover", main = "Turnover of Nestle India Shares")
acf(nestle_t, xlab = "Year", ylab = "ACF of Turnover", main = "ACF on Turnover of Nestle India Shares") 
pacf(nestle_t, xlab = "Year", ylab = "PACF of Turnover", main = "PACF on Turnover of Nestle India Shares") 
adf.test(nestle_t)
nestle_t_model=auto.arima(nestle_t,ic="aic",trace=TRUE)
nestle_t_model
acf(ts(nestle_t_model$residuals))
pacf(ts(nestle_t_model$residuals))
nestle_t_forecast=forecast(nestle_t_model,level=c(95),h=1*365)
nestle_t_forecast
plot(nestle_t_forecast, xlab = "Year", ylab = "Forecast", main = "Turnover forecast of Nestle India Shares")
Box.test(nestle_t_forecast$residuals, lag=5, type= "Ljung-Box")
Box.test(nestle_t_forecast$residuals, lag=10, type= "Ljung-Box")
Box.test(nestle_t_forecast$residuals, lag=15, type= "Ljung-Box")

forecast1=nestle_t_forecast[["mean"]]
actual1=ts(nestle_t,start=2020,end=2021,frequency=365)
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
nestle <- na.omit(nestle)
nestle_dv=ts(nestle$`Deliverable Volume`, start =2010 ,end =2020 ,frequency = 365)
class(nestle_dv)
plot(nestle_dv, xlab = "Year", ylab = " Deliverable volume ", main = " Deliverable volume of Nestle India Shares")
acf(nestle_dv, xlab = "Year", ylab = "ACF of Deliverable Volume", main = "ACF on Deliverable volume of Nestle India Shares") 
pacf(nestle_dv, xlab = "Year", ylab = "PACF of Deliverable Volume", main = "PACF on Deliverable volume of Nestle India Shares") 
adf.test(nestle_dv)
nestle_dv_model=auto.arima(nestle_dv,ic="aic",trace=TRUE)
nestle_dv_model
acf(ts(nestle_dv_model$residuals))
pacf(ts(nestle_dv_model$residuals))
nestle_dv_forecast=forecast(nestle_dv_model,level=c(95),h=1*365)
nestle_dv_forecast
plot(nestle_dv_forecast, xlab = "Year", ylab = "Forecast", main = " Deliverable volume forecast of Nestle India Shares")
Box.test(nestle_dv_forecast$residuals, lag=5, type= "Ljung-Box")
Box.test(nestle_dv_forecast$residuals, lag=10, type= "Ljung-Box")
Box.test(nestle_dv_forecast$residuals, lag=15, type= "Ljung-Box")

forecast1=nestle_dv_forecast[["mean"]]
actual1=ts(nestle_dv,start=2020,end=2021,frequency=365)
d = actual1-forecast1
mse = mean((d)^2)
mse
mae = mean(abs(d))
mae
rmse = sqrt(mse)
rmse
cat("Deliverable Volume MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n")


## BOILINGER BANDS
input <- read.csv("D:/FALL SEMESTER_2021-22/A1 - DA&V - ECM3001/project/NESTLEIND.csv")  
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

