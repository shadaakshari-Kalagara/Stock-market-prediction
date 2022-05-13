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

