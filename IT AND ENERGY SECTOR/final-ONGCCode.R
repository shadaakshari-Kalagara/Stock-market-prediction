#ongc
setwd('C:/Users/Admin/Downloads/csv datasets/datasets')
getwd()
ongc <- read.csv("ONGC.csv")
#view(ongc) typed in console to view dataset extracted
class(ongc)
#choose predictor variables
ongctime1<-na.omit(ongc$X.Deliverble)
ongctime2<-na.omit(ongc$VWAP)
ongctime3<-na.omit(ongc$Turnover)
ongctime1
ongctime2
ongctime3
ongctime1=ts(ongctime1,start =2002 ,end =2020 ,frequency = 365)
ongctime2=ts(ongc$VWAP,start =2002 ,end =2020 ,frequency = 365)
ongctime3=ts(ongc$Turnover,start =2002 ,end =2020 ,frequency = 365)
#check class of all predictor variables
class(ongctime1)
class(ongctime2)
class(ongctime3)
#import  libraries
library(forecast)
library(tseries)
plot(ongctime1,main = "Time series plot for ongc- %deliverable")
plot(ongctime2,main = "Time series plot for ongc- VWAP")
plot(ongctime3,main = "Time series plot for ongc- Turnover")

#data has to be stationary
# here is a trend
# How to check statioarity->>> check auto correlation
acf(ongctime1,na.action = na.pass,main="ongc- %deliverable")
acf(ongctime2,main="ongc- VWAP")
acf(ongctime3,main="ongc- Turnover")
#deliverable->high correlation with itself
# if the lines cross the bundary blue line the data is considered to be not stationary
# next-> check partial acf (pacf)
pacf(ongctime1,na.action = na.pass,main="ongc- %deliverable")
pacf(ongctime2,main="ongc- VWAP")
pacf(ongctime3,main="ongc- Turnover")
#pacf also tells us that the data is not stationary
# last stationarity test is  adf test
#In statistics and econometrics, an augmented Dickey-Fuller test (ADF) tests the null hypothesis 
#that a unit root is present in a time series sample. 
#The alternative hypothesis is different depending on which version of the test is used, but is usually stationarity or trend-stationarity. 
adf.test(ongctime1)
adf.test(ongctime2)
adf.test(ongctime3)
# p value should be <0.05.. if it is >0.05 it means data is not stationary
#how to convert non stationary data to stationary
#use auto arima function to do that .
ongcmodel1=auto.arima(ongctime1,ic="aic",trace=TRUE)
ongcmodel2=auto.arima(ongctime2,ic="aic",trace=TRUE)
ongcmodel3=auto.arima(ongctime3,ic="aic",trace=TRUE)
# best model arima(pdq ) will be shown
ongcmodel1
ongcmodel2
ongcmodel3
#before forecasting check for stationarity
acf(ts(ongcmodel1$residuals),main="ongc- %deliverable")
acf(ts(ongcmodel2$residuals),main="ongc- VWAP")
acf(ts(ongcmodel3$residuals),main="Turnover")
#lines are under control ->indicate data is stationary
pacf(ts(ongcmodel1$residuals),main="ongc- %deliverable")
pacf(ts(ongcmodel2$residuals),main="ongc- VWAP")
pacf(ts(ongcmodel3$residuals),main="ongc-Turnover")
#only less spikes are outside blue dotted line this indicate stationarity
myongcforecast1=forecast(ongcmodel1,level=c(95),h=1*365)
myongcforecast2=forecast(ongcmodel2,level=c(95),h=1*365)
myongcforecast3=forecast(ongcmodel3,level=c(95),h=1*365)
#select the model,confidence interval as 95,h=how many years forecast needed -here next 1year*frequency
myongcforecast1
myongcforecast2
myongcforecast3
plot(myongcforecast1,main="Forecast of %deliverable for 2021")
plot(myongcforecast2,main="Forecast of VWAP for 2021")
plot(myongcforecast3,main="Forecast of Turnover for 2021")
Box.test(myongcforecast1$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(myongcforecast1$residuals, lag=10, type= "Ljung-Box")
Box.test(myongcforecast1$residuals, lag=15, type= "Ljung-Box")
Box.test(myongcforecast2$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(myongcforecast2$residuals, lag=10, type= "Ljung-Box")
Box.test(myongcforecast2$residuals, lag=15, type= "Ljung-Box")

Box.test(myongcforecast3$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(myongcforecast3$residuals, lag=10, type= "Ljung-Box")
Box.test(myongcforecast3$residuals, lag=15, type= "Ljung-Box")
ongctime5=ts(ongc$High,start =2002 ,end =2020 ,frequency = 365)
ongctime6=ts(ongc$Low,start =2002 ,end =2020 ,frequency = 365)
plot(ongctime5,type = "l",main="ongc-High Data Plot",ylab = "High value",xlab="Year")
plot(ongctime6,type = "l",main="ongc-Low Data Plot",ylab = "Low value",xlab="Year")
ongctime7=ts(ongc$Open,start =2002 ,end =2020 ,frequency = 365)
ongctime8=ts(ongc$Close,start =2002 ,end =2020 ,frequency = 365)
plot(ongctime7,type = "l",main="ongc-Open Data Plot",ylab = "Open value",xlab="Year")
plot(ongctime8,type = "l",main="ongc-Close Data Plot",ylab = "Close value",xlab="Year")

#forecast values
forecast1=myongcforecast1[["mean"]]
forecast2=myongcforecast2[["mean"]]
forecast3=myongcforecast3[["mean"]]
forecast1
forecast2
forecast3
#actual values
actual1=ts(ongctime1,start=2020,end=2021,frequency=365)
actual2=ts(ongctime2,start=2020,end=2021,frequency=365)
actual3=ts(ongctime3,start=2020,end=2021,frequency=365)
actual1
actual2
actual3
d = actual1-forecast1
mse = mean((d)^2)
mse
mae = mean(abs(d))
mae
rmse = sqrt(mse)
rmse
cat("%deliverable MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n")

d = actual2-forecast2
mse = mean((d)^2)
mse
mae = mean(abs(d))
mae
rmse = sqrt(mse)
rmse
cat("VWAP- MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n")

d = actual3-forecast3
mse = mean((d)^2)
mse
mae = mean(abs(d))
mae
rmse = sqrt(mse)
rmse
cat("Turnover MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n")

# bollinger bands
input <- read.csv("ONGC.csv")  
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
     main="ongc", sub="", ylab="Close Price", cex=0.6, lwd=2)
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
