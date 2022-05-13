#AUTOMOBILE Sector 
#Maruti
setwd('D:/DA&V/proj/bollinger')
getwd()
maruti <- read.csv("MARUTI.csv")
#view(maruti) typed in console to view dataset extracted
class(maruti)
#choose predictor variables
marutitime1<-na.omit(maruti$X.Deliverble)
marutitime2<-na.omit(maruti$VWAP)
marutitime3<-na.omit(maruti$Turnover)
marutitime1
marutitime2
marutitime3
marutitime1=ts(marutitime1,start =2002 ,end =2020 ,frequency = 365)
marutitime2=ts(maruti$VWAP,start =2002 ,end =2020 ,frequency = 365)
marutitime3=ts(maruti$Turnover,start =2002 ,end =2020 ,frequency = 365)
#check class of all predictor variables
class(marutitime1)
class(marutitime2)
class(marutitime3)
#import  libraries
library(forecast)
library(tseries)
plot(marutitime1,main = "Time series plot for MARUTI- %deliverable")
plot(marutitime2,main = "Time series plot for MARUTI- VWAP")
plot(marutitime3,main = "Time series plot for MARUTI- Turnover")

#data has to be stationary
# here is a trend
# How to check statioarity->>> check auto correlation
acf(marutitime1,na.action = na.pass,main="MARUTI- %deliverable")
acf(marutitime2,main="MARUTI- VWAP")
acf(marutitime3,main="MARUTI- Turnover")
#deliverable->high correlation with itself
# if the lines cross the bundary blue line the data is considered to be not stationary
# next-> check partial acf (pacf)
pacf(marutitime1,na.action = na.pass,main="MARUTI- %deliverable")
pacf(marutitime2,main="MARUTI- VWAP")
pacf(marutitime3,main="MARUTI- Turnover")
#pacf also tells us that the data is not stationary
# last stationarity test is  adf test
#In statistics and econometrics, an augmented Dickey-Fuller test (ADF) tests the null hypothesis 
#that a unit root is present in a time series sample. 
#The alternative hypothesis is different depending on which version of the test is used, but is usually stationarity or trend-stationarity. 
adf.test(marutitime1)
adf.test(marutitime2)
adf.test(marutitime3)
# p value should be <0.05.. if it is >0.05 it means data is not stationary
#how to convert non stationary data to stationary
#use auto arima function to do that .
marutimodel1=auto.arima(marutitime1,ic="aic",trace=TRUE)
marutimodel2=auto.arima(marutitime2,ic="aic",trace=TRUE)
marutimodel3=auto.arima(marutitime3,ic="aic",trace=TRUE)
# best model arima(pdq ) will be shown
marutimodel1
marutimodel2
marutimodel3
#before forecasting check for stationarity
acf(ts(marutimodel1$residuals),main="MARUTI- %deliverable")
acf(ts(marutimodel2$residuals),main="MARUTI- VWAP")
acf(ts(marutimodel3$residuals),main="Turnover")
#lines are under control ->indicate data is stationary
pacf(ts(marutimodel1$residuals),main="MARUTI- %deliverable")
pacf(ts(marutimodel2$residuals),main="MARUTI- VWAP")
pacf(ts(marutimodel3$residuals),main="MARUTI-Turnover")
#only less spikes are outside blue dotted line this indicate stationarity
mymarutiforecast1=forecast(marutimodel1,level=c(95),h=1*365)
mymarutiforecast2=forecast(marutimodel2,level=c(95),h=1*365)
mymarutiforecast3=forecast(marutimodel3,level=c(95),h=1*365)
#select the model,confidence interval as 95,h=how many years forecast needed -here next 1year*frequency
mymarutiforecast1
mymarutiforecast2
mymarutiforecast3
plot(mymarutiforecast1,main="Forecast of %deliverable for 2021")
plot(mymarutiforecast2,main="Forecast of VWAP for 2021")
plot(mymarutiforecast3,main="Forecast of Turnover for 2021")
Box.test(mymarutiforecast1$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(mymarutiforecast1$residuals, lag=10, type= "Ljung-Box")
Box.test(mymarutiforecast1$residuals, lag=15, type= "Ljung-Box")
Box.test(mymarutiforecast2$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(mymarutiforecast2$residuals, lag=10, type= "Ljung-Box")
Box.test(mymarutiforecast2$residuals, lag=15, type= "Ljung-Box")

Box.test(mymarutiforecast3$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(mymarutiforecast3$residuals, lag=10, type= "Ljung-Box")
Box.test(mymarutiforecast3$residuals, lag=15, type= "Ljung-Box")
marutitime5=ts(maruti$High,start =2002 ,end =2020 ,frequency = 365)
marutitime6=ts(maruti$Low,start =2002 ,end =2020 ,frequency = 365)
plot(marutitime5,type = "l",main="Maruti-High Data Plot",ylab = "High value",xlab="Year")
plot(marutitime6,type = "l",main="Maruti-Low Data Plot",ylab = "Low value",xlab="Year")
marutitime7=ts(maruti$Open,start =2002 ,end =2020 ,frequency = 365)
marutitime8=ts(maruti$Close,start =2002 ,end =2020 ,frequency = 365)
plot(marutitime7,type = "l",main="Maruti-Open Data Plot",ylab = "Open value",xlab="Year")
plot(marutitime8,type = "l",main="Maruti-Close Data Plot",ylab = "Close value",xlab="Year")

#forecast values
forecast1=mymarutiforecast1[["mean"]]
forecast2=mymarutiforecast2[["mean"]]
forecast3=mymarutiforecast3[["mean"]]
forecast1
forecast2
forecast3
#actual values
actual1=ts(marutitime1,start=2020,end=2021,frequency=365)
actual2=ts(marutitime2,start=2020,end=2021,frequency=365)
actual3=ts(marutitime3,start=2020,end=2021,frequency=365)
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
input <- read.csv("MARUTI.csv")  
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
     main="MARUTI", sub="", ylab="Close Price", cex=0.6, lwd=2)
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

