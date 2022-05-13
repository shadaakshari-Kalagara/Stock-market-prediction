#IT Sector 
#Wipro
setwd('C:/Users/Admin/Downloads/csv datasets/datasets')
getwd()
wipro <- read.csv("WIPRO.csv")
#view(wipro) typed in console to view dataset extracted
class(wipro)
#choose predictor variables
wiprotime1<-na.omit(wipro$X.Deliverble)
wiprotime2<-na.omit(wipro$VWAP)
wiprotime3<-na.omit(wipro$Turnover)
wiprotime1
wiprotime2
wiprotime3
wiprotime1=ts(wiprotime1,start =2002 ,end =2020 ,frequency = 365)
wiprotime2=ts(wipro$VWAP,start =2002 ,end =2020 ,frequency = 365)
wiprotime3=ts(wipro$Turnover,start =2002 ,end =2020 ,frequency = 365)
#check class of all predictor variables
class(wiprotime1)
class(wiprotime2)
class(wiprotime3)
#import  libraries
library(forecast)
library(tseries)
plot(wiprotime1,main = "Time series plot for WIPRO- %deliverable")
plot(wiprotime2,main = "Time series plot for WIPRO- VWAP")
plot(wiprotime3,main = "Time series plot for WIPRO- Turnover")

#data has to be stationary
# here is a trend
# How to check statioarity->>> check auto correlation
acf(wiprotime1,na.action = na.pass,main="WIPRO- %deliverable")
acf(wiprotime2,main="WIPRO- VWAP")
acf(wiprotime3,main="WIPRO- Turnover")
#deliverable->high correlation with itself
# if the lines cross the bundary blue line the data is considered to be not stationary
# next-> check partial acf (pacf)
pacf(wiprotime1,na.action = na.pass,main="WIPRO- %deliverable")
pacf(wiprotime2,main="WIPRO- VWAP")
pacf(wiprotime3,main="WIPRO- Turnover")
#pacf also tells us that the data is not stationary
# last stationarity test is  adf test
#In statistics and econometrics, an augmented Dickey-Fuller test (ADF) tests the null hypothesis 
#that a unit root is present in a time series sample. 
#The alternative hypothesis is different depending on which version of the test is used, but is usually stationarity or trend-stationarity. 
adf.test(wiprotime1)
adf.test(wiprotime2)
adf.test(wiprotime3)
# p value should be <0.05.. if it is >0.05 it means data is not stationary
#how to convert non stationary data to stationary
#use auto arima function to do that .
wipromodel1=auto.arima(wiprotime1,ic="aic",trace=TRUE)
wipromodel2=auto.arima(wiprotime2,ic="aic",trace=TRUE)
wipromodel3=auto.arima(wiprotime3,ic="aic",trace=TRUE)
# best model arima(pdq ) will be shown
wipromodel1
wipromodel2
wipromodel3
#before forecasting check for stationarity
acf(ts(wipromodel1$residuals),main="WIPRO- %deliverable")
acf(ts(wipromodel2$residuals),main="WIPRO- VWAP")
acf(ts(wipromodel3$residuals),main="Turnover")
#lines are under control ->indicate data is stationary
pacf(ts(wipromodel1$residuals),main="WIPRO- %deliverable")
pacf(ts(wipromodel2$residuals),main="WIPRO- VWAP")
pacf(ts(wipromodel3$residuals),main="WIPRO-Turnover")
#only less spikes are outside blue dotted line this indicate stationarity
mywiproforecast1=forecast(wipromodel1,level=c(95),h=1*365)
mywiproforecast2=forecast(wipromodel2,level=c(95),h=1*365)
mywiproforecast3=forecast(wipromodel3,level=c(95),h=1*365)
#select the model,confidence interval as 95,h=how many years forecast needed -here next 1year*frequency
mywiproforecast1
mywiproforecast2
mywiproforecast3
plot(mywiproforecast1,main="Forecast of %deliverable for 2021")
plot(mywiproforecast2,main="Forecast of VWAP for 2021")
plot(mywiproforecast3,main="Forecast of Turnover for 2021")
Box.test(mywiproforecast1$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(mywiproforecast1$residuals, lag=10, type= "Ljung-Box")
Box.test(mywiproforecast1$residuals, lag=15, type= "Ljung-Box")
Box.test(mywiproforecast2$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(mywiproforecast2$residuals, lag=10, type= "Ljung-Box")
Box.test(mywiproforecast2$residuals, lag=15, type= "Ljung-Box")

Box.test(mywiproforecast3$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(mywiproforecast3$residuals, lag=10, type= "Ljung-Box")
Box.test(mywiproforecast3$residuals, lag=15, type= "Ljung-Box")
wiprotime5=ts(wipro$High,start =2002 ,end =2020 ,frequency = 365)
wiprotime6=ts(wipro$Low,start =2002 ,end =2020 ,frequency = 365)
plot(wiprotime5,type = "l",main="Wipro-High Data Plot",ylab = "High value",xlab="Year")
plot(wiprotime6,type = "l",main="Wipro-Low Data Plot",ylab = "Low value",xlab="Year")
wiprotime7=ts(wipro$Open,start =2002 ,end =2020 ,frequency = 365)
wiprotime8=ts(wipro$Close,start =2002 ,end =2020 ,frequency = 365)
plot(wiprotime7,type = "l",main="Wipro-Open Data Plot",ylab = "Open value",xlab="Year")
plot(wiprotime8,type = "l",main="Wipro-Close Data Plot",ylab = "Close value",xlab="Year")

#forecast values
forecast1=mywiproforecast1[["mean"]]
forecast2=mywiproforecast2[["mean"]]
forecast3=mywiproforecast3[["mean"]]
forecast1
forecast2
forecast3
#actual values
actual1=ts(wiprotime1,start=2020,end=2021,frequency=365)
actual2=ts(wiprotime2,start=2020,end=2021,frequency=365)
actual3=ts(wiprotime3,start=2020,end=2021,frequency=365)
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
input <- read.csv("WIPRO.csv")  
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
     main="WIPRO", sub="", ylab="Close Price", cex=0.6, lwd=2)
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

