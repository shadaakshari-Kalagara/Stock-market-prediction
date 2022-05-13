#Energy Sector 
#BPCL
setwd('C:/Users/Admin/Downloads/csv datasets/datasets')
getwd()
bpcl <- read.csv("BPCL.csv")
#view(bpcl) typed in console to view dataset extracted
class(bpcl)
#choose predictor variables
bpcltime1<-na.omit(bpcl$X.Deliverble)
bpcltime2<-na.omit(bpcl$VWAP)
bpcltime3<-na.omit(bpcl$Turnover)
bpcltime1
bpcltime2
bpcltime3
bpcltime1=ts(bpcltime1,start =2002 ,end =2020 ,frequency = 365)
bpcltime2=ts(bpcl$VWAP,start =2002 ,end =2020 ,frequency = 365)
bpcltime3=ts(bpcl$Turnover,start =2002 ,end =2020 ,frequency = 365)
#check class of all predictor variables
class(bpcltime1)
class(bpcltime2)
class(bpcltime3)
#import  libraries
library(forecast)
library(tseries)
plot(bpcltime1,main = "Time series plot for bpcl- %deliverable")
plot(bpcltime2,main = "Time series plot for bpcl- VWAP")
plot(bpcltime3,main = "Time series plot for bpcl- Turnover")

#data has to be stationary
# here is a trend
# How to check statioarity->>> check auto correlation
acf(bpcltime1,na.action = na.pass,main="bpcl- %deliverable")
acf(bpcltime2,main="bpcl- VWAP")
acf(bpcltime3,main="bpcl- Turnover")
#deliverable->high correlation with itself
# if the lines cross the bundary blue line the data is considered to be not stationary
# next-> check partial acf (pacf)
pacf(bpcltime1,na.action = na.pass,main="bpcl- %deliverable")
pacf(bpcltime2,main="bpcl- VWAP")
pacf(bpcltime3,main="bpcl- Turnover")
#pacf also tells us that the data is not stationary
# last stationarity test is  adf test
#In statistics and econometrics, an augmented Dickey-Fuller test (ADF) tests the null hypothesis 
#that a unit root is present in a time series sample. 
#The alternative hypothesis is different depending on which version of the test is used, but is usually stationarity or trend-stationarity. 
adf.test(bpcltime1)
adf.test(bpcltime2)
adf.test(bpcltime3)
# p value should be <0.05.. if it is >0.05 it means data is not stationary
#how to convert non stationary data to stationary
#use auto arima function to do that .
bpclmodel1=auto.arima(bpcltime1,ic="aic",trace=TRUE)
bpclmodel2=auto.arima(bpcltime2,ic="aic",trace=TRUE)
bpclmodel3=auto.arima(bpcltime3,ic="aic",trace=TRUE)
# best model arima(pdq ) will be shown
bpclmodel1
bpclmodel2
bpclmodel3
#before forecasting check for stationarity
acf(ts(bpclmodel1$residuals),main="bpcl- %deliverable")
acf(ts(bpclmodel2$residuals),main="bpcl- VWAP")
acf(ts(bpclmodel3$residuals),main="Turnover")
#lines are under control ->indicate data is stationary
pacf(ts(bpclmodel1$residuals),main="bpcl- %deliverable")
pacf(ts(bpclmodel2$residuals),main="bpcl- VWAP")
pacf(ts(bpclmodel3$residuals),main="bpcl-Turnover")
#only less spikes are outside blue dotted line this indicate stationarity
mybpclforecast1=forecast(bpclmodel1,level=c(95),h=1*365)
mybpclforecast2=forecast(bpclmodel2,level=c(95),h=1*365)
mybpclforecast3=forecast(bpclmodel3,level=c(95),h=1*365)
#select the model,confidence interval as 95,h=how many years forecast needed -here next 1year*frequency
mybpclforecast1
mybpclforecast2
mybpclforecast3
plot(mybpclforecast1,main="Forecast of %deliverable for 2021")
plot(mybpclforecast2,main="Forecast of VWAP for 2021")
plot(mybpclforecast3,main="Forecast of Turnover for 2021")
Box.test(mybpclforecast1$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(mybpclforecast1$residuals, lag=10, type= "Ljung-Box")
Box.test(mybpclforecast1$residuals, lag=15, type= "Ljung-Box")
Box.test(mybpclforecast2$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(mybpclforecast2$residuals, lag=10, type= "Ljung-Box")
Box.test(mybpclforecast2$residuals, lag=15, type= "Ljung-Box")

Box.test(mybpclforecast3$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(mybpclforecast3$residuals, lag=10, type= "Ljung-Box")
Box.test(mybpclforecast3$residuals, lag=15, type= "Ljung-Box")
bpcltime5=ts(bpcl$High,start =2002 ,end =2020 ,frequency = 365)
bpcltime6=ts(bpcl$Low,start =2002 ,end =2020 ,frequency = 365)
plot(bpcltime5,type = "l",main="bpcl-High Data Plot",ylab = "High value",xlab="Year")
plot(bpcltime6,type = "l",main="bpcl-Low Data Plot",ylab = "Low value",xlab="Year")
bpcltime7=ts(bpcl$Open,start =2002 ,end =2020 ,frequency = 365)
bpcltime8=ts(bpcl$Close,start =2002 ,end =2020 ,frequency = 365)
plot(bpcltime7,type = "l",main="bpcl-Open Data Plot",ylab = "Open value",xlab="Year")
plot(bpcltime8,type = "l",main="bpcl-Close Data Plot",ylab = "Close value",xlab="Year")

#forecast values
forecast1=mybpclforecast1[["mean"]]
forecast2=mybpclforecast2[["mean"]]
forecast3=mybpclforecast3[["mean"]]
forecast1
forecast2
forecast3
#actual values
actual1=ts(bpcltime1,start=2020,end=2021,frequency=365)
actual2=ts(bpcltime2,start=2020,end=2021,frequency=365)
actual3=ts(bpcltime3,start=2020,end=2021,frequency=365)
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
input <- read.csv("BPCL.csv")  
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
     main="bpcl", sub="", ylab="Close Price", cex=0.6, lwd=2)
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

