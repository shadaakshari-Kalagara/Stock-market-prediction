#IT Sector 
#Infosys
setwd('C:/Users/Admin/Downloads/csv datasets/datasets')
getwd()
infosys <- read.csv("INFY.csv")
#view(infosys) typed in console to view dataset extracted
class(infosys)
#choose predictor variables
infosystime1<-na.omit(infosys$X.Deliverble)
infosystime2<-na.omit(infosys$VWAP)
infosystime3<-na.omit(infosys$Turnover)
infosystime1
infosystime2
infosystime3
infosystime1=ts(infosystime1,start =2002 ,end =2020 ,frequency = 365)
infosystime2=ts(infosys$VWAP,start =2002 ,end =2020 ,frequency = 365)
infosystime3=ts(infosys$Turnover,start =2002 ,end =2020 ,frequency = 365)
#check class of all predictor variables
class(infosystime1)
class(infosystime2)
class(infosystime3)
#import  libraries
library(forecast)
library(tseries)
plot(infosystime1,main = "Time series plot for infosys- %deliverable")
plot(infosystime2,main = "Time series plot for infosys- VWAP")
plot(infosystime3,main = "Time series plot for infosys- Turnover")

#data has to be stationary
# here is a trend
# How to check statioarity->>> check auto correlation
acf(infosystime1,na.action = na.pass,main="infosys- %deliverable")
acf(infosystime2,main="infosys- VWAP")
acf(infosystime3,main="infosys- Turnover")
#deliverable->high correlation with itself
# if the lines cross the bundary blue line the data is considered to be not stationary
# next-> check partial acf (pacf)
pacf(infosystime1,na.action = na.pass,main="infosys- %deliverable")
pacf(infosystime2,main="infosys- VWAP")
pacf(infosystime3,main="infosys- Turnover")
#pacf also tells us that the data is not stationary
# last stationarity test is  adf test
#In statistics and econometrics, an augmented Dickey-Fuller test (ADF) tests the null hypothesis 
#that a unit root is present in a time series sample. 
#The alternative hypothesis is different depending on which version of the test is used, but is usually stationarity or trend-stationarity. 
adf.test(infosystime1)
adf.test(infosystime2)
adf.test(infosystime3)
# p value should be <0.05.. if it is >0.05 it means data is not stationary
#how to convert non stationary data to stationary
#use auto arima function to do that .
infosysmodel1=auto.arima(infosystime1,ic="aic",trace=TRUE)
infosysmodel2=auto.arima(infosystime2,ic="aic",trace=TRUE)
infosysmodel3=auto.arima(infosystime3,ic="aic",trace=TRUE)
# best model arima(pdq ) will be shown
infosysmodel1
infosysmodel2
infosysmodel3
#before forecasting check for stationarity
acf(ts(infosysmodel1$residuals),main="infosys- %deliverable")
acf(ts(infosysmodel2$residuals),main="infosys- VWAP")
acf(ts(infosysmodel3$residuals),main="Turnover")
#lines are under control ->indicate data is stationary
pacf(ts(infosysmodel1$residuals),main="infosys- %deliverable")
pacf(ts(infosysmodel2$residuals),main="infosys- VWAP")
pacf(ts(infosysmodel3$residuals),main="infosys-Turnover")
#only less spikes are outside blue dotted line this indicate stationarity
myinfosysforecast1=forecast(infosysmodel1,level=c(95),h=1*365)
myinfosysforecast2=forecast(infosysmodel2,level=c(95),h=1*365)
myinfosysforecast3=forecast(infosysmodel3,level=c(95),h=1*365)
#select the model,confidence interval as 95,h=how many years forecast needed -here next 1year*frequency
myinfosysforecast1
myinfosysforecast2
myinfosysforecast3
plot(myinfosysforecast1,main="Forecast of %deliverable for 2021")
plot(myinfosysforecast2,main="Forecast of VWAP for 2021")
plot(myinfosysforecast3,main="Forecast of Turnover for 2021")
Box.test(myinfosysforecast1$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(myinfosysforecast1$residuals, lag=10, type= "Ljung-Box")
Box.test(myinfosysforecast1$residuals, lag=15, type= "Ljung-Box")
Box.test(myinfosysforecast2$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(myinfosysforecast2$residuals, lag=10, type= "Ljung-Box")
Box.test(myinfosysforecast2$residuals, lag=15, type= "Ljung-Box")

Box.test(myinfosysforecast3$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(myinfosysforecast3$residuals, lag=10, type= "Ljung-Box")
Box.test(myinfosysforecast3$residuals, lag=15, type= "Ljung-Box")
infosystime5=ts(infosys$High,start =2002 ,end =2020 ,frequency = 365)
infosystime6=ts(infosys$Low,start =2002 ,end =2020 ,frequency = 365)
plot(infosystime5,type = "l",main="infosys-High Data Plot",ylab = "High value",xlab="Year")
plot(infosystime6,type = "l",main="infosys-Low Data Plot",ylab = "Low value",xlab="Year")
infosystime7=ts(infosys$Open,start =2002 ,end =2020 ,frequency = 365)
infosystime8=ts(infosys$Close,start =2002 ,end =2020 ,frequency = 365)
plot(infosystime7,type = "l",main="infosys-Open Data Plot",ylab = "Open value",xlab="Year")
plot(infosystime8,type = "l",main="infosys-Close Data Plot",ylab = "Close value",xlab="Year")

#forecast values
forecast1=myinfosysforecast1[["mean"]]
forecast2=myinfosysforecast2[["mean"]]
forecast3=myinfosysforecast3[["mean"]]
forecast1
forecast2
forecast3
#actual values
actual1=ts(infosystime1,start=2020,end=2021,frequency=365)
actual2=ts(infosystime2,start=2020,end=2021,frequency=365)
actual3=ts(infosystime3,start=2020,end=2021,frequency=365)
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
input <- read.csv("INFY.csv")  
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
     main="infosys", sub="", ylab="Close Price", cex=0.6, lwd=2)
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

