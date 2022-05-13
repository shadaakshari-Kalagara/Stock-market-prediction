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


#Mm
setwd('D:/DA&V/proj/bollinger')
getwd()
mm <- read.csv("MM.csv")
#view(mm) typed in console to view dataset extracted
class(mm)
#choose predictor variables
mmtime1<-na.omit(mm$X.Deliverble)
mmtime2<-na.omit(mm$VWAP)
mmtime3<-na.omit(mm$Turnover)
mmtime1
mmtime2
mmtime3
mmtime1=ts(mmtime1,start =2002 ,end =2020 ,frequency = 365)
mmtime2=ts(mm$VWAP,start =2002 ,end =2020 ,frequency = 365)
mmtime3=ts(mm$Turnover,start =2002 ,end =2020 ,frequency = 365)
#check class of all predictor variables
class(mmtime1)
class(mmtime2)
class(mmtime3)
#import  libraries
library(forecast)
library(tseries)
plot(mmtime1,main = "Time series plot for MM- %deliverable")
plot(mmtime2,main = "Time series plot for MM- VWAP")
plot(mmtime3,main = "Time series plot for MM- Turnover")

#data has to be stationary
# here is a trend
# How to check statioarity->>> check auto correlation
acf(mmtime1,na.action = na.pass,main="MM- %deliverable")
acf(mmtime2,main="MM- VWAP")
acf(mmtime3,main="MM- Turnover")
#deliverable->high correlation with itself
# if the lines cross the bundary blue line the data is considered to be not stationary
# next-> check partial acf (pacf)
pacf(mmtime1,na.action = na.pass,main="MM- %deliverable")
pacf(mmtime2,main="MM- VWAP")
pacf(mmtime3,main="MM- Turnover")
#pacf also tells us that the data is not stationary
# last stationarity test is  adf test
#In statistics and econometrics, an augmented Dickey-Fuller test (ADF) tests the null hypothesis 
#that a unit root is present in a time series sample. 
#The alternative hypothesis is different depending on which version of the test is used, but is usually stationarity or trend-stationarity. 
adf.test(mmtime1)
adf.test(mmtime2)
adf.test(mmtime3)
# p value should be <0.05.. if it is >0.05 it means data is not stationary
#how to convert non stationary data to stationary
#use auto arima function to do that .
mmmodel1=auto.arima(mmtime1,ic="aic",trace=TRUE)
mmmodel2=auto.arima(mmtime2,ic="aic",trace=TRUE)
mmmodel3=auto.arima(mmtime3,ic="aic",trace=TRUE)
# best model arima(pdq ) will be shown
mmmodel1
mmmodel2
mmmodel3
#before forecasting check for stationarity
acf(ts(mmmodel1$residuals),main="MM- %deliverable")
acf(ts(mmmodel2$residuals),main="MM- VWAP")
acf(ts(mmmodel3$residuals),main="Turnover")
#lines are under control ->indicate data is stationary
pacf(ts(mmmodel1$residuals),main="MM- %deliverable")
pacf(ts(mmmodel2$residuals),main="MM- VWAP")
pacf(ts(mmmodel3$residuals),main="MM-Turnover")
#only less spikes are outside blue dotted line this indicate stationarity
mymmforecast1=forecast(mmmodel1,level=c(95),h=1*365)
mymmforecast2=forecast(mmmodel2,level=c(95),h=1*365)
mymmforecast3=forecast(mmmodel3,level=c(95),h=1*365)
#select the model,confidence interval as 95,h=how many years forecast needed -here next 1year*frequency
mymmforecast1
mymmforecast2
mymmforecast3
plot(mymmforecast1,main="Forecast of %deliverable for 2021")
plot(mymmforecast2,main="Forecast of VWAP for 2021")
plot(mymmforecast3,main="Forecast of Turnover for 2021")
Box.test(mymmforecast1$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(mymmforecast1$residuals, lag=10, type= "Ljung-Box")
Box.test(mymmforecast1$residuals, lag=15, type= "Ljung-Box")
Box.test(mymmforecast2$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(mymmforecast2$residuals, lag=10, type= "Ljung-Box")
Box.test(mymmforecast2$residuals, lag=15, type= "Ljung-Box")

Box.test(mymmforecast3$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(mymmforecast3$residuals, lag=10, type= "Ljung-Box")
Box.test(mymmforecast3$residuals, lag=15, type= "Ljung-Box")
mmtime5=ts(mm$High,start =2002 ,end =2020 ,frequency = 365)
mmtime6=ts(mm$Low,start =2002 ,end =2020 ,frequency = 365)
plot(mmtime5,type = "l",main="Mm-High Data Plot",ylab = "High value",xlab="Year")
plot(mmtime6,type = "l",main="Mm-Low Data Plot",ylab = "Low value",xlab="Year")
mmtime7=ts(mm$Open,start =2002 ,end =2020 ,frequency = 365)
mmtime8=ts(mm$Close,start =2002 ,end =2020 ,frequency = 365)
plot(mmtime7,type = "l",main="Mm-Open Data Plot",ylab = "Open value",xlab="Year")
plot(mmtime8,type = "l",main="Mm-Close Data Plot",ylab = "Close value",xlab="Year")

#forecast values
forecast1=mymmforecast1[["mean"]]
forecast2=mymmforecast2[["mean"]]
forecast3=mymmforecast3[["mean"]]
forecast1
forecast2
forecast3
#actual values
actual1=ts(mmtime1,start=2020,end=2021,frequency=365)
actual2=ts(mmtime2,start=2020,end=2021,frequency=365)
actual3=ts(mmtime3,start=2020,end=2021,frequency=365)
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
input <- read.csv("MM.csv")  
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
     main="MM", sub="", ylab="Close Price", cex=0.6, lwd=2)
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


#Tatamotors
setwd('D:/DA&V/proj/bollinger')
getwd()
tatamotors <- read.csv("TATAMOTORS.csv")
#view(tatamotors) typed in console to view dataset extracted
class(tatamotors)
#choose predictor variables
tatamotorstime1<-na.omit(tatamotors$X.Deliverble)
tatamotorstime2<-na.omit(tatamotors$VWAP)
tatamotorstime3<-na.omit(tatamotors$Turnover)
tatamotorstime1
tatamotorstime2
tatamotorstime3
tatamotorstime1=ts(tatamotorstime1,start =2002 ,end =2020 ,frequency = 365)
tatamotorstime2=ts(tatamotors$VWAP,start =2002 ,end =2020 ,frequency = 365)
tatamotorstime3=ts(tatamotors$Turnover,start =2002 ,end =2020 ,frequency = 365)
#check class of all predictor variables
class(tatamotorstime1)
class(tatamotorstime2)
class(tatamotorstime3)
#import  libraries
library(forecast)
library(tseries)
plot(tatamotorstime1,main = "Time series plot for TATAMOTORS- %deliverable")
plot(tatamotorstime2,main = "Time series plot for TATAMOTORS- VWAP")
plot(tatamotorstime3,main = "Time series plot for TATAMOTORS- Turnover")

#data has to be stationary
# here is a trend
# How to check statioarity->>> check auto correlation
acf(tatamotorstime1,na.action = na.pass,main="TATAMOTORS- %deliverable")
acf(tatamotorstime2,main="TATAMOTORS- VWAP")
acf(tatamotorstime3,main="TATAMOTORS- Turnover")
#deliverable->high correlation with itself
# if the lines cross the bundary blue line the data is considered to be not stationary
# next-> check partial acf (pacf)
pacf(tatamotorstime1,na.action = na.pass,main="TATAMOTORS- %deliverable")
pacf(tatamotorstime2,main="TATAMOTORS- VWAP")
pacf(tatamotorstime3,main="TATAMOTORS- Turnover")
#pacf also tells us that the data is not stationary
# last stationarity test is  adf test
#In statistics and econometrics, an augmented Dickey-Fuller test (ADF) tests the null hypothesis 
#that a unit root is present in a time series sample. 
#The alternative hypothesis is different depending on which version of the test is used, but is usually stationarity or trend-stationarity. 
adf.test(tatamotorstime1)
adf.test(tatamotorstime2)
adf.test(tatamotorstime3)
# p value should be <0.05.. if it is >0.05 it means data is not stationary
#how to convert non stationary data to stationary
#use auto arima function to do that .
tatamotorsmodel1=auto.arima(tatamotorstime1,ic="aic",trace=TRUE)
tatamotorsmodel2=auto.arima(tatamotorstime2,ic="aic",trace=TRUE)
tatamotorsmodel3=auto.arima(tatamotorstime3,ic="aic",trace=TRUE)
# best model arima(pdq ) will be shown
tatamotorsmodel1
tatamotorsmodel2
tatamotorsmodel3
#before forecasting check for stationarity
acf(ts(tatamotorsmodel1$residuals),main="TATAMOTORS- %deliverable")
acf(ts(tatamotorsmodel2$residuals),main="TATAMOTORS- VWAP")
acf(ts(tatamotorsmodel3$residuals),main="Turnover")
#lines are under control ->indicate data is stationary
pacf(ts(tatamotorsmodel1$residuals),main="TATAMOTORS- %deliverable")
pacf(ts(tatamotorsmodel2$residuals),main="TATAMOTORS- VWAP")
pacf(ts(tatamotorsmodel3$residuals),main="TATAMOTORS-Turnover")
#only less spikes are outside blue dotted line this indicate stationarity
mytatamotorsforecast1=forecast(tatamotorsmodel1,level=c(95),h=1*365)
mytatamotorsforecast2=forecast(tatamotorsmodel2,level=c(95),h=1*365)
mytatamotorsforecast3=forecast(tatamotorsmodel3,level=c(95),h=1*365)
#select the model,confidence interval as 95,h=how many years forecast needed -here next 1year*frequency
mytatamotorsforecast1
mytatamotorsforecast2
mytatamotorsforecast3
plot(mytatamotorsforecast1,main="Forecast of %deliverable for 2021")
plot(mytatamotorsforecast2,main="Forecast of VWAP for 2021")
plot(mytatamotorsforecast3,main="Forecast of Turnover for 2021")
Box.test(mytatamotorsforecast1$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(mytatamotorsforecast1$residuals, lag=10, type= "Ljung-Box")
Box.test(mytatamotorsforecast1$residuals, lag=15, type= "Ljung-Box")
Box.test(mytatamotorsforecast2$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(mytatamotorsforecast2$residuals, lag=10, type= "Ljung-Box")
Box.test(mytatamotorsforecast2$residuals, lag=15, type= "Ljung-Box")

Box.test(mytatamotorsforecast3$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(mytatamotorsforecast3$residuals, lag=10, type= "Ljung-Box")
Box.test(mytatamotorsforecast3$residuals, lag=15, type= "Ljung-Box")
tatamotorstime5=ts(tatamotors$High,start =2002 ,end =2020 ,frequency = 365)
tatamotorstime6=ts(tatamotors$Low,start =2002 ,end =2020 ,frequency = 365)
plot(tatamotorstime5,type = "l",main="Tatamotors-High Data Plot",ylab = "High value",xlab="Year")
plot(tatamotorstime6,type = "l",main="Tatamotors-Low Data Plot",ylab = "Low value",xlab="Year")
tatamotorstime7=ts(tatamotors$Open,start =2002 ,end =2020 ,frequency = 365)
tatamotorstime8=ts(tatamotors$Close,start =2002 ,end =2020 ,frequency = 365)
plot(tatamotorstime7,type = "l",main="Tatamotors-Open Data Plot",ylab = "Open value",xlab="Year")
plot(tatamotorstime8,type = "l",main="Tatamotors-Close Data Plot",ylab = "Close value",xlab="Year")

#forecast values
forecast1=mytatamotorsforecast1[["mean"]]
forecast2=mytatamotorsforecast2[["mean"]]
forecast3=mytatamotorsforecast3[["mean"]]
forecast1
forecast2
forecast3
#actual values
actual1=ts(tatamotorstime1,start=2020,end=2021,frequency=365)
actual2=ts(tatamotorstime2,start=2020,end=2021,frequency=365)
actual3=ts(tatamotorstime3,start=2020,end=2021,frequency=365)
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
input <- read.csv("TATAMOTORS.csv")  
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
     main="TATAMOTORS", sub="", ylab="Close Price", cex=0.6, lwd=2)
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





