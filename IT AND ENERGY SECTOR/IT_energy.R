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

#HCL
setwd('C:/Users/Admin/Downloads/csv datasets/datasets')
getwd()
hcl <- read.csv("HCLTECH.csv")
#view(hcl) typed in console to view dataset extracted
class(hcl)
#choose predictor variables
hcltime1<-na.omit(hcl$X.Deliverble)
hcltime2<-na.omit(hcl$VWAP)
hcltime3<-na.omit(hcl$Turnover)
hcltime1
hcltime2
hcltime3
hcltime1=ts(hcltime1,start =2002 ,end =2020 ,frequency = 365)
hcltime2=ts(hcl$VWAP,start =2002 ,end =2020 ,frequency = 365)
hcltime3=ts(hcl$Turnover,start =2002 ,end =2020 ,frequency = 365)
#check class of all predictor variables
class(hcltime1)
class(hcltime2)
class(hcltime3)
#import  libraries
library(forecast)
library(tseries)
plot(hcltime1,main = "Time series plot for hcl- %deliverable")
plot(hcltime2,main = "Time series plot for hcl- VWAP")
plot(hcltime3,main = "Time series plot for hcl- Turnover")

#data has to be stationary
# here is a trend
# How to check statioarity->>> check auto correlation
acf(hcltime1,na.action = na.pass,main="hcl- %deliverable")
acf(hcltime2,main="hcl- VWAP")
acf(hcltime3,main="hcl- Turnover")
#deliverable->high correlation with itself
# if the lines cross the bundary blue line the data is considered to be not stationary
# next-> check partial acf (pacf)
pacf(hcltime1,na.action = na.pass,main="hcl- %deliverable")
pacf(hcltime2,main="hcl- VWAP")
pacf(hcltime3,main="hcl- Turnover")
#pacf also tells us that the data is not stationary
# last stationarity test is  adf test
#In statistics and econometrics, an augmented Dickey-Fuller test (ADF) tests the null hypothesis 
#that a unit root is present in a time series sample. 
#The alternative hypothesis is different depending on which version of the test is used, but is usually stationarity or trend-stationarity. 
adf.test(hcltime1)
adf.test(hcltime2)
adf.test(hcltime3)
# p value should be <0.05.. if it is >0.05 it means data is not stationary
#how to convert non stationary data to stationary
#use auto arima function to do that .
hclmodel1=auto.arima(hcltime1,ic="aic",trace=TRUE)
hclmodel2=auto.arima(hcltime2,ic="aic",trace=TRUE)
hclmodel3=auto.arima(hcltime3,ic="aic",trace=TRUE)
# best model arima(pdq ) will be shown
hclmodel1
hclmodel2
hclmodel3
#before forecasting check for stationarity
acf(ts(hclmodel1$residuals),main="hcl- %deliverable")
acf(ts(hclmodel2$residuals),main="hcl- VWAP")
acf(ts(hclmodel3$residuals),main="Turnover")
#lines are under control ->indicate data is stationary
pacf(ts(hclmodel1$residuals),main="hcl- %deliverable")
pacf(ts(hclmodel2$residuals),main="hcl- VWAP")
pacf(ts(hclmodel3$residuals),main="hcl-Turnover")
#only less spikes are outside blue dotted line this indicate stationarity
myhclforecast1=forecast(hclmodel1,level=c(95),h=1*365)
myhclforecast2=forecast(hclmodel2,level=c(95),h=1*365)
myhclforecast3=forecast(hclmodel3,level=c(95),h=1*365)
#select the model,confidence interval as 95,h=how many years forecast needed -here next 1year*frequency
myhclforecast1
myhclforecast2
myhclforecast3
plot(myhclforecast1,main="Forecast of %deliverable for 2021")
plot(myhclforecast2,main="Forecast of VWAP for 2021")
plot(myhclforecast3,main="Forecast of Turnover for 2021")
Box.test(myhclforecast1$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(myhclforecast1$residuals, lag=10, type= "Ljung-Box")
Box.test(myhclforecast1$residuals, lag=15, type= "Ljung-Box")
Box.test(myhclforecast2$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(myhclforecast2$residuals, lag=10, type= "Ljung-Box")
Box.test(myhclforecast2$residuals, lag=15, type= "Ljung-Box")

Box.test(myhclforecast3$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(myhclforecast3$residuals, lag=10, type= "Ljung-Box")
Box.test(myhclforecast3$residuals, lag=15, type= "Ljung-Box")
hcltime5=ts(hcl$High,start =2002 ,end =2020 ,frequency = 365)
hcltime6=ts(hcl$Low,start =2002 ,end =2020 ,frequency = 365)
plot(hcltime5,type = "l",main="hcl-High Data Plot",ylab = "High value",xlab="Year")
plot(hcltime6,type = "l",main="hcl-Low Data Plot",ylab = "Low value",xlab="Year")
hcltime7=ts(hcl$Open,start =2002 ,end =2020 ,frequency = 365)
hcltime8=ts(hcl$Close,start =2002 ,end =2020 ,frequency = 365)
plot(hcltime7,type = "l",main="hcl-Open Data Plot",ylab = "Open value",xlab="Year")
plot(hcltime8,type = "l",main="hcl-Close Data Plot",ylab = "Close value",xlab="Year")

#forecast values
forecast1=myhclforecast1[["mean"]]
forecast2=myhclforecast2[["mean"]]
forecast3=myhclforecast3[["mean"]]
forecast1
forecast2
forecast3
#actual values
actual1=ts(hcltime1,start=2020,end=2021,frequency=365)
actual2=ts(hcltime2,start=2020,end=2021,frequency=365)
actual3=ts(hcltime3,start=2020,end=2021,frequency=365)
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
input <- read.csv("HCLTECH.csv")  
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
     main="hcl", sub="", ylab="Close Price", cex=0.6, lwd=2)
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

#Reliance
setwd('C:/Users/Admin/Downloads/csv datasets/datasets')
getwd()
Reliance <- read.csv("RELIANCE.csv")
#view(Reliance) typed in console to view dataset extracted
class(Reliance)
#choose predictor variables
Reliancetime1<-na.omit(Reliance$X.Deliverble)
Reliancetime2<-na.omit(Reliance$VWAP)
Reliancetime3<-na.omit(Reliance$Turnover)
Reliancetime1
Reliancetime2
Reliancetime3
Reliancetime1=ts(Reliancetime1,start =2002 ,end =2020 ,frequency = 365)
Reliancetime2=ts(Reliance$VWAP,start =2002 ,end =2020 ,frequency = 365)
Reliancetime3=ts(Reliance$Turnover,start =2002 ,end =2020 ,frequency = 365)
#check class of all predictor variables
class(Reliancetime1)
class(Reliancetime2)
class(Reliancetime3)
#import  libraries
library(forecast)
library(tseries)
plot(Reliancetime1,main = "Time series plot for Reliance- %deliverable")
plot(Reliancetime2,main = "Time series plot for Reliance- VWAP")
plot(Reliancetime3,main = "Time series plot for Reliance- Turnover")

#data has to be stationary
# here is a trend
# How to check statioarity->>> check auto correlation
acf(Reliancetime1,na.action = na.pass,main="Reliance- %deliverable")
acf(Reliancetime2,main="Reliance- VWAP")
acf(Reliancetime3,main="Reliance- Turnover")
#deliverable->high correlation with itself
# if the lines cross the bundary blue line the data is considered to be not stationary
# next-> check partial acf (pacf)
pacf(Reliancetime1,na.action = na.pass,main="Reliance- %deliverable")
pacf(Reliancetime2,main="Reliance- VWAP")
pacf(Reliancetime3,main="Reliance- Turnover")
#pacf also tells us that the data is not stationary
# last stationarity test is  adf test
#In statistics and econometrics, an augmented Dickey-Fuller test (ADF) tests the null hypothesis 
#that a unit root is present in a time series sample. 
#The alternative hypothesis is different depending on which version of the test is used, but is usually stationarity or trend-stationarity. 
adf.test(Reliancetime1)
adf.test(Reliancetime2)
adf.test(Reliancetime3)
# p value should be <0.05.. if it is >0.05 it means data is not stationary
#how to convert non stationary data to stationary
#use auto arima function to do that .
Reliancemodel1=auto.arima(Reliancetime1,ic="aic",trace=TRUE)
Reliancemodel2=auto.arima(Reliancetime2,ic="aic",trace=TRUE)
Reliancemodel3=auto.arima(Reliancetime3,ic="aic",trace=TRUE)
# best model arima(pdq ) will be shown
Reliancemodel1
Reliancemodel2
Reliancemodel3
#before forecasting check for stationarity
acf(ts(Reliancemodel1$residuals),main="Reliance- %deliverable")
acf(ts(Reliancemodel2$residuals),main="Reliance- VWAP")
acf(ts(Reliancemodel3$residuals),main="Turnover")
#lines are under control ->indicate data is stationary
pacf(ts(Reliancemodel1$residuals),main="Reliance- %deliverable")
pacf(ts(Reliancemodel2$residuals),main="Reliance- VWAP")
pacf(ts(Reliancemodel3$residuals),main="Reliance-Turnover")
#only less spikes are outside blue dotted line this indicate stationarity
myRelianceforecast1=forecast(Reliancemodel1,level=c(95),h=1*365)
myRelianceforecast2=forecast(Reliancemodel2,level=c(95),h=1*365)
myRelianceforecast3=forecast(Reliancemodel3,level=c(95),h=1*365)
#select the model,confidence interval as 95,h=how many years forecast needed -here next 1year*frequency
myRelianceforecast1
myRelianceforecast2
myRelianceforecast3
plot(myRelianceforecast1,main="Forecast of %deliverable for 2021")
plot(myRelianceforecast2,main="Forecast of VWAP for 2021")
plot(myRelianceforecast3,main="Forecast of Turnover for 2021")
Box.test(myRelianceforecast1$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(myRelianceforecast1$residuals, lag=10, type= "Ljung-Box")
Box.test(myRelianceforecast1$residuals, lag=15, type= "Ljung-Box")
Box.test(myRelianceforecast2$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(myRelianceforecast2$residuals, lag=10, type= "Ljung-Box")
Box.test(myRelianceforecast2$residuals, lag=15, type= "Ljung-Box")

Box.test(myRelianceforecast3$residuals, lag=5, type= "Ljung-Box")
#p>0.05 no auto correlation----box test <0.05 data is still having auto correlation 
#increase lag and check if the value is <0.05
Box.test(myRelianceforecast3$residuals, lag=10, type= "Ljung-Box")
Box.test(myRelianceforecast3$residuals, lag=15, type= "Ljung-Box")
Reliancetime5=ts(Reliance$High,start =2002 ,end =2020 ,frequency = 365)
Reliancetime6=ts(Reliance$Low,start =2002 ,end =2020 ,frequency = 365)
plot(Reliancetime5,type = "l",main="Reliance-High Data Plot",ylab = "High value",xlab="Year")
plot(Reliancetime6,type = "l",main="Reliance-Low Data Plot",ylab = "Low value",xlab="Year")
Reliancetime7=ts(Reliance$Open,start =2002 ,end =2020 ,frequency = 365)
Reliancetime8=ts(Reliance$Close,start =2002 ,end =2020 ,frequency = 365)
plot(Reliancetime7,type = "l",main="Reliance-Open Data Plot",ylab = "Open value",xlab="Year")
plot(Reliancetime8,type = "l",main="Reliance-Close Data Plot",ylab = "Close value",xlab="Year")

#forecast values
forecast1=myRelianceforecast1[["mean"]]
forecast2=myRelianceforecast2[["mean"]]
forecast3=myRelianceforecast3[["mean"]]
forecast1
forecast2
forecast3
#actual values
actual1=ts(Reliancetime1,start=2020,end=2021,frequency=365)
actual2=ts(Reliancetime2,start=2020,end=2021,frequency=365)
actual3=ts(Reliancetime3,start=2020,end=2021,frequency=365)
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
input <- read.csv("RELIANCE.csv")  
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
     main="Reliance", sub="", ylab="Close Price", cex=0.6, lwd=2)
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

