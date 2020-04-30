install.packages("tseries")
install.packages("forecast")
install.packages("TSA")
install.packages("xts")
install.packages("ggplot2")

library(forecast)
library(TSA) 
library(xts)
library(astsa)
library(ggplot2)
library(tseries)


Delivery <- read.csv("/Users/huanglin/Desktop/Pitney bowes/Delivery_Volume.csv")
delivery_level<-Delivery[,'DELIVERED_VOLUME']
delivery_level[is.na(delivery_level)] <- 0
head(delivery_level)
plot.ts(delivery_level)



BoxCox.lambda(delivery_level)# 1 no transfer is preferable

d_del=diff(delivery_level)
plot.ts(d_del)

#test stationarity----do not need difference
adf.test(delivery_level)#small p-value stationary
adf.test(d_del)
Box.test(delivery_level,lag = 50,type = "Ljung-Box")
Box.test(d_del,lag = 50,type = "Ljung-Box")

auto.arima(delivery_level) #ARIMA(1,0,3) with non-zero mean 

par(mfrow=c(2,1))
acf(delivery_level,40,main='ACF')
pacf(delivery_level,40,main='PACF')

# with frequency =7
sdel<- ts(delivery_level,frequency=7) 
modArima <- auto.arima(sdel)
print(modArima) # ARIMA(0,1,1)(0,1,0)[7]


model1<-arima(sdel, order=c(0,1,1),seasonal = list(order=c(0,1,0),period=7))
print(model1)
sarima(sdel,0,1,1,0,1,0,7) #dignostic
sdel_fit$ttable

#without frequency

modArima2 <- auto.arima(delivery_level)
print(modArima2) #ARIMA(1,0,3) with non-zero mean 

model2<-arima(delivery_level, order=c(1,0,3))
sarima(delivery_level,1,0,3)


#forecast
sarima.for(delivery_level,5,0,1,1,0,1,0,7)
fore1 = predict (model1,5)
print(fore1) # -104.9938 2221.0062 1293.0062  523.0062  887.0062


fore2 = predict(modArima2,5)
print(fore2) # 904.8723 1102.5842  565.7957  566.6609  567.4824
ts.plot(as.ts(delivery_level),fore2$pred)
