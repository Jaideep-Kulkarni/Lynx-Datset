library(forecast)
library(ggplot2)

data("lynx")
original_dataset<-lynxplot(lynx)

log_dataset<-log(lynx)

plot(log(lynx))
plot(diff(log_dataset))

Acf(log_dataset, lag.max =60)
acf(diff(log_dataset),lag.max=60)

pacf(diff(log_dataset),lag.max=60)
pacf(diff(log_dataset,lag=10),lag.max=60)

acf(diff(log_dataset,lag=10),lag.max=60)

arima(log_dataset,order=c(9,0,0),seasonal=list(order=c(0,1,0),period=10))

pacf(arima(log_dataset,order=c(9,0,0),seasonal=list(order=c(0,1,0),period=10))$resid,lag.max=60)
pacf(arima(log_dataset,order=c(3,0,5),seasonal=list(order=c(0,1,1),period=10))$resid,lag.max=60)
pacf(arima(log_dataset,order=c(2,0,5),seasonal=list(order=c(0,1,1),period=10))$resid,lag.max=60)
pacf(arima(log_dataset,order=c(2,0,4),seasonal=list(order=c(0,1,1),period=10))$resid,lag.max=60)

acf(arima(log_dataset,order=c(2,0,4),seasonal=list(order=c(0,1,1),period=10))$resid,lag.max=60)

arima(log_dataset,order=c(2,0,4),seasonal=list(order=c(0,1,1),period=10))

predicted_values <-exp(predict(arima(log_dataset,order=c(2,0,4),seasonal=list(order=c(0,1,1),period=10)),n.ahead=20)$pred)
predicted_values

autoplot(original_dataset) +autolayer(predicted_values)

