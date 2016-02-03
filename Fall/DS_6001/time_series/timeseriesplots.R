setwd("~/MSDS/Fall/DS_6001/time_series")

library(forecast)

births <- read.csv("LiveBirths.txt",header=T)
organs <- read.csv("UStransplant.csv")

organs <- organs[-which(organs$Year==2010),]

births.ts <- ts(births[,1],frequency=12,start=c(1948,1))
organs.ts <- ts(organs$All_Organs,frequency = 1,start(1988,1))

ts.plot(births.ts)
ts.plot(organs.ts)

spec.pgram(births.ts,spans=10,demean=T,log='no')
spec.pgram(organs.ts,spans=3,demean=T,log='no')


par(mfrow = c(1,2))
acf(births.ts)
pacf(births.ts)
par(mfrow = c(1,1))

par(mfrow = c(1,2))
acf(organs.ts)
pacf(organs.ts)
par(mfrow = c(1,1))


birth.arima <- auto.arima(births.ts)
birth.fit <- forecast(birth.arima,12)
plot(birth.fit)

organ.arima <- auto.arima(organs.ts)
organ.fit <- forecast(organ.arima,2)
plot(organ.fit)
