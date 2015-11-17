


#***************************************************************
#
#  Time Series
#		
#
#***************************************************************

#***************************************************************
#
#  Packages
#
#***************************************************************


library(forecast)
library(datasets)


#***************************************************************
#  Visualizing time series
#***************************************************************

plot(AirPassengers)

plot(decompose(AirPassengers))

plot(stl(AirPassengers, "periodic"), main = "Airline Passengers")

ts.plot(BJsales, main = "BJ's Wholsale Club", ylab = "Sales")

ts.plot(sunspot.year, main = "Sunspot Flares", ylab = "Number")

ts.plot(precip)

plot(presidents)

plot(lag(sunspots, 1), sunspots, pch = ".")

plot(uspop, main = "U.S. Population", ylab = "Number in Millions")


#***************************************************************
#  Spectral Analysis
#***************************************************************

#Get the periodogram for ham.ts


air.pg <-spec.pgram(AirPassengers,spans=9,demean=T,log='no')

air.pg <-spec.pgram(AirPassengers,spans=3,demean=T,log='no')

##find the peak, max.omega
max.omega.air<-air.pg$freq[which(air.pg$spec==max(air.pg$spec))] 

#where is the peak?
max.omega.air

##What is the period (1/max.omega)?
1/max.omega.air


sunspot.pg <- spec.pgram(sunspot.year, spans = 9, demean = T, log = "no")

##find the peak, max.omega
max.omega.sunspot<-sunspot.pg$freq[which(sunspot.pg$spec==max(sunspot.pg$spec))] 

#where is the peak?
max.omega.sunspot

##What is the period (1/max.omega)?
1/max.omega.sunspot



#***************************************************************
#  ACF and PACF
#***************************************************************


par(mfrow = c(1,2))

acf(AirPassengers)

pacf(AirPassengers)

par(mfrow = c(1,1))

#***************************************************************
#  ARIMA Forecasting
#***************************************************************


air.arima <- auto.arima(AirPassengers)

air.fit <- forecast(air.arima,12)

plot(air.fit)


sunspot.arima <- auto.arima(sunspot.year)

sunspot.fit <- forecast(sunspot.arima, 10)

plot(sunspot.fit)


