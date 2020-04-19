
library(ggfortify)
library(tseries)
library(forecast)

data("co2")

# Take a look at the entries
co2

# Start date, and date and frequency
tsp(co2)

# Check for missing values
sum(is.na(co2))

# Check the cycle of the time series
cycle(co2)

# Review the table summary
summary(co2)

# Plot ts
ylab <- expression(CO[2] ~ (ppm))
autoplot(co2) + labs(x ="Date", y = ylab, title="Mauna Loa CO2 (PPM) from 1959 to 1998")


boxplot(co2~cycle(co2),xlab="Date", ylab = ylab ,main ="Monthly CO2 Boxplot from 1959 to 1998")

# decompose series
decomposeCO2 <- decompose(co2,"additive")
autoplot(decomposeCO2)

# test stationarity
adf.test(co2)

autoplot(acf(co2,plot=FALSE))+ labs(title="Correlogram of CO2 from 1959 to 1998") 
autoplot(pacf(co2,plot=FALSE))+ labs(title="Correlogram of CO2 from 1959 to 1998") 


# remove trend and seasonal component
adf.test(diff(log(co2)), alternative="stationary", k=0)

autoplot(acf(diff(log(co2)),plot=FALSE))+ labs(title="Correlogram of CO2 from 1959 to 1998") 
autoplot(pacf(diff(log(co2)),plot=FALSE))+ labs(title="Correlogram of CO2 from 1959 to 1998") 

## Fit model

# linear model
autoplot(co2) + geom_smooth(method="lm")+ labs(x ="Date", y = ylab, title="Mauna Loa CO2 (PPM) from 1959 to 1998") 

# arima model
arimaCO2 <- auto.arima(co2)
arimaCO2
ggtsdiag(arimaCO2)

# qqnorm is a generic function the default method of which produces a normal QQ plot of the values in y. qqline adds a line to a “theoretical”, by default normal, quantile-quantile plot which passes through the probs quantiles, by default the first and third quartiles.
par(mfrow=c(1,2))
hist(residuals(arimaCO2), main='Mauna Loa CO2 Monthly', xlab='CO2 PPM')
qqnorm(residuals(arimaCO2))
qqline(residuals(arimaCO2))

forecastCO2 <- forecast(arimaCO2, level = c(95), h = 36)
autoplot(forecastCO2)

