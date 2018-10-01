
#FORECASTING OF EURO-AUD EXCHANGE RATES WITH THE WITH THE ARIMA AND STL(Seasonal and Trend decomposition using Loess) MODELS
#The data used are historical currency exchange rates from January 1999 to June 2014 provided by the European Central Bank.

#******** Forecasting functions STL and ARIMA. You can move this to a different file as a best coding practice and then refer back in the main program.

library(forecast)


## Forecast with STL model
forecastStl <- function(x, n.ahead=30){
  myTs <- ts(x$AUD, start=1, frequency=256)
  fit.stl <- stl(myTs, s.window=256)
  sts <- fit.stl$time.series
  trend <- sts[,"trend"]
  fore <- forecast(fit.stl, h=n.ahead, level=95)
  plot(fore)
  pred <- fore$mean
  upper <- fore$upper
  lower <- fore$lower
  output <- data.frame(actual = c(x$AUD, rep(NA, n.ahead)),
                       trend = c(trend, rep(NA, n.ahead)),
                       #pred = c(trend, pred),
                       pred = c(rep(NA, nrow(x)), pred),
                       lower = c(rep(NA, nrow(x)), lower),                       
                       upper = c(rep(NA, nrow(x)), upper),                       
                       date = c(x$Date, max(x$Date) + (1:n.ahead))
                       )
  return(output)
}


## Forecast with ARIMA model
forecastArima <- function(x, n.ahead=30){
  myTs <- ts(x$AUD, start=1, frequency=256)
  fit.arima <- arima(myTs, order=c(0,0,1))
  fore <- forecast(fit.arima, h=n.ahead)
  plot(fore)
  upper <- fore$upper[,'95%']
  lower <- fore$lower[,'95%']
  trend <- as.numeric(fore$fitted)
  pred <- as.numeric(fore$mean)
  output <- data.frame(actual = c(x$AUD, rep(NA, n.ahead)),
                       trend = c(trend, rep(NA, n.ahead)),
                       #pred = c(trend, pred),
                       pred = c(rep(NA, nrow(x)), pred),
                       lower = c(rep(NA, nrow(x)), lower),                       
                       upper = c(rep(NA, nrow(x)), upper),                       
                       date = c(x$Date, max(x$Date) + (1:n.ahead))  
                       )
  return(output)
}


#******************************Plot forecast graph**********************
plotForecastResult <- function(x, title=NULL) {
  x <- x[order(x$date),]
  max.val <- max(c(x$actual, x$upper), na.rm=T)
  min.val <- min(c(x$actual, x$lower), na.rm=T)
  plot(x$date, x$actual, type="l", col="grey", main=title,
       xlab="Time", ylab="Exchange Rate",
       xlim=range(x$date), ylim=c(min.val, max.val))
  grid()
  lines(x$date, x$trend, col="yellowgreen")
  lines(x$date, x$pred, col="green")
  lines(x$date, x$lower, col="blue")
  lines(x$date, x$upper, col="blue")
  legend("bottomleft", col=c("grey", "yellowgreen", "green", "blue"), lty=1,
         c("Actual", "Trend", "Forecast", "Lower/Upper Bound"))
}

#******************************************************************
#*****************FORECASTING MODEL********************************
#******************************************************************
Path = "C:\\Users\\btrip1\\Desktop\\TS Forecast demo\\eurofxref-hist\\eurofxref-hist.csv" #Change this path to your local
rates <- read.csv(Path)

rates[1:2, ]
str(rates$Date)
##  Factor w/ 3968 levels "1999-01-04","1999-01-05",..: 3968 3967 3966 3965 3964 3963 3962 3961 3960 3959 ...
## convert into date format
rates$Date <- as.Date(rates$Date, "%Y-%m-%d")
str(rates$Date)
##  Date[1:3968], format: "2014-07-01" "2014-06-30" "2014-06-27" "2014-06-26" ...
range(rates$Date)
## [1] "1999-01-04" "2014-07-01"
rates <- rates[order(rates$Date), ]
## plot time series
plot(rates$Date, rates$AUD, type = "l")

#ARIMA Forecast
head(rates$Date, 20)

years <- format(rates$Date, "%Y")
tab <- table(years)
tab

## number of days per year after removing 2014
mean(tab[1:(length(tab) - 1)])


result.arima <- forecastArima(rates, n.ahead = 90)

plotForecastResult(result.arima, title = "Exchange rate forecasting with ARIMA")

result.stl <- forecastStl(rates, n.ahead = 90)

plotForecastResult(result.stl, title = "Exchange rate forecasting with STL")

## exchange rate in 2014
result <- subset(result.stl, date >= "2014-01-01")
plotForecastResult(result, title = "Exchange rate forecasting with STL (2014)")

