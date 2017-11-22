#1.a.
help(EuStockMarkets)
library(datasets)
library(ggplot2)

str(EuStockMarkets)
dim(EuStockMarkets)
head(EuStockMarkets)

#1.a.
plot(EuStockMarkets[,1], col='blue', xlab='Year', ylab='Price of DAX', main='DAX vs Time')
abline(v=1997, col='red')

#1.c.
decomp<-decompose(EuStockMarkets[,1])
ls(decomp)

plot(decomp, col='blue', xlab='Year', ylab='Price of DAX')
abline(v=1997, col='red')

plot(decomp$trend, col='blue', xlab='Year', ylab='Price of DAX', main='Trend Component of DAX')
abline(v=1997, col='red')

plot(decomp$random, col='blue', xlab='Year', ylab='Price of DAX', main='Random Component of DAX')
abline(v=1997, col='red')

plot(decomp$seasonal, col='blue', xlab='Year', ylab='Price of DAX', main='Seasonal Component of DAX')
abline(v=1997, col='red')


#2.
#install.packages('fpp2')

#2.a.
library(forecast)
library(fpp2)
library(ggplot2)
#library(fpp)
help(maxtemp)
autoplot(maxtemp)
ls('package:fpp2')

#2.b.
str(maxtemp)
#maxtempsub<-maxtemp[names(maxtemp) >= 1990]
#Retain time series class
maxtempsub<-window(maxtemp, start=1990)
class(maxtempsub)

#2.c.
?ses
?forecast

#plot SES
alph=0.5
betab=0.2
test<-ses(maxtempsub, alpha=alph, initial = 'optimal', h=5)
#test$model
#AICc is in the -$model line

#class(test)
#ls(test)
#length(maxtempsub)
#class(maxtempsub)
plot(test, PI = TRUE)

#par(mfrow=c(2,1))
plot(test, type='o', ylab="Max Temperature in Australia", flwd=1, PI = FALSE)
lines(fitted(test), type='o', col='blue2')
lines(test$mean, type='o', col='violetred')
legend('bottomright', lty=1, pch=1, col=c(1, 'blue', 'violetred'), c('original', 'fitted', 'predicted'))

#2.d.
#plot holt
holttest<-holt(maxtempsub, alpha=alph, beta=betab, initial = 'optimal', h=5, damped=TRUE)

plot(holttest, type='o', ylab="Max Temperature in Australia", flwd=1, PI = FALSE)
lines(fitted(holttest), type='o', col='blue2')
lines(holttest$mean, type='o', col='violetred')
legend('bottomright', lty=1, pch=1, col=c(1, 'blue', 'violetred'), c('original', 'fitted', 'predicted'))

#2.e.
#Get AICc
test$model$aicc
#ls(test$model)
holttest$model$aicc


#3.
library(dygraphs)
#3.a.
#getwd()
Gregor<-read.csv('C:/Users/acasi/Downloads/Unit11TimeSeries_Gregorovitch.csv', header = FALSE)
Olivander<-read.csv('C:/Users/acasi/Downloads/Unit11TimeSeries_Ollivander.csv', header = FALSE)

#3.b.
#str(Gregor)
#str(Olivander)
names(Gregor)<-c('Date', 'Wands')
names(Olivander)<-c('Date', 'Wands')
Gregor$Date<-as.Date(Gregor$Date, '%d/%m/%Y')
Olivander$Date<-as.Date(Olivander$Date, '%d/%m/%Y')

str(Gregor)
str(Olivander)

#3.c.
library(xts)
#?xts
Gregor.ts <- xts(Gregor$Wands, order.by = Gregor$Date)
Olivander.ts <- xts(Olivander$Wands, order.by = Olivander$Date)
class(Gregor.ts)
class(Olivander.ts)

#3.d.
Wands<-cbind.xts(Gregor.ts, Olivander.ts)

dygraph(Wands, ylab = "Wands" ) %>% dyAxis("x",drawGrid=FALSE) %>% 
                                    dySeries("..1", label = "Gregorovitch") %>% 
                                    dySeries("..2", label = "Ollivander") %>% 
                                    dyOptions(colors=c("red", 'green')) %>% 
                                    dyRangeSelector(height=100, strokeColor = "brown") %>%
                                    dyHighlight(hideOnMouseOut) %>% 
                                    dyShading(from = "1995-1-1", to = "1999-1-1", color = "seashell")

?arima
wine.fit <- hw(wineind,h=48)
plot(wine.fit)
