# ACasillas6306HW11
Arturo Casillas  
November 21, 2017  

##1. Warm Up: Brief Financial Data (20%)

###a. Natively in R, you have access to sample data sets of prominent stocks over time. We'll be using EuStockMarkets for this question. Type help(EuStockMarkets) to learn more. From these data, pull specifically the DAX index. For all questions in this assignment, you're welcome to normalize (or don't!) how you see fit, but, if you choose to, please document what you're doing and why for the grader. It's not necessary for the purpose of this assignment.

I look up 'EuStockMarkets' in the help menu and do some very basic data exploration.


```r
#1.a.
#help(EuStockMarkets)
library(datasets)
library(ggplot2)

str(EuStockMarkets)
```

```
##  Time-Series [1:1860, 1:4] from 1991 to 1999: 1629 1614 1607 1621 1618 ...
##  - attr(*, "dimnames")=List of 2
##   ..$ : NULL
##   ..$ : chr [1:4] "DAX" "SMI" "CAC" "FTSE"
```

```r
dim(EuStockMarkets)
```

```
## [1] 1860    4
```

```r
head(EuStockMarkets)
```

```
##          DAX    SMI    CAC   FTSE
## [1,] 1628.75 1678.1 1772.8 2443.6
## [2,] 1613.63 1688.5 1750.5 2460.2
## [3,] 1606.51 1678.6 1718.0 2448.2
## [4,] 1621.04 1684.1 1708.1 2470.4
## [5,] 1618.16 1686.6 1723.1 2484.7
## [6,] 1610.61 1671.6 1714.3 2466.8
```

###b. These are annual European Stock Data from 1990 onward. Create a rudimentary plot of the data. Make the line blue. Give an informative title. Label the axes accurately. In 1997, an event happened you want to indicate; add a vertical red line to your plot which divides pre-1997 and post-1997 information.

Here's a basic plot with the additional colors and markers requested.


```r
#1.b.
plot(EuStockMarkets[,1], col='blue', xlab='Year', ylab='Price of DAX', main='DAX vs Time')
abline(v=1997, col='red')
```

![](ACasillas6306HW11_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

###c. Decompose the time series into its components (i.e., trend, seasonality, random). Keep in mind that this is a multiplicative model you want. Create a plot of all decomposed components. As before, make all lines blue and have a vertical divider at the year 1997.

First, the decomposition and a general plot.


```r
#1.c.
decomp<-decompose(EuStockMarkets[,1])
ls(decomp)
```

```
## [1] "figure"   "random"   "seasonal" "trend"    "type"     "x"
```

```r
plot(decomp, col='blue', xlab='Year', ylab='Price of DAX')
abline(v=1997, col='red')
```

![](ACasillas6306HW11_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Now, a closer look at trend.


```r
#1.c.
plot(decomp$trend, col='blue', xlab='Year', ylab='Price of DAX', main='Trend Component of DAX')
abline(v=1997, col='red')
```

![](ACasillas6306HW11_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

A closer look at the random component


```r
#1.c.
plot(decomp$random, col='blue', xlab='Year', ylab='Price of DAX', main='Random Component of DAX')
abline(v=1997, col='red')
```

![](ACasillas6306HW11_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

A closer look at seasonal


```r
#1.c.
plot(decomp$seasonal, col='blue', xlab='Year', ylab='Price of DAX', main='Seasonal Component of DAX')
abline(v=1997, col='red')
```

![](ACasillas6306HW11_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


##2. Temperature Data (40%)

###a. Using the maxtemp dataset granted by loading fpp2, there are maximum annual temperature data in Celsius. For more information, use help(maxtemp). To see what you're looking at, execute the command in 'Examples' in the help document.

From 'help(maxtemp)', I learn to load the 'fpp2' library. For the 'autoplot(maxtemp)' in the example portion of the documentation, I also learn I have to load 'ggplot2'. Furthermore, I load the 'forecast' library here for the rest of no.2.


```r
#2.a.
#help(maxtemp)
library(forecast)
library(fpp2)
```

```
## Loading required package: fma
```

```
## Loading required package: expsmooth
```

```r
library(ggplot2)
#library(fpp)
autoplot(maxtemp)
```

![](ACasillas6306HW11_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
#ls('package:fpp2')
```

Note, I did not make any modifications to 'autoplot'. I ran the code exactly as in the 'Examples' portion of the documentation per the instruction.

###b. We are only concerned with information after 1990. Please eliminate unwanted information or subset information we care about.

I will use the 'window' function to subest 'maxtemp' but retain it as a time series.


```r
#2.b.
#str(maxtemp)
#maxtempsub<-maxtemp[names(maxtemp) >= 1990]
#Retain time series class
maxtempsub<-window(maxtemp, start=1990)
class(maxtempsub)
```

```
## [1] "ts"
```

###c. Utilize SES to predict the next five years of maximum temperatures in Melbourne. Plot this information, including the prior information and the forecast. Add the predicted value line across 1990-present as a separate line, preferably blue. So, to review, you should have your fit, the predicted value line overlaying it, and a forecast through 2021, all on one axis. Find the AICc of this fitted model. You will use that information later.

First, define my parameters to use for all modeling later.


```r
alph=0.5
betab=0.2
```

Now to perform and plot the simple exponential smoothing with alpha of 0.5. The fitted values are in blue and the predicted values are in violet.


```r
test<-ses(maxtempsub, alpha=alph, initial = 'optimal', h=5)
#test$model
#AICc is in the -$model line
plot(test, type='o', ylab="Max Temperature in Australia", flwd=1, PI = FALSE)
lines(fitted(test), type='o', col='blue2')
lines(test$mean, type='o', col='violetred')
legend('bottomright', lty=1, pch=1, col=c(1, 'blue', 'violetred'), c('original', 'fitted', 'predicted'))
```

![](ACasillas6306HW11_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


###d. Now use a damped Holt's linear trend to also predict out five years. Make sure initial="optimal." As above, create a similar plot to 1C, but use the Holt fit instead.

I do the same as above with alpha of 0.5 and a beta of 0.2. The fitted values are in blue and the predicted values are in violet.


```r
holttest<-holt(maxtempsub, alpha=alph, beta=betab, initial = 'optimal', h=5, damped=TRUE)

plot(holttest, type='o', ylab="Max Temperature in Australia", flwd=1, PI = FALSE)
lines(fitted(holttest), type='o', col='blue2')
lines(holttest$mean, type='o', col='violetred')
legend('bottomright', lty=1, pch=1, col=c(1, 'blue', 'violetred'), c('original', 'fitted', 'predicted'))
```

![](ACasillas6306HW11_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

###e. Compare the AICc of the ses() and holt() models. Which model is better here?

The corrected Aikake information criteria (AICc) can be found in as parts of the 'ses()' and 'holt()' outputs.


```r
#2.e.
#Get AICc
#ls(test$model)
test$model$aicc
```

```
## [1] 142.3879
```

```r
#ls(holttest$model)
holttest$model$aicc
```

```
## [1] 151.9458
```

Since the smaller AICc is preferred, the simple exponential smoothing model is preferred to the holt linear trend model.

##3. The Wands Choose the Wizard (40%)

###a. Utilize the dygraphs library. Read in both Unit11TimeSeries_Ollivander and Unit11TimeSeries_Gregorovitch.csv as two different data frames. They do not have headers, so make sure you account for that. This is a time series of Wands sold over years.

Below, I read in the two datasets and the dygraphs library.


```r
library(dygraphs)
#3.a.
#getwd()
Gregor<-read.csv('C:/Users/acasi/Downloads/Unit11TimeSeries_Gregorovitch.csv', header = FALSE)
Olivander<-read.csv('C:/Users/acasi/Downloads/Unit11TimeSeries_Ollivander.csv', header = FALSE)
```

BTW, I misspelled Ollivander's name.

###b. You don't have your information in the proper format! In both data sets, you'll need to first convert the date-like variable to an actual Date class.

First, I add the names of the data set to make referencing easier.


```r
#3.b.
names(Gregor)<-c('Date', 'Wands')
names(Olivander)<-c('Date', 'Wands')
```

Now, I use '' to convert the date column into an actual date value.


```r
Gregor$Date<-as.Date(Gregor$Date, '%d/%m/%Y')
Olivander$Date<-as.Date(Olivander$Date, '%d/%m/%Y')
```

Double check the success of the conversion.


```r
str(Gregor)
```

```
## 'data.frame':	48 obs. of  2 variables:
##  $ Date : Date, format: "1970-01-01" "1971-01-01" ...
##  $ Wands: int  1268 1295 1349 1298 1493 1432 1431 1291 1247 1403 ...
```

```r
str(Olivander)
```

```
## 'data.frame':	48 obs. of  2 variables:
##  $ Date : Date, format: "1970-01-01" "1971-01-01" ...
##  $ Wands: int  1345 1304 1168 1252 1296 1458 1443 1282 1450 1338 ...
```

###c. Use the library xts (and the xts() function in it) to make each data frame an xts object (effectively, a time series). You'll want to order.by the Date variable.

Load the 'xts' library and use 'xts' with an 'order.by' clause on the date to convert the data to time serires.


```r
#3.c.
library(xts)
```

```
## Loading required package: zoo
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```r
#?xts
Gregor.ts <- xts(Gregor$Wands, order.by = Gregor$Date)
Olivander.ts <- xts(Olivander$Wands, order.by = Olivander$Date)
```

Double check the success of the conversion.


```r
class(Gregor.ts)
```

```
## [1] "xts" "zoo"
```

```r
class(Olivander.ts)
```

```
## [1] "xts" "zoo"
```

###d. Bind the two xts objects together and create a dygraph from it. Utilize the help() index if you're stuck.
####- Give an effective title and x/y axes.
####- Label each Series (via dySeries) to be the appropriate wand-maker. So, one line should create a label for Ollivander and the other for Gregorovitch.
####- Stack this graph and modify the two lines to be different colors (and not the default ones!) Any colors are fine, but make sure they're visible and that Ollivander is a different color than Gregorovitch.
####- Activate a range selector and make it big enough to view.
####- Use dyShading to illuminate approximately when Voldemort was revived and at-large: between 1995 to 1999.
####- Enable Highlighting on the graph, so mousing over a line bolds it.

First bind, then use 'dygraphs' to plot.


```r
#3.d.
#bind
Wands<-cbind.xts(Gregor.ts, Olivander.ts)
#plot
dygraph(Wands, ylab = "Wands", xlab = 'Year', main='Wands Sold by Year: 1970-2017') %>% #<--Title & Axes labels
                                    dyAxis("x",drawGrid=FALSE) %>% 
                                    dySeries("..1", label = "Gregorovitch") %>% #<--Series label
                                    dySeries("..2", label = "Ollivander") %>%   #<--Series label
                                    dyOptions(colors=c("red", 'green')) %>%     #<--Series Colors
                                    dyRangeSelector(height=90, strokeColor = "brown") %>%  #<--Range Selector
                                    dyHighlight(hideOnMouseOut=TRUE) %>% #<--Mouse Highlight
                                    dyShading(from = '1995-1-1', to = '1999-1-1', color = "seashell") #<--dyShading
```

<!--html_preserve--><div id="htmlwidget-a97d66abe0cefe919fce" style="width:672px;height:480px;" class="dygraphs html-widget"></div>
<script type="application/json" data-for="htmlwidget-a97d66abe0cefe919fce">{"x":{"attrs":{"axes":{"x":{"pixelsPerLabel":60,"drawGrid":false,"drawAxis":true},"y":{"drawAxis":true}},"title":"Wands Sold by Year: 1970-2017","xlabel":"Year","ylabel":"Wands","labels":["year","Gregorovitch","Ollivander"],"legend":"auto","retainDateWindow":false,"series":{"Gregorovitch":{"axis":"y"},"Ollivander":{"axis":"y"}},"stackedGraph":false,"fillGraph":false,"fillAlpha":0.15,"stepPlot":false,"drawPoints":false,"pointSize":1,"drawGapEdgePoints":false,"connectSeparatedPoints":false,"strokeWidth":1,"strokeBorderColor":"white","colors":["red","green"],"colorValue":0.5,"colorSaturation":1,"includeZero":false,"drawAxesAtZero":false,"logscale":false,"axisTickSize":3,"axisLineColor":"black","axisLineWidth":0.3,"axisLabelColor":"black","axisLabelFontSize":14,"axisLabelWidth":60,"drawGrid":true,"gridLineWidth":0.3,"rightGap":5,"digitsAfterDecimal":2,"labelsKMB":false,"labelsKMG2":false,"labelsUTC":false,"maxNumberWidth":6,"animatedZooms":false,"mobileDisableYTouch":true,"showRangeSelector":true,"rangeSelectorHeight":90,"rangeSelectorPlotFillColor":" #A7B1C4","rangeSelectorPlotStrokeColor":"brown","interactionModel":"Dygraph.Interaction.defaultModel","highlightCircleSize":3,"highlightSeriesBackgroundAlpha":0.5,"highlightSeriesOpts":[],"hideOverlayOnMouseOut":true},"scale":"yearly","annotations":[],"shadings":[{"from":"1995-01-01T00:00:00.000Z","to":"1999-01-01T00:00:00.000Z","color":"seashell","axis":"x"}],"events":[],"format":"date","data":[["1970-01-01T00:00:00.000Z","1971-01-01T00:00:00.000Z","1972-01-01T00:00:00.000Z","1973-01-01T00:00:00.000Z","1974-01-01T00:00:00.000Z","1975-01-01T00:00:00.000Z","1976-01-01T00:00:00.000Z","1977-01-01T00:00:00.000Z","1978-01-01T00:00:00.000Z","1979-01-01T00:00:00.000Z","1980-01-01T00:00:00.000Z","1981-01-01T00:00:00.000Z","1982-01-01T00:00:00.000Z","1983-01-01T00:00:00.000Z","1984-01-01T00:00:00.000Z","1985-01-01T00:00:00.000Z","1986-01-01T00:00:00.000Z","1987-01-01T00:00:00.000Z","1988-01-01T00:00:00.000Z","1989-01-01T00:00:00.000Z","1990-01-01T00:00:00.000Z","1991-01-01T00:00:00.000Z","1992-01-01T00:00:00.000Z","1993-01-01T00:00:00.000Z","1994-01-01T00:00:00.000Z","1995-01-01T00:00:00.000Z","1996-01-01T00:00:00.000Z","1997-01-01T00:00:00.000Z","1998-01-01T00:00:00.000Z","1999-01-01T00:00:00.000Z","2000-01-01T00:00:00.000Z","2001-01-01T00:00:00.000Z","2002-01-01T00:00:00.000Z","2003-01-01T00:00:00.000Z","2004-01-01T00:00:00.000Z","2005-01-01T00:00:00.000Z","2006-01-01T00:00:00.000Z","2007-01-01T00:00:00.000Z","2008-01-01T00:00:00.000Z","2009-01-01T00:00:00.000Z","2010-01-01T00:00:00.000Z","2011-01-01T00:00:00.000Z","2012-01-01T00:00:00.000Z","2013-01-01T00:00:00.000Z","2014-01-01T00:00:00.000Z","2015-01-01T00:00:00.000Z","2016-01-01T00:00:00.000Z","2017-01-01T00:00:00.000Z"],[1268,1295,1349,1298,1493,1432,1431,1291,1247,1403,1188,1555,1512,1552,1023,1190,1197,1120,1119,1319,1692,1452,1494,1346,1519,1580,1623,1863,0,845,858,814,869,864,942,837,838,671,425,634,618,404,758,410,510,103,49,70],[1345,1304,1168,1252,1296,1458,1443,1282,1450,1338,1063,1230,1237,1291,1211,1442,1649,1629,1260,1283,1617,1284,1399,1272,1297,1666,1797,1620,450,200,1308,1277,1444,1070,1031,1405,1487,1229,1493,1317,1520,1337,1547,1632,1336,1289,1439,1226]],"fixedtz":false,"tzone":"UTC"},"evals":["attrs.interactionModel"],"jsHooks":[]}</script><!--/html_preserve-->

By the way, the link to my github repository is here: https://github.com/ArturoCasillas/Acasillas-SMU-MSDS-homework.git
