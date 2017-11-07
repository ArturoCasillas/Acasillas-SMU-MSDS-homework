# S&P 500 BLT
Arturo Casillas  
November 7, 2017  

Bring in the data and calculate log-returns


```r
library(tseries)

## S&P 500 (^GSPC)
###    SNP - SNP Real Time Price. Currency in USD

# TODO: Download the data of SP500 '^gspc'.
SNPdata <- get.hist.quote('^gspc',quote="Close")
```

```
## 'getSymbols' currently uses auto.assign=TRUE by default, but will
## use auto.assign=FALSE in 0.5-0. You will still be able to use
## 'loadSymbols' to automatically load data. getOption("getSymbols.env")
## and getOption("getSymbols.auto.assign") will still be checked for
## alternate defaults.
## 
## This message is shown once per session and may be disabled by setting 
## options("getSymbols.warning4.0"=FALSE). See ?getSymbols for details.
```

```
## 
## WARNING: There have been significant changes to Yahoo Finance data.
## Please see the Warning section of '?getSymbols.yahoo' for details.
## 
## This message is shown once per session and may be disabled by setting
## options("getSymbols.yahoo.warning"=FALSE).
```

```
## time series ends   2017-11-03
```

```r
# TODO: Calculate the log returns, which is the subtraction of log(lag(SNPdata)) and log(SNPdata)

# dim(SNPdata)
# SNPret<-data.frame(rep(0, length(SNPdata)))
# 
# for(i in 2:length(SNPdata)){
#   SNPret[i]=log(SNPret[i+1]/SNPret[i])
# }
# dim(SNPret)

SNPret <- log(SNPdata) - log(lag(SNPdata))
dim(SNPret)
```

```
## [1] 6764    1
```

Calculate volatility and create a function that calculates 'local' volatility


```r
# TODO: Calculate volatility measure that is to multiply sd(SNPret),sqrt(250), 100
SNPvol <-  sd(SNPret)*sqrt(250)*100


## Define getVol function for volatility
getVol <- function(d, logrets) {
	var = 0
	lam = 0
	varlist <- c()

	for (r in logrets) {
		lam = lam*(1 - 1/d) + 1
	  var = (1 - 1/lam)*var + (1/lam)*r^2
		varlist <- c(varlist, var)
	}

	sqrt(varlist)
}
```

Apply our function to get volatility graphs and then plot


```r
# TODO: call getVol function with the parameters: 10,SNPret
volest <- getVol(10, SNPret)

# TODO: call getVol function with the parameters: 30,SNPret
volest2 <- getVol(30, SNPret)

# TODO: call getVol function with the parameters: 100,SNPret
volest3 <- getVol(100, SNPret)

# Plot the results, overlaying the volatility curves on the data, just as was done in the S&P example.
plot(volest,type="l")

# TODO: Add connected line segments for volest2 with the parameters: type="l",col="red"
lines(volest2, type="l",col="red")

# TODO: Add connected line segments for volest3 with the parameters: type="l",col="blue"
lines(volest3, type="l",col="blue")
```

![](ACasillasSP500_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
