library(quantmod)
library(moments)
library(PerformanceAnalytics)
library(tseries)
library(tidyverse)
library(xts)
library(zoo)

NGSEINS10 <- read.csv("C:/Users/hp/Documents/R/Rscript/Mrs Anulika/Enjoy Thesis R script - Edited/NSE Insurance Historical Rates - Investing.com NG.csv")
str(NGSEINS10)

### Converting Date Varible From Factor to Character then to Date
NGSEINS10$Date<- as.Date(NGSEINS10$Date,format = "%m/%d/%Y")

### Converting the Volume variable from factor to Numeric
NGSEINS10$Vol.<- as.numeric(NGSEINS10$Vol.) 

### Save the Date Variable as Vector
timedate  <- NGSEINS10$Date

### Removing Date Variable and Rearranging our Variables
NGSEINS10<- NGSEINS10[-1]
NGSEINS10<- NGSEINS10[c(2,3,4,1,5,6)]

### Converting to zoo for time series Analysis
NGSEINS10 <- zoo(NGSEINS10,order.by=timedate)

### Converting to Xts
NGSEINS10 <- as.xts(NGSEINS10)

chartSeries(NGSEINS10$Price,type="line",name="Closing Stock Price")

#######  Daily stock Returns
##  ISEQ
dr.ngseins10<-dailyReturn(NGSEINS10,type="log")
colnames(dr.ngseins10)<- "NGSEINS10"

# Chart of Daily Returns
chartSeries(dr.ngseins10)

############################# Descriptive Summary
table.Stats(dr.ngseins10,digits=7)

#### Density Distribution
chart.Histogram(dr.ngseins10,methods = "add.density",main = "Density Plot for ISEQ")

# Test for normality
jarque.test(as.vector(dr.ngseins10))

####### Weak random walk Test
## Unit Root test
adf.test(NGSEINS10$Price, alternative = "stationary",k=15)



####################### Serial correlation test
Box.test(dr.ngseins10,lag=15,type=("Ljung-Box"))
chart.ACF(dr.ngseins10)

##################### Run test
detach("package:tseries", unload=TRUE)### removing the t series package
library(randtests)

runs.test(as.vector(dr.ngseins10),"two.sided")



