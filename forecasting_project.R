    ##################################
    #    Forecasting Project         #
    #  Retail Sales data Rossmann    #
    ##################################

rm(list = ls())

setwd("C:\\Users\\KAug\\Desktop\\Uni\\3. Semester\\Forecasting\\Project")
getwd()

#load libaries
library(vars)
library(ggplot2)
library(ggthemes)
library(reshape) 
library(readxl)
library(tseries)
library(na.tools)
library(grangers)
library(readr)
library(fpp2)

## Load data
data <- read_csv("train.csv")

## Filter for data for Store 1
df <- subset(data, select = c(Date,Sales,Store))
df <- df[(df$Store == 1),]

## Order by date
df <- df[order(as.Date(df$Date, format="%y-/%m-/%d")),]
view(df)

## Create Time Series & plot
sales=ts(df$Sales, start = c(2013,01), frequency = 365)
plot.ts(sales, ylab="Sales in Euros", axes = FALSE) +
  abline(reg = lm(sales~time(sales)))
  axis(1, at=seq(2013,2016, by=1))
  axis(2, at=seq(0,10000, by=1000))
  #On first sight data looks stationary with outliers (seasonality) at the end of the year

Box.test(sales, lag = 1, type = "Ljung")
  # p-value < 0.01 for all kinds of lags: Our data has autocorrelation -> no white Noise
  

##Decomposing the time-series
components_sales <- decompose(sales)
plot(components_sales)
  #Not sure if we should include it as I don't know ow to correctly adapt it in the data
  #We can observe some kind of trend as well as seasonality -> seasonal variation on a weekly level?

###################
#Model-free extrapolation
##################

## The free-hand method: Single exponential smoothing
ses.sales <- ses(sales,
                alpha = .3,
                h = 50,
                initial = "simple")
autoplot(ses.sales)
 #No useful result -> include in paper to show what's not good.

## The free-hand method: Holt-Winters

HW.sales<- HoltWinters(sales)
plot(sales, ylab="Sales") +
      lines(HW.sales$fitted[,1], lty=2, col="blue")
FCHW <- forecast(HW.sales, 150)
plot(FCHW)
  # Very useful result: Still need to make sure I understand the decomposition in additative and multiplicative trend 
 
## Plots for first analysis of time-series
acf(sales, main = "ACF of sales") #interpretation?
pacf(sales, main = "PACF sales")
adf.test(sales) # p-value of 0.01 -> statioarity

## Looking at the data when excluding zero-sales (Sundays and holidays) -> Although not suited for forecasting i thought it might be easier to see a trend here
    #nozeros <- filter(df, Sales != "0")
    #plot(nozeros$Date, nozeros$Sales, type ="l")
    #tsnz <- ts(nozeros$Sales, start = c(2013,01), frequency = 312) #I ued the frequency as its the result of 781/2.5 -> apporx how many values we have per year
    #decomposition <- decompose(tsnz)
    #plot(decomposition)
    #plot.ts(tsnz, ylab = "Sales in Euros")
    #summary(nozeros)
    #acf(tsnz)


## Investigate Seasonalitiy
ggseasonplot(sales) + ggtitle("Seasonal Plot")
 #wow, you can see nothing here

####################################
# SOURCES
# https://rstudio-pubs-static.s3.amazonaws.com/366011_3ee069277eb84547824f8f4022973823.html
#https://rc2e.com/timeseriesanalysis
####################################
