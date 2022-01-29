    ##################################
    # Forecasting Project            #
    #Retail Sales data Rossmann      #
    ##################################

rm(list = ls())

setwd("C:\\Users\\KAug\\Desktop\\Uni\\3. Semester\\Forecasting\\Project")
getwd()

library(tidyverse)
library(tseries)
library(vars)


## Load data

data <- read.csv("C:\\Users\\KAug\\Desktop\\Uni\\3. Semester\\Forecasting\\Project\\train.csv", header = TRUE, sep = ",")

#data$Date <- as.Date(data$Date, format = "%y-/%m-/%d")
#data <- data[order(data$Date),]
#view(data)


## Filter for data of Store 1 & and sort by date

Store1 <- filter(data, Store==1)
class(Store1$Date)
Store1 <- Store1[order(as.Date(Store1$Date, format="%y-/%m-/%d")),]
view(Store1)


## Create Time Series & plot
sales=ts(Store1$Sales, start = c(2013), frequency = 365) # Still wrong (begins with 2015) 
plot.ts(sales, ylab="Sales in Euros") ## High Sales in fall of 2013/2014.... 


##Plots for first analysis of time-series
acf(sales, main = "ACF of sales")
pacf(sales, main = "PACF sales")
adf.test(sales) ## Lag order 9? Doesn't seem to make sense intuitively - ACF and PACF also dnt say much?! 
