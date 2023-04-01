#load required r packages
rm(list=ls()) #Removes all items in Environment!
library(forecast)
library(tidyverse)
library(uroot)
library("tseries")
library(nortest)
#------------------------------------------------------------------------
# Load data
# The dataset contains the Monthly Unemployment Rate between January 1976 to January 2020.
df<- read.csv("D:\\Projects\\Intro to time series\\project materials\\Data\\Mass Monthly Unemployment Rate.csv")
head(df);dim(df)
#This dataset contains two columns and 529 rows.
#The first column is the date column and
#the second one is the monthly unemployment rate.
#check date class
class(df$DATE)
#The date column type isn't date class.
#So,I will transform the date class to date type.
#change date class to date type
df$DATE<-as.Date(df$DATE)
#check date class
class(df$DATE)
#------------------------------------------------------------------------
#Checking for Stationary
#-----------------------------
# First way
#---------------
#check time series plot
ggplot(df,aes(x=DATE,y=MAURN))+geom_line()
#From the above plot, I can see that generally the values don't appear super well bounded and the mean doesn't appear constant over time.
#And this a strong evidence that this time series data isn't stationary.
#-----------------------------------------------------------------------
# Second way
#---------------
#check ACF plot
ggAcf(df$MAURN,type ="correlation")
#I can see that the ACF plot doesn't die off quickly and go to zero.
#Instead it's very high across a large number of lags in time series.
#Again that is strong evidence that the time series data are non-stationary.
#----------------------------------------------------------------------
# Third way
#---------------
#run ADF test
adf.test(df$MAURN)
#p-value > 0.05 so we accept the null hypothesis that said the series is non-stationary.
#we know now that the series is non stationary so we need to transform it.
#--------------------------------------------------------------------------
#- Box-Jenkins Approach
#--------------------------------
#step(1): Model Identification
#step(2): Model Estimation
#step(3): Model Checking
#step(4): Forecasting
#--------------------------------
#step(1): Model Identification
#--------------------------
# 1- Finding d
#--------------------------
D.X = diff(df$MAURN, lag=1)
# plot the data with and without difference
windows() ; par(mfrow=c(3,1))
plot.ts (df$MAURN,col =2  ) ; plot.ts(D.X)
# the data with difference = 1, has constant mean and become stationary.
# Use Augmented Dickey-Fuller Test to test whether a given Time series is stationary or not.
adf.test(df$MAURN)
# the p-value > 0.05 so we accept the null hypothesis(the series is non-stationary)
adf.test(D.X)
# the p-value < 0.05 so we reject the null hypothesis and the series is stationary
#--------------------------
# 2- Finding p and q
#--------------------------
par(mfrow=c(1,2))
ggAcf(D.X,type ="correlation")
ggAcf(D.X,type ="partial")
#From the ACF , I can see that the most suitable value for q =2.
#From PACF , I can see that the most suitable value for p =4.
#step(2): Model Estimation
#--------------------------
#When fitting an ARIMA model we need to choose three things:
#p, the number of AR lags, q, the number of MA lags,
#and d,the number of differences to use.
# I give the arima the original series because it can handle the difference.
#ARIMA(p,d,q)
arimaModel_1=arima(df$MAURN, order=c(4,1,2))
arimaModel_2=arima(df$MAURN, order=c(4,1,0))
## look at the parameters
print(arimaModel_1);print(arimaModel_2)
AIC(arimaModel_1,arimaModel_2)
#ARIMA model 1 has a lower aic than ARIMA model 2 
#so I will continue with the first one.
CI=confint(arimaModel_1) #95% CI on coefs
#-----------------------------------------------------------------------
#step(3): Model Checking 
#-------------------------
par(mfrow=c(1,2))
hist(arimaModel_1$residuals)
# from histogram , the residuals looks follow normal distribution.
qqnorm(arimaModel_1$residuals) ; qqline(arimaModel_1$residuals)
# The normal Q-Q plot of the residuals is approximately linear.
#Box-Pierce and Ljung-Box Tests
#test for the absence of serial autocorrelation, up to a specified lag k.
Box.test(arimaModel_1$residuals, type = "Ljung-Box")
# The p-value > 0.05 so I can accept the null hypothesis that said
# the auto-correlations (for the chosen lags) in the population from which the sample is taken are all zero.
#-------------------------------------------------------------------------------
#step(4):Forecasting
#------------------------------
#make forecast
#Forecast  24 predictions since I have monthly data 
#I will make predictions two years out.
arimaModel_1.forecast <- forecast(arimaModel_1,h=24)
arimaModel_1.forecast
#plot forecast for the  model 
autoplot(arimaModel_1.forecast)
#According to this time series model, the monthly unemployment rate 
#either remains the same or decrease slightly in the next two years.
     



 
