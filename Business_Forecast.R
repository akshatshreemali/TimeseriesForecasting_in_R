# 
# # ---
#   title: "Business_Forecasting"
# author: "Akshat"
# date: "April 26, 2018"
# output: html_document
# ---
#   <b>Introduction</b>
#   There has been a lot of discussions on trade policies lately. It would be good to understand the current trend of US Net Imports before we predict the implication due to the policy changes. 
# https://www.bea.gov/ is a great resource to learn more. Our dataset will be seasonally adjusted quarterly US net imports in Billions of Dollar for the last few years. 
# 
# 
# Loading the data as a dataframe and converting the data into a timeseries data


library(readr)
library(forecast)
library(fpp)
Data <- read_csv("C:/Users/Akshat/Downloads/Data_Spring_2018_NetImports.csv")
NITS <- ts(Data$Imports,start=c(2008,01),frequency = 4)
plot(NITS)




# .	Summary of the observations in the times series plot
# From the above graph, we can summarize that there is an upward trend after the sharp decline from 2008 to 2010. Seasonality doesn't appear in this timeseries.


# .	What are the min, max, mean, median, 1st and 3rd Quartile values of the times series? 
# .	Also plot the box plot. 
# .	Can you summarize your observation about the time series from the summary stats and box plot? 
# 
# 
summary(NITS)


# The mean and median are not close to each other and mean<median. 
# The distribution is the left skewed


boxplot(NITS,ylab='Boxplot of US Net Imports')



#There appears to be outliers in our graph and our data looks left skewed. The amount of data present in both the quartiles are not in sync.The outliers are present in the lower half of data ie) less than the 1st quartile

# <b>Decomposition</b>
# 
# .	Plot the decomposition of the time series.
# .	Is the times series seasonal?
# No, The time series is not seasonal. From the decomposition plot, it is seen that the time series is not seasonal as the graph is same and it repeats itself in the same pattern every season. The time series has trend and it has trend to a greater extent.


deco<-stl(NITS,s.window = 'periodic')
plot(deco)



Is the decomposition additive or multiplicative? 


a<-decompose(NITS)
a$type

# additive

#what are the values of the seasonal monthly indices? 

a$seasonal


#The series values are constant throught the quarters indicating no seasonality.

a$trend





#.	For which month is the value of time series high and for which month is it low?
max(na.rm = TRUE ,a$trend)
# Time series was highest during Q2-2017
min(na.rm = TRUE,a$trend)
# Time series was lowest during Q2-2008


#.	Show the plot for time series adjusted for seasonality. Overlay this with the line for actual time series? Does seasonality have big fluctuations to the value of time series? 

library(forecast)
plot(seasadj(a),type="l",col="black")
lines(NITS,col="green")

#Seasonality has no fluctuations on this and even after adjusting that parameter, graph fits the original data almost accurately

# <b> Naïve Method </b>
# Using Naive method to forecast

naive_ts<-naive(NITS)
naive_ts

# .	Perform Residual Analysis for this technique. 
# o	Do a plot of residuals. What does the plot indicate?
# o	Do a Histogram plot of residuals. What does the plot indicate?
# o	Do a plot of fitted values vs. residuals. What does the plot indicate? 
# o	Do a plot of actual values vs. residuals. What does the plot indicate?
# o	Do an ACF plot of the residuals? What does this plot indicate?
# 


plot(naive_ts$residuals, ylab = "residuals")

# The residual plot pattern is almost similar to our timeseries plot.
# The graph decrease sharply and then rises again sharply and then follows a gradual rise and fall throught 


hist((naive_ts$residuals))


# The histogram plot appears highly left skewed and the gap in data is the sharp decline in the data.
# 

plot(naive_ts$fitted, naive_ts$residuals, main="Fitted vs Residual",pch=19)

# Fitted vs residual plots doesn't appear to be linear or equally distributed. There are  points which appear to be outliers.  Also, it's random with no pattern

#library(car)
#scatterplot(naive_ts$x~ naive_ts$residuals)
plot(naive_ts$x, naive_ts$residuals, main="Actual vs Residual")

# Actual vs residual plots doesn't appear to be linear and equally distributed. There are  points which appear to be outliers. Also, there's no pattern

Acf(naive_ts$residuals)

# Acf plot indicates the presense of trend and only the starting point is above the significance. Rest are not

accuracy(naive_ts)

# The value of MAPE is <b>2.85</b> , which is fairly high indicating that we should go for other model.


q <- forecast(naive_ts, h=4)
q



plot(q)

# <b> Using Simple Moving Averages </b>
# .	Plot the graph for time series. 
# .	Show the Simple Moving average of order 3 on the plot above in Red
# .	Show the Simple Moving average of order 6 on the plot above in Blue
# .	Show the Simple Moving average of order 9 on the plot above in Green


plot(NITS)
ma_3 <- lines(ma(NITS,order=3),col ="red")
ma_6 <- lines(ma(NITS,order=6),col ="blue")
ma_9 <- lines(ma(NITS,order=9),col ="green")


# Forecast of next 12 months using one of the simple average order that you feel works best for time series.
# 
# Forecasting the time series using m=3 because it fits the time series more accurately.

ma_fore <- forecast(ma(NITS,3),h=4)
ma_fore


ma_fore <- forecast(ma(NITS,20),h=4)
plot(ma_fore)

# 
# <b> using Simple Smoothing model to forecast</b>
# .	Perform a smoothing forecast for next 12 months for the time series. 
# o	What is the value of alpha?  What does that value signify? 
# o	What is the value of initial state? 
# o	What is the value of sigma?  What does the sigma signify?
# 




ets_forecast <- ets(NITS)
forecast_ets <- forecast.ets(ets_forecast, h=4)
summary(forecast_ets)

# The value of Alpha sigma is 0.9999  The value of alpha signifies that higher weight is given to recent data. It is the choice of smoothing factor .
# If the value is very close to zero, it indicates that the forecasts are based on both recent and less recent observations.
# As seen from above, the value of alpha is 0.999 ,This value is very close to 1 which indicates that the most recent values have more weight
# 
# 
# Value of initial state is 2505.16 As it increases, influence of forecast decreases


# .	Perform Residual Analysis for this technique. 
# o	Do a plot of residuals. What does the plot indicate?


plot(ets_forecast$residuals,main="Residual plot for Simple Smoothing",ylab="Residual")


# The above shown is residual plot which is same like the time series graph . The graph is smooth time series. Residual component doesn't has a high influence in time series
# 

hist(ets_forecast$residuals)

# The histogram plot appears highly left skewed and the gap in data is the sharp decline in the data. Also, tthere is no pattern as such in the graph


# o	Do a plot of fitted values vs. residuals. What does the plot indicate? 

plot(ets_forecast$fitted, ets_forecast$residuals, main = "fitted vs residuals" , xlab ="Fitted" , ylab = "Residuals")

# Fitted vs residual plots doesn't appear to be linear and equally distributed. There are  points which appear to be outliers. Also, there's no pattern

plot(NITS, ets_forecast$residuals, main = "actuals vs residuals" , xlab ="Actuals" , ylab = "Residuals")

# Actuals vs residual plots doesn't appear to be linear and equally distributed. There are  points which appear to be outliers. Also, there's no pattern

Acf(ets_forecast$r)

# Acf plot indicates the presense of trend and only the starting point is above the significance. Rest are not.
# 
# .	Print the 5 measures of accuracy for this forecasting technique

accuracy(ets_forecast)

# .	Forecast 
# o	Time series value for next year. Show table and plot


q <- forecast(ets_forecast, h=4)
print(q)




plot(q)

# <b>Holt-Winters </b>
#   .	Perform Holt-Winters forecast for next 12 months for the time series. 
# o	What is the value of alpha?  What does that value signify? 
# o	What is the value of beta? What does that value signify?
# o	What is the value of gamma? What does that value signify?
# o	What is the value of initial states for the level, trend and seasonality? What do these values signify? 
# o	What is the value of sigma?  What does the sigma signify?



Price_Winters <- HoltWinters(NITS)
pf <- forecast(Price_Winters, h=4)
pf



summary(pf)

# .	Perform Residual Analysis for this technique. 
# o	Do a plot of residuals. What does the plot indicate?
# o	Do a Histogram plot of residuals. What does the plot indicate?
# o	Do a plot of fitted values vs. residuals. What does the plot indicate? 
# o	Do a plot of actual values vs. residuals. What does the plot indicate?
# o	Do an ACF plot of the residuals? What does this plot indicate?


plot(pf$residuals)

#The residual plot looks very different when compared with other models. it indicates trend along and doesn't match with the original time series graph.

hist((pf$residuals))

#Histogram of the residuals also look different and appears rightly skewed ,while the other models looked left skewed.


plot(pf$fitted, ets_forecast$residuals, main = "fitted vs residuals" , xlab ="fitted" , ylab = "Residuals")

#The fitted vs residuals graph doesn't seems to have any patterns. there appears to be some outliers.


plot(pf$x, ets_forecast$residuals, main = "Actuals vs residuals" , xlab ="Actuals" , ylab = "Residuals")

#The actuals vs residuals graph doesn't seems to have any patterns. there appears to be some outliers.

Acf(pf$residuals)

# Acf of the residuals seems to have only two significant points. All the other points are in it. There apears to be a trend.
# 
# 
# <b> the 5 measures of accuracy for this forecasting technique </b>


accuracy(pf)


# .	Forecast 
# o	Time series value for next year. Show table and plot


pf



plot(pf)


# .	Summarize this forecasting technique
# o	How good is the accuracy?
# <b>Accuracy is bad since the value of MAPE is 5.85</b>
# o	What does it predict the value of time series will be in one year?
# The predicted value will be between 2317.315 & 3961.695. Both in 2018 Q4, indicating that 2018Q4 me see the highest or lowest value.



# <b> ARIMA or Box-Jenkins </b>
# 
# .	Is Time Series data stationary? How did you verify? Please post the output from one of the test. 
# 
# # ADF test says differences is required if p-value is < 0.05

library(tseries)
adf.test(NITS)



kpss.test(NITS)



ndiffs(NITS)


NITS1<-diff(NITS,differences=1)
ndiffs(NITS1)



tsdisplay(NITS1)



Acf(NITS1, lag.max=20)



Pacf(NITS1, lag.max=20)



Acf(NITS1, lag.max=20,plot = FALSE)



pacf(NITS1, lag.max=20,plot = FALSE)





auto.arima(NITS)



auto.arima(NITS1)



auto.arima(NITS, stepwise=TRUE, trace=FALSE, approximation=FALSE)


#.	Based on the ACF and PACF, which are the possible ARIMA model possible?

fit_Arima <- Arima(NITS, order=c(2,1,1))



#.	Show the AIC, BIC and Sigma^2 for the possible models?

summary(fit_Arima)



# .	Perform Residual Analysis for this technique. 
# o	Do a plot of residuals. What does the plot indicate?


plot(fit_Arima$re)


# The residual plot using ARIMA is almost similar to our time series model.The residual plot captures the steep rise and fall in the beginning

hist(fit_Arima$residuals)

# Histogram of the residuals looks severely left skewed. It also doesn't have any specific pattern or not even close to a normal distribution


plot(fit_Arima$fitted,fit_Arima$residuals,xlab='Fitted',ylab='Residuals',main='Fitted vs Residuals')

# Fitted vs residual plots doesn't seem to have any specific pattern and outliers are also present


plot(NITS,fit_Arima$residuals,xlab='Actuals',ylab='Residuals',main='Actuals vs Residuals')

#Actual vs residual plots doesn't seem to have any specific pattern and outliers are also present


Acf(fit_Arima$residuals)

#There is no significant lines which are present. Also most of the points are close to zero.


accuracy(fit_Arima)

# .	Forecast 
# o	Next one year. Show table and plot


fo<-forecast(fit_Arima,h=4)
fo



plot(fo)


# .	Forecast 
# o	Next two year. Show table and plot

fo<-forecast(fit_Arima,h=8)
fo


plot(fo)


# Model Comparision
# Model	Accuracy Metrics						
# ME	RMSE	MAE	MPE	MAPE	MASE	ACF1
# Naïve Method	11.54359	109.7054	68.08205	0.2970542	2.852688	0.3286562	0.5405785
# Simple Smoothing	12.9118	108.8364	68.04088	0.3540331	2.845989	0.3284575	0.5489618
# Holt Winters	41.5499	210.1229	145.5452	1.67805	5.850816	0.7025981	0.3363672
# ARIMA	7.607852	69.20748	42.97381	0.263098	1.657254	0.2074498	-0.03197029
# 

# Model	Accuracy Metrics						
# ME	RMSE	MAE	MPE	MAPE	MASE	ACF1
# Best	ARIMA	ARIMA	ARIMA	ARIMA	ARIMA	ARIMA	Holt Winters
# Worst	Holt Winters	Holt Winters	Holt Winters	Holt Winters	Holt Winters	Holt Winters	Simple Smoothing








