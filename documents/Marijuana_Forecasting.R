################################################################
# GOAL - Forecast Colorado's monthly medicinal marijuana sales
################################################################


#### DATA PREPARATION ####


# Load required packages
library(scales)
library(seasonal)
library(urca)
library(forecast)
library(dplyr)
library(ggplot2)

#Sales Data
#https://cdor.colorado.gov/data-and-reports/marijuana-data/marijuana-sales-reports

#Consumer Price Index Data
#https://fred.stlouisfed.org/series/CPIAUCSL

# Read sales data
marijuana <- read.csv("marijuana.csv") 

# Remove commas and dollar signs, change to numeric
marijuana$Sales <- gsub(",","",marijuana$Sales)
marijuana$Sales  <- gsub("\\$","",marijuana$Sales)
marijuana$Sales <- as.numeric(marijuana$Sales)

# Read seasonally adjusted Consumer Price Index
cpi <- read.csv("CPI.csv")
# Create index to adjustment for inflation
cpi$Index <- cpi$CPIAUCSL/last(cpi$CPIAUCSL)

# Join dataframes and calculate sales adjusted for inflation
marijuana <- inner_join(marijuana, cpi, "Date")
marijuana$Adjusted_Sales <- marijuana$Sales/marijuana$Index 

# Make data a times series object
Y <- ts(marijuana[,5],  start = c(2014,1), frequency = 12)


#### DATA EXPLORATION ####


# Plot data
autoplot(Y) + xlab("Year") + ylab("Monthly Sales Dollars") +
  ggtitle("Monthly Retail Medical Marijuana Sales") + 
  scale_y_continuous(labels = unit_format(unit = "MM", scale = 1e-6))
# It is difficult to identify seasonality or trend in the plot above.
# Let's plot the data by month.

# Plot data by month
ggseasonplot(Y) + xlab("Month") + ylab("Monthly Sales Dollars") +
  ggtitle("Seasonal Plot - Monthly Retail Medical Marijuana Sales") + 
  scale_y_continuous(labels = unit_format(unit = "MM", scale = 1e-6))

# The plot above provides a better picture of the seasonality present in the data.
# Sales decrease throughout January every year (excluding 2016)
# Sales increase throughout February every year
# Sales increase throughout June and July every year (excluding 2017 and 2020, respectively)
# Sales decrease every year throughout August (excluding 2019) and October
# The significant sales increase in April of 2020 and the consistently high sales that followed are likley due to COVID-19

# The plot below is confirmation of our previous observations
# The horizontal lines represent the monthly mean sales dollars for all years
ggsubseriesplot(Y) + xlab("Month") + ylab("Monthly Sales Dollars") +
  ggtitle("Subseries Plot - Monthly Retail Medical Marijuana Sales") + 
  scale_y_continuous(labels = unit_format(unit = "MM", scale = 1e-6))


#### BENCHMARK FORECASTING ####


# Mean method - Forecasts using this method are equal to the average value of all previous observations.
meanf(Y, h = 12) # h = number of time periods to predict

# The plot below shows the point prediction (small dark line around $35MM) and its 80% (dark shaded region) and 90%
autoplot(Y) +
  autolayer(meanf(Y, h = 12)) +
  xlab("Year") + ylab("Monthly Sales Dollars") +
  ggtitle("Average Method Forecast") + 
  scale_y_continuous(labels = unit_format(unit = "MM", scale = 1e-6))

# The mean absoulte error (MAE) is 4,554,857. We will use this metric to compare all future models.
summary(meanf(Y, h=12))

# Naive method (Random Walk) - Forecasts using this method are equal to the value of the most recent observation
naive(Y, h = 12)

autoplot(Y) +
  autolayer(naive(Y, h = 12)) +
  xlab("Year") + ylab("Monthly Sales Dollars") +
  ggtitle("Naive Method Forecast") + 
  scale_y_continuous(labels = unit_format(unit = "MM", scale = 1e-6))

# The mean absoulte error (MAE) is 2,022,780. This is a huge improvement from the mean model!
summary(naive(Y, h = 12))

# Random walk with a drift - Forecasts using this method are equal to the value of the most recent observation
# PLUS the average change of all previous observations
rwf(Y, h = 12, drift=TRUE)

autoplot(Y) +
  autolayer(rwf(Y, h = 12, drift=TRUE)) +
  xlab("Year") + ylab("Monthly Sales Dollars") +
  ggtitle("Drift Method Forecast") + 
  scale_y_continuous(labels = unit_format(unit = "MM", scale = 1e-6))

#The mean absoulte error (MAE) is 2,021,088. This model performed slightly better than naive method.
summary(rwf(Y, h = 12, drift=TRUE))

# The plot below shows both naive and drift forecasts for 24 months.
# Extending the length of our forecasts allows us to better visualize the difference between the two methods
autoplot(Y) +
  autolayer(naive(Y, h=24),
            series="Naive", PI=FALSE) +
  autolayer(rwf(Y, h=24, drift = TRUE),
            series="Drift", PI=FALSE) +
  ggtitle("Naive and Drift Forecasts") + 
  scale_y_continuous(labels = unit_format(unit = "MM", scale = 1e-6)) + 
  xlab("Year") + ylab("Monthly Sales Dollars")

# Seasonal naive method - forecast is equal to the value of the most recent observation that occurred in the same season of the year
snaive(Y, h = 12)

autoplot(snaive(Y, h = 12), PI = TRUE) + xlab("Year") + ylab("Monthly Sales Dollars") +
  scale_y_continuous(labels = unit_format(unit = "MM", scale = 1e-6))

# The mean absoulte error (MAE) is 4,877,299. This model performed worse than the average method
summary(snaive(Y, h = 12))

# Let's plot three of these predictions simultaneously 
autoplot(Y) +
  autolayer(meanf(Y, h=12),
            series="Mean", PI=FALSE) +
  autolayer(rwf(Y, h=12, drift = TRUE),
            series="Drift", PI=FALSE) +
  autolayer(snaive(Y, h=12),
            series="Seasonal Naive", PI=FALSE) +
  xlab("Year") + ylab("Monthly Sales Dollars") +
  ggtitle("Forecasts") + 
  scale_y_continuous(labels = unit_format(unit = "MM", scale = 1e-6))


#### DECOMPOSITION ####


# Decomposition splits time series data into its different components.

# 1) Trend-cycle (Combination of trend and cyclicity)
# 2) Seasonality
# 3) Remainder

# If the seasonal component is removed from the original data, the data is said to be "seasonally adjusted."

# There are two types of classical decomposition

# Additive - This model assumes the seasonal component does not change over time
Y %>% decompose(type="additive") %>%
  autoplot()

# Multiplicative - This model assumes the seasonal component does change over time
Y %>% decompose(type="multiplicative") %>%
  autoplot()

# The multiplicative decomposition plot looks almost identical to the additive decomposition plot
# The additive decomposition model makes the most sense for this data because the magnitude of the seasonality does not change over time.

# There are also modern ways to decompose data.
# Three of them are presented here. These models attempt make up for the shortcomings of the additive and multiplicative models.

# X11
Y %>% seas(x11="") -> fit
autoplot(fit) +
  ggtitle("X11 decomposition of monthly sales")
# The X11 method reduced the much of the remainder significantly

# SEATS - "Seasonal Extraction in ARIMA Time Series"
Y %>% seas() %>% autoplot()

# STL
Y %>%
  stl(t.window=5, s.window="periodic", robust=TRUE) %>%
  autoplot()


#### Stationarity ####


# Time series data is stationary when its properties do not depend on time.

# Stationary data should:
    #Look flat
    #Not have a trend
    #Have a constant variance
    #Differencing is method that transforms time series data by removing its dependency on time.
        #Difference(t) = observation(t) - observation(t-1)

# You can think of differenced data as the monthly change in sales
# Let's look again at our original data.

autoplot(Y) + xlab("Year") + ylab("Monthly Sales Dollars") +
  ggtitle("Monthly Retail Medical Marijuana Sales") + 
  scale_y_continuous(labels = unit_format(unit = "MM", scale = 1e-6))

# Let's take the first difference of our data.
DY <- diff(Y)
autoplot(DY) + xlab("Year") + ylab("Monthly Sales Dollars") +
  ggtitle("Monthly Change In Retail Medical Marijuana Sales") + 
  scale_y_continuous(labels = unit_format(unit = "MM", scale = 1e-6))

# Let's take first differences of the data 
Y %>% diff() %>% autoplot()
# The data looks stationary.

#Let's compare the ACF plots of the original and differenced data.
ggAcf(Y)
ggAcf(DY)

# The ACF plot for the original data shows trend and seasonality
# The ACF plot for the differenced data looks much more like white noise.
# There are, however, signficant autocorrelations at lags 6, 12, and 24.

# How can we be sure that we are taking the right number of first differences and seasonal differences? Statistical tests!
# KPSS test null hypothesis - The data is stationary

Y %>% ur.kpss() %>% summary() # The test-statistic is similar to the critical values.  We have enough evidence to reject the null.
Y %>% diff() %>% ur.kpss()  %>% summary() # The test statistic is very small
# We do not have enough evidence to reject the null. The data is stationary!

# We can also use the ndiffs() function to determine how many differences we need
ndiffs(Y) #1

# Should we take any seasonal differences?
nsdiffs(Y) #0


#### ETS "Exponential Smoothing" MODEL ####


# To explain this method, let's discuss previous methods.

# Average - Think of this method as a weighted average of all previous observations in which equal weight is given to all previous values
# Naive - Think of this method as a weighted average of all previous observations in which 100% of the weight is given to the most recent observation
# The ETA method, of course, can also be thought of as a weighted average!
# However, higher weights are given to the most recent observations, and lower weights are given to older observations

fit <- ets(Y) # The ets function does not require that we difference the data. The function takes care of that for us.
forecast(fit, h = 12)

autoplot(Y) +
  autolayer(forecast(fit, h = 12)) +
  xlab("Year") + ylab("Monthly Sales Dollars") +
  ggtitle("ETS Method Forecast") + 
  scale_y_continuous(labels = unit_format(unit = "MM", scale = 1e-6))

# Residuals represent the difference between observations and forecasts.
# To obtain optimal forecasts, it is best if residuals have the following characteristics:
 
# They are uncorrelated from each other
# Their mean is zero
# They have a constant variance
# They are normally distributed
# In other words, the residuals should resemble white noise.

# Below are the residuals for the ETS model.
checkresiduals(fit)
# Visually, it seems the residuals are close to resembling white noise.  However, we obtained a small p-value for the Ljung-Box test.
# Therefore, we reject the null and determine that significant autocorrelation exists.  This means that the ETS model does not account for all available information.

# The MAE for this model is 1,353,106.  This is by far the best score we have obtained.
summary(ets(Y))


#### ARIMA (Auto Regressive Integrated Moving Average) MODELS ####


# ARIMA models consist of a combination of the three components beloW

# Autoregressive (AR) models use linear combinations of previous values to forecast future values
# Integrated (I) - this describes the order of differencing of a time series
# Moving Average (MA) models use the errors of prevous forecasts to forecast future values

# SARIMA (Seasonal ARIMA)
# In addition to the components above, SARIMA models have a seasonal component

# Let's take another look at our data after taking the first difference
# Remember that the kpss tests showed that the differenced data is stationary.
# Unfortunately, the ACF and PACF plots both have significant lags. This means that our models may lose predictive power.

# Let's use the function auto.arima to find the best model for us based on the AIC.
auto.arima(Y, stepwise=TRUE)

# ARIMA(0,1,0)(1,0,0)
# ARIMA(p,d,q)(P,D,Q)

# p,d, and q describe the non-seasonal parts of the model, while P,D, and Q decribe the seasonal parts of the model.

# p/P = order of autoregression (AR)
# d/D = degree of first differencing (I)
# q/Q = order of moving average (MA)

# This model takes one non-seasonal difference(0,1,0) and has one order of seasonal autogregression(1,0,0).

fit <- auto.arima(Y, stepwise=TRUE)

autoplot(Y) +
  autolayer(forecast(fit,h=12)) +
  xlab("Year") + ylab("Monthly Sales Dollars") +
  ggtitle("SARIMA Method Forecast") + 
  scale_y_continuous(labels = unit_format(unit = "MM", scale = 1e-6))

# The SARIMA forecast resemble that of the seasonal naive model.

# This makes sense for the following reasons:
    # ARIMA(0,1,0), the non-seasonal part of this model, is equivalent to a random walk without a drift
    # Adding the seasonal component takes into account the most recent observations that occurred in the same season (month) of the previous year

# The MAE is 1,705,159. This model outperformed all of the benchmark models but did not outperform the exponential smoothing model.
summary(fit)

checkresiduals(fit)


#### SUMMARY ####


# Through visualization and decomposition techniques, we identified the data as being non-stationary (i.e. it has seasonality and trend).  
# Our best benchmark model was the seasonal naïve method according to the mean absolute error (MAE). This is likely because it considered the seasonality of the data.

# The data needed to be differenced.  As a reminder, differencing is a method that transforms time series data by removing its dependency on time.
# Our more complex models attempt to account for this dependency.

# The SARIMA model had an MAE of 1,705,159.  However, the residuals resembled white noise.
# The ETS model outperformed all other models with an MAE of 1,353,106.  However, the residuals did not resemble white noise.

# It would be interesting to account for external factors like the COVID-19 and adjustments to the marijuana industry (curbside pickup, online sales).
# Because of the pandemic, people are staying home more often and have more opportunities to use marijuana.
# Additionally, medical marijuana may be allieviating increased levels of anxiety caused by the pandemic.
# These factors could perhaps be accounted for using neural networks, bootstrapping, and bagging models.
