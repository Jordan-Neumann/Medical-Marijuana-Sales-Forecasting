################################################################
# GOAL - Forecast California's monthly medicinal marijuana sales
################################################################



#### DATA PREPARATION ####



# Load required packages
library(fpp2)
library(lubridate)
library(dplyr)
library(scales)
library(seasonal)
library(urca)
library(tseries)

# Read sales data
marijuana <- read.csv("marijuana.csv") 

# Remove commas and dollar signs, change to numeric
marijuana$Sales <- gsub(",","",marijuana$Sales)
marijuana$Sales  <- gsub("\\$","",marijuana$Sales)
marijuana$Sales <- as.numeric(marijuana$Sales)

# Read seasonally adjusted Consumer Price Index https://fred.stlouisfed.org/series/CPIAUCSL
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
# It is difficult to see seasonality in the plot.
# There may be a slight negative trend

# Plot data by month
ggseasonplot(Y) + xlab("Month") + ylab("Monthly Sales Dollars") +
  ggtitle("Seasonal Plot - Monthly Retail Medical Marijuana Sales") + 
  scale_y_continuous(labels = unit_format(unit = "MM", scale = 1e-6))

# There is a  decrease in sales every year throughout January excluding 2016
# There is an increase in sales every year throughout February
# There is an increase in sales every year throughout July excluding 2020
# There is a  decrease in sales every year in August and October

# The plot below is confirmation of our previous observations
# The horizontal lines represent the monthly mean sales dollars for all years
ggsubseriesplot(Y) + xlab("Month") + ylab("Monthly Sales Dollars") +
  ggtitle("Subseries Plot - Monthly Retail Medical Marijuana Sales") + 
  scale_y_continuous(labels = unit_format(unit = "MM", scale = 1e-6))

# ACF Plot
ggAcf(Y) + ggtitle("Autocorrelation Plot")
# The decrease in ACF as lags increase is caused by the trend
# The scalloped shape appears to show seasonality



#### BASIC FORECASTING ####



# Mean method # - forecast is equal to the average value of all historical data
meanf(Y, h = 12) # h = number of time periods to predict

# Naive method (Random Walk) - forecast is equal to the value of the most recent observation
naive(Y, h = 12)

# Random walk with a drift - forecast is equal to the value of the most recent observation PLUS the average change of the historical data
rwf(Y, h = 12, drift=TRUE)

# Let's predict 24 months in advance to compare the naive and random walk w/ drift methods
autoplot(Y) +
  autolayer(naive(Y, h=24),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(Y, h=24, drift = TRUE),
            series="Drift", PI=FALSE) +
  ggtitle("Naive and Drift Forecasts") + 
  scale_y_continuous(labels = unit_format(unit = "MM", scale = 1e-6))
# The drift forecast decreases over time because of the negative trend in the data

# Seasonal naive method - forecast is equal to the value of the most recent observation that occurred in the same season of the year
snaive(Y, h = 4)

# Plotting predictions further into the future can provide a better understanding of the seasonal naive method.
autoplot(snaive(Y, h = 24)) + xlab("Year") + ylab("Monthly Sales Dollars") +
  scale_y_continuous(labels = unit_format(unit = "MM", scale = 1e-6))

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

# Additive - this method assumes the seasonal component does not change over time
Y %>% decompose(type="additive") %>%
  autoplot()

# Multiplicative
Y %>% decompose(type="multiplicative") %>%
  autoplot()

# There is not a huge difference between the additive and multiplicative decompositions
# Additive makes more sense because the magnitude of seasonality does not change over time.

# A newer method of decomposition is X11

# X11
Y %>% seas(x11="") -> fit
autoplot(fit) +
  ggtitle("X11 decomposition of monthly sales")

# The X11 method reduced the much of the remainder significantly



#### Stationarity ####



# Data is stationary when its properties do not depend on time

# Differencing is method that transforms time series data by removing its dependency on time
# Difference(t) = observation(t) - observation(t-1)

# Let's take first differences of the data 
Y %>% diff() %>% autoplot()
# The data seems to resemble white noise.  How can we be sure?

# Statistical hypothesis tests

# Null hypothesis - the data are stationary
Y %>% ur.kpss() %>% summary() # The test-statistic is similar to the critical values
Y %>% diff() %>% ur.kpss()  %>% summary() # The test-statistic is very small relative to the critical values 
# Therefore, do not reject the null

# We can also use the ndiffs() function to determine how many differences we need
ndiffs(Y)

# Should we take any seasonal differences?
nsdiffs(Y)

# Let's look at the ACF plots for the data and the differenced data
DY <- diff(Y)

ggAcf(Y) # The data is not stationary
ggAcf(DY) # There are a few lags with high autocorrelation but the data seems relatively stationary



