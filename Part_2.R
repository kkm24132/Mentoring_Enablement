# ---------------------------------------------------------------------------------------
# Time Series Forecasting 
# Kamal Mishra
# Last updated: Sep-2016

# ---------------------------------------------------------------------------------------
# Load required libraries
library(dplyr)
library(tseries)

# ---------------------------------------------------------------------------------------
# Loading the Data Set
# Take an example of open dataset that comes with R
data(AirPassengers)
class(AirPassengers) #This tells you that the data series is in a time series format
start(AirPassengers) #This is the start of the time series
end(AirPassengers) #This is the end of the time series
frequency(AirPassengers) #The cycle of this time series is 12months in a year
summary(AirPassengers)

# Detailed Metrics
plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers))) # This will fit in a line
cycle(AirPassengers) #This will print the cycle across years.
plot(aggregate(AirPassengers,FUN=mean)) #This will aggregate the cycles and display a year on year trend
boxplot(AirPassengers~cycle(AirPassengers)) #Box plot across months will give us a sense on seasonal effect

# So key inferences that we can get are as follows:
#  The year on year trend clearly shows that the #passengers have been increasing without fail.
#  The variance and the mean value in July and August is much higher than rest of the months.
#  Even though the mean value of each month is quite different their variance is small. Hence, we have 
#  strong seasonal effect with a cycle of 12 months or less.

# Above exploratory data analysis is important as it can indicate whether a series is stationary ornot.


# Try with Augmented Dickey-Fuller Test
# We know that we need to address two issues before we test stationary series. One, we need to remove 
# unequal variances. We do this using log of the series. Two, we need to address the trend component. We 
# do this by taking difference of the series. Now, let’s test the resultant series.
adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)

# We see that the series is stationary enough to do any kind of time series modelling.


# ---------------------------------------------------------------------------------------
# Find optimal parameters

# Find the right parameters to be used in the ARIMA model. We already know that 
# the ‘d’ component is 1 as we need 1 difference to make the series stationary. We do this using 
# the Correlation plots. Following are the ACF plots for the series

# ACF plots to be drawn
acf(log(AirPassengers))

# Clearly, the decay of ACF chart is very slow, which means that the population is not stationary. 
# We now intend to regress on the difference of logs rather than log directly. Let’s see how ACF and PACF 
# curve come out after regressing on the difference.
acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers)))

# Clearly, ACF plot cuts off after the first lag. Hence, we understood that value of p should be 0 as 
# the ACF is the curve getting a cut off. While value of q should be 1 or 2. After a few iterations, we 
# found that (0,1,1) as (p,d,q) comes out to be the combination with least AIC and BIC.


# ---------------------------------------------------------------------------------------
# Build and Fit ARIMA model
# Let’s fit an ARIMA model and predict the future 10 years. Also, we will try fitting in a seasonal component 
# in the ARIMA formulation. Then, we will visualize the prediction along with the training data. 
(fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))



# ---------------------------------------------------------------------------------------
# Make Predictions
pred <- predict(fit, n.ahead = 10*12)

ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))



# Ends
# ---------------------------------------------------------------------------------------
