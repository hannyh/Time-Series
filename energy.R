
# US Residential energy consumption
#Data from site: https://www.eia.gov/totalenergy/data/browser/?tbl=T02.02#/?f=M
#accessed Jan 12, 2018

data1 <- read.csv(file="/Users/hannahward/Documents/School/Winter 2018/Regression/Time Series/energy.csv", header=TRUE)
head(data1)

#Subset to TERCBUS Total Energy Consumed by the Residential Sector
data2 <- subset(data1, MSN=="TERCBUS")

#Subset to my lifetime
data3 <- subset(data2, data2$YYYYMM > 199100)

#Removes yearly total (coded month 13)
data4 <- subset(data3, data3$YYYYMM%%100 != 13)

energy <- as.numeric(data4$Value)
head(energy)
tail(energy)

#EDA: plot data to look for monthly patterns
plot(energy~as.factor(data4$YYYYMM), pch=19)
#or
plot(energy, type="b")

#There appears to be a cyclical trend going on with the months. Some months of the year have high energy
#consumption, while others months have lower energy consumption. This business/economic trend could be 
#caused by the weather in the United States. Many states are much colder in the winter, requiring more
#energy to keep a house warm. It is also very warm in the summer in some states, and many use air 
# conditioning to keep their homes cool, which also increases energy use within the residential sector.

T <- length(energy)

#Analysis

#Fit Model ARIMA (1,1,1)x(1,1,1)12
#   Features month to month correllation as well as year to year correllation
library(astsa)
energy.out <- sarima(energy,1,1,1,1,1,1,12)

#Report of Parameter Estimates and Stantard Errors
energy.out$ttable
#Note ar1 is lower case phi, ma1 is lower case theta, sar1 is capital phi, and sma1 is capital theta

#Predictions for the next 24 Months 
energy.future <- sarima.for(energy, n.ahead=24,1,1,1,1,1,1,12)

# compute 95% prediction intervals
L <- energy.future$pred - 2*energy.future$se
U <- energy.future$pred + 2*energy.future$se

#Table of predictions and prediction intervals
cbind(energy.future$pred, L, U)

#Graphic
plot(energy[290:T], type="b", xlim=c(0,T-290+24), main="24 Month Prediction of US Residential Energy Consumption",
     ylab="Energy Consumption (in trillion Btu)", pch=19)
lines((T+1-290):(T-290+length(energy.future$pred)), energy.future$pred, col="red",type="b",pch=19)
lines((T+1-290):(T-290+length(energy.future$pred)), L, col="darkgray",lty=2)
lines((T+1-290):(T-290+length(energy.future$pred)), U ,col="darkgray",lty=2)

#Research Task: Predict US Residential energy consumption for the next 2 years (24 months)

#Data Features: Month to Month correlation as well as year over year correlation. We expect the 
#pattern to continue for the next 24 months. 

# Analysis Weaknesses:
# Doesn't account for long long term trends and changes, such as global warming. 
# Doesn't account for a changes in the demand for energy due to the increase of alternative energy 
# sources that people can provide for themselves.


#Challenge
##Research Task: Predict the Daily High Temperature at the Salt Lake City Airport for the next year
##Data found at https://www.wunderground.com/history/airport/





