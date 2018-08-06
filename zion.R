#Predict Zion NP Annual Visitors

#Data:
#Title,Bookmark
#Zion NP,Bookmark this report: https://irma.nps.gov/Stats/SSRSReports/Park%20Specific%20Reports/Annual%20Park%20Recreation%20Visitation%20(1904%20-%20Last%20Calendar%20Year)

#Accessed Mon Jan 8, 2018

#create a dataframe
zion <- read.csv(header=TRUE, text='
Year,RecreationVisitors,TotalRecreationVisitors
1919,"1,814","111,311,078"
1920,"3,692","111,311,078"
1921,"2,937","111,311,078"
1922,"4,109","111,311,078"
1923,"6,408","111,311,078"
1924,"8,400","111,311,078"
1925,"16,817","111,311,078"
1926,"21,964","111,311,078"
1927,"24,303","111,311,078"
1928,"30,016","111,311,078"
1929,"33,383","111,311,078"
1930,"55,297","111,311,078"
1931,"59,186","111,311,078"
1932,"51,650","111,311,078"
1933,"48,763","111,311,078"
1934,"68,801","111,311,078"
1935,"97,280","111,311,078"
1936,"124,393","111,311,078"
1937,"137,404","111,311,078"
1938,"149,075","111,311,078"
1939,"158,063","111,311,078"
1940,"165,029","111,311,078"
1941,"192,805","111,311,078"
1942,"68,797","111,311,078"
1943,"44,089","111,311,078"
1944,"42,243","111,311,078"
1945,"78,280","111,311,078"
1946,"212,280","111,311,078"
1947,"273,953","111,311,078"
1948,"297,571","111,311,078"
1949,"307,881","111,311,078"
1950,"323,402","111,311,078"
1951,"331,079","111,311,078"
1952,"352,921","111,311,078"
1953,"389,445","111,311,078"
1954,"416,800","111,311,078"
1955,"406,800","111,311,078"
1956,"421,200","111,311,078"
1957,"525,100","111,311,078"
1958,"590,700","111,311,078"
1959,"585,000","111,311,078"
1960,"575,800","111,311,078"
1961,"604,700","111,311,078"
1962,"622,100","111,311,078"
1963,"681,100","111,311,078"
1964,"705,200","111,311,078"
1965,"763,600","111,311,078"
1966,"815,200","111,311,078"
1967,"788,400","111,311,078"
1968,"877,100","111,311,078"
1969,"904,300","111,311,078"
1970,"903,600","111,311,078"
1971,"897,000","111,311,078"
1972,"889,417","111,311,078"
1973,"993,800","111,311,078"
1974,"859,300","111,311,078"
1975,"1,055,200","111,311,078"
1976,"1,090,000","111,311,078"
1977,"1,105,900","111,311,078"
1978,"1,193,212","111,311,078"
1979,"1,040,528","111,311,078"
1980,"1,123,846","111,311,078"
1981,"1,288,808","111,311,078"
1982,"1,246,290","111,311,078"
1983,"1,273,030","111,311,078"
1984,"1,377,254","111,311,078"
1985,"1,503,272","111,311,078"
1986,"1,670,503","111,311,078"
1987,"1,777,619","111,311,078"
1988,"1,948,332","111,311,078"
1989,"1,998,856","111,311,078"
1990,"2,102,400","111,311,078"
1991,"2,236,997","111,311,078"
1992,"2,390,626","111,311,078"
1993,"2,392,580","111,311,078"
1994,"2,270,871","111,311,078"
1995,"2,430,162","111,311,078"
1996,"2,498,001","111,311,078"
1997,"2,445,534","111,311,078"
1998,"2,370,048","111,311,078"
1999,"2,449,664","111,311,078"
2000,"2,432,348","111,311,078"
2001,"2,217,779","111,311,078"
2002,"2,592,545","111,311,078"
2003,"2,458,792","111,311,078"
2004,"2,677,342","111,311,078"
2005,"2,586,665","111,311,078"
2006,"2,567,350","111,311,078"
2007,"2,657,281","111,311,078"
2008,"2,690,154","111,311,078"
2009,"2,735,402","111,311,078"
2010,"2,665,972","111,311,078"
2011,"2,825,505","111,311,078"
2012,"2,973,607","111,311,078"
2013,"2,807,387","111,311,078"
2014,"3,189,696","111,311,078"
2015,"3,648,846","111,311,078"
2016,"4,295,127","111,311,078"
')

#Get rid of commas in the RecreationVisitors col and change to numeric vector 
zion$RecreationVisitors <- as.numeric(gsub(',','', zion$RecreationVisitors))
#convert to "millions of Visitors"
zion$RecreationVisitors <- zion$RecreationVisitors/10^6
head(zion)
tail(zion)

#EDA
#plot the time series for visitors in millions
plot(zion$RecreationVisitors~zion$Year, type="b", ylab="Annual Visitors in Millions")

#Analysis

#I saw curvature in the data, evidence of a multiplicative model. To account for this, I will take
#the log transformation of the data, which will convert the data into an additivie model. 
zion$RecreationVisitors <- log(zion$RecreationVisitors)

#There is evidence of non-constant mean change- a dip in the number of visitors during WWII.
#We'll filter to the recent past (1960) to avoid those outlier years. After 1960, the mean 
#change is fairly constant, and our data is improved because the war time years won't be taken into 
#account as we forecast for the future, which we don't anticipate to be a time of war. 
zion.1960 <- zion[zion$Year >= 1960,]

#Plot Data again to check
plot(zion.1960$RecreationVisitors~zion.1960$Year, type="b", ylab="Annual Visitors in Millions")
#the plot is now in a straightish line with no outliers, starting at the year 1960

# Model: ARIMA(1,1,1)
#Report the parameter estimates and standard errors
library("astsa")

zion.1960.arima <- sarima(zion.1960$RecreationVisitors, 1,1,1)
zion.1960.arima$ttable
#Note: mu hat is "constant", phi hat is "ar1", theta hat is "ma1"

#Compute Predictions (in logs) for the next 5 Years
zion.1960.future <- sarima.for(zion.1960$RecreationVisitors, n.ahead=5, 1,1,1)

zion.1960.future

#Compute 95% Prediction Intervals (in logs)
l <- zion.1960.future$pred - 1.96*zion.1960.future$se
u <- zion.1960.future$pred + 1.96*zion.1960.future$se

#Table of Un-transformed predictions and prediction intervals for the next 5 years (in millions)
cbind(exp(zion.1960.future$pred), exp(l) , exp(u))


# Make a publication quality graphic

##Create a dataframe of past data starting with the year 2000
zion.2000 <- zion.1960[zion.1960$Year >= 2000, c("Year", "RecreationVisitors")]
zion.2000$RecreationVisitors <- exp(zion.2000$RecreationVisitors)
## Create a dataframe of the future years and predicted recreation visitors.
future.year <- c(2017, 2018, 2019, 2020, 2021)
future.pred <- data.frame(exp(zion.1960.future$pred))
pred <- cbind(future.year, future.pred)

##Combine the dataframes to create a dataframe with the past 16 years and 5 years of prediction. 
colnames(pred) <- c("Year", "RecreationVisitors")
graphic <- rbind(zion.2000, pred)

#Make the Prediciton Points Red
graphic$Color <- "Black"
graphic$Color[graphic$Year>=2017]="red"
graphic

#Plot the Graphic
plot(graphic$RecreationVisitors~graphic$Year, ylab="Number of Visitors (millions)", xlab="Year", 
     col=graphic$Color, pch=16, main="Zion's Annual Visitors (in millions)", ylim=c(1,6.7))

#Add in the Upper and Lower Prediciton Limits
lines(2017:2021,exp(l),col="red",lty=2)
lines(2017:2021,exp(u),col="red",lty=2)


# Research Task: Predict Future Values
# Data Features: Time Series, Correlation observed in past is expected to continue next 5 years

# Analysis Weaknesses: No examination of "why" ... that is, no explanatory variables
# Our Prediction Intervals were also fairly wide, which is not as helpful to Zion National Park in 
# knowing how to allocate their resources

# Challenge
# Task: Predict the number of visitors at the Grand Canyon for the next 10 years.
# Data: found at https://irma.nps.gov/Stats/SSRSReports/Park





