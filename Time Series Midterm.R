#Data

#Q1
# webscraper for Q1 data from http://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q1
# function that allows reading in data of the form $xxx,xxx,xxx.xx
setClass("AccountingNumber")
setAs("character", "AccountingNumber", 
      function(from) as.numeric(gsub(",", "", gsub("[:$:]", "", from) ) ) )
# data from webpage
library(XML)
# Q1 box office url
q1boxoffice.url<-paste("http://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q1")
# read webpage and store in memory
q1boxoffice.webpage<-htmlParse(q1boxoffice.url)
# create R dataset from webpage contents
q1boxoffice<-readHTMLTable(q1boxoffice.webpage,
                           header=TRUE,which=4,
                           colClasses=c("numeric","AccountingNumber","Percent","numeric",
                                        "AccountingNumber","Percent","character",
                                        "AccountingNumber","Percent") )
# keep only year and gross
q1boxoffice<-q1boxoffice[,1:2]
# change variable name so it doesn't have a space
names(q1boxoffice)<-c("year","gross")

#Q2
# webscraper for Q2 data from http://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q2
# function that allows reading in data of the form $xxx,xxx,xxx.xx
setClass("AccountingNumber")
setAs("character", "AccountingNumber", 
      function(from) as.numeric(gsub(",", "", gsub("[:$:]", "", from) ) ) )
# data from webpage
library(XML)
# Q2 box office url
q2boxoffice.url<-paste("http://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q2")
# read webpage and store in memory
q2boxoffice.webpage<-htmlParse(q2boxoffice.url)
# create R dataset from webpage contents
q2boxoffice<-readHTMLTable(q2boxoffice.webpage,
                           header=TRUE,which=4,
                           colClasses=c("numeric","AccountingNumber","Percent","numeric",
                                        "AccountingNumber","Percent","character",
                                        "AccountingNumber","Percent") )
# keep only year and gross
q2boxoffice<-q2boxoffice[,1:2]
# change variable name so it doesn't have a space
names(q2boxoffice)<-c("year","gross")


#Q3
# webscraper for Q3 data from http://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q3
# function that allows reading in data of the form $xxx,xxx,xxx.xx
setClass("AccountingNumber")
setAs("character", "AccountingNumber", 
      function(from) as.numeric(gsub(",", "", gsub("[:$:]", "", from) ) ) )
# data from webpage
library(XML)
# Q3 box office url
q3boxoffice.url<-paste("http://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q3")
# read webpage and store in memory
q3boxoffice.webpage<-htmlParse(q3boxoffice.url)
# create R dataset from webpage contents
q3boxoffice<-readHTMLTable(q3boxoffice.webpage,
                           header=TRUE,which=4,
                           colClasses=c("numeric","AccountingNumber","Percent","numeric",
                                        "AccountingNumber","Percent","character",
                                        "AccountingNumber","Percent") )
# keep only year and gross
q3boxoffice<-q3boxoffice[,1:2]
# change variable name so it doesn't have a space
names(q3boxoffice)<-c("year","gross")

#Q4
# webscraper for Q4 data from http://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q4
# function that allows reading in data of the form $xxx,xxx,xxx.xx
setClass("AccountingNumber")
setAs("character", "AccountingNumber", 
      function(from) as.numeric(gsub(",", "", gsub("[:$:]", "", from) ) ) )
# data from webpage
library(XML)
# Q4 box office url
q4boxoffice.url<-paste("http://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q4")
# read webpage and store in memory
q4boxoffice.webpage<-htmlParse(q4boxoffice.url)
# create R dataset from webpage contents
q4boxoffice<-readHTMLTable(q4boxoffice.webpage,
                           header=TRUE,which=4,
                           colClasses=c("numeric","AccountingNumber","Percent","numeric",
                                        "AccountingNumber","Percent","character",
                                        "AccountingNumber","Percent") )
# keep only year and gross
q4boxoffice<-q4boxoffice[,1:2]
# change variable name so it doesn't have a space
names(q4boxoffice)<-c("year","gross")


#Create variable qtr for each Quarter's dataframe
q1boxoffice$qtr <- 1
q2boxoffice$qtr <- 2
q3boxoffice$qtr <- 3
q4boxoffice$qtr <- 4

#Combine Quarter's dataframes
boxoffice <- rbind(q1boxoffice, q2boxoffice, q3boxoffice, q4boxoffice)

#Reorder dataframe to sort by year and quarter
boxoffice <- boxoffice[order(boxoffice$year, boxoffice$qtr),]

#Delete current quarter
boxoffice <- boxoffice[-145,]
tail(boxoffice)

#EDA
#Plot the timeseries
plot(boxoffice$gross, type="b", main="Quarterly Box Office Gross", xlab="Quarters", ylab="Dollars (in millions)")

#Model
#ARIMA(1,1,1)x(1,1,1)4
library(astsa)
box.out <- sarima(boxoffice$gross,1,1,1,1,1,1,4)
box.out$ttable

#Forecast the Quarterly totals for the next 3 years
box.future <- sarima.for(boxoffice$gross, n.ahead = 12,1,1,1,1,1,1,4)

#create Upper and Lower 95% prediction intervals
lower <- box.future$pred-2*box.future$se
upper <- box.future$pred+2*box.future$se

#Report a table of predicitons and 95% prediction intervals
cbind(box.future$pred, lower, upper)

#Graphic
#show only after 2013
plot(boxoffice$gross[125:144], type="b", main="3 Year Prediction of Box Office Gross",
     ylab="Dollars (in millions)", pch=19, xlim=c(0,32), ylim=c(1500, 4000)) 
lines((21):(20+length(box.future$pred)), box.future$pred, col="red",type="b",pch=19)
lines((21):(20+length(box.future$pred)), lower, col="darkgray",lty=2)
lines((21):(20+length(box.future$pred)), upper ,col="darkgray",lty=2)

