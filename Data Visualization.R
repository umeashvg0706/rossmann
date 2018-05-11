library(ggplot2)
library(zoo)
library(data.table)
library(dplyr)


setwd("G:/Engineering Management/CourseWork/Data Mining/Assingment/CaseStudy/Rossman")
#Loading the data sets
train <- read.table("train.csv",header = TRUE,sep = ",")
View(train)
test <- read.table("test.csv",header = TRUE,sep = ",")
View(test)
store <- read.table("store.csv",header = TRUE,sep = ",")
View(store)

#Structure of data set
str(train)
str(test)
str(store)

#Converting Date from Character to Date format for Easy manipulation
train$Date <- as.Date(train$Date,"%m/%d/%Y")
test$Date <- as.Date(test$Date)


test[is.na(test$Open), ]
test[is.na(test)] <- 1
test$Open[test$Store == 622]

#Converting other variables to Factor Data type for Easy Manipulation 
plot.train <- train
plot.train$DayOfWeek <- as.factor(plot.train$DayOfWeek)
plot.train$Open <- as.factor(plot.train$Open)
plot.train$StateHoliday <- as.factor(plot.train$StateHoliday)
plot.train$SchoolHoliday <- as.factor(plot.train$SchoolHoliday)
plot.train$Promo <- as.factor(plot.train$Promo)

#Summary Of Data Set after all conversions
summary(plot.train)

#Converting the Variables in Test Data to Factor Type
plot.test <- test
plot.test$DayOfWeek <- as.factor(plot.test$DayOfWeek)
plot.test$Open <- as.factor(plot.test$Open)
plot.test$Promo <- as.factor(plot.test$Promo)
plot.test$StateHoliday <- as.factor(plot.test$StateHoliday)
plot.test$SchoolHoliday <- as.factor(plot.test$SchoolHoliday)

#Summary of Test Data
summary(plot.test)

plot.store <- store

#Merging Store Data Set to Test and Train Data Set
final.train <- merge(plot.train,plot.store)
final.test <- merge(plot.test,plot.store)

#Creating New variable Month,Date and Year to Visualize sales Induvidually
final.train$Month <- as.factor(format(as.Date(final.train$Date),"%m"))
final.train$Day <- as.factor(format(as.Date(final.train$Date),"%d"))
final.train$Year <- as.factor(format(as.Date(final.train$Date),"%Y"))

#Average Sales of Each store 
sales.by.id <- aggregate(final.train$Sales,by = list(final.train$Store),mean)
names(sales.by.id) <- c("Store","Average.Sales")
ggplot(data=sales.by.id,aes(x=Store,y=Average.Sales)) + geom_point(color="blue")
 
  # +ggtitle("Average sales by store id")
#We see that the average sales of each store is 5000$-1000$ range 

#Average Sales through each day of the week
sales.by.day <- aggregate(final.train$Sales,by = list(final.train$DayOfWeek),mean)
names(sales.by.day) <- c("DayOfWeek","Average.Sales")
ggplot(data=sales.by.day,aes(x=DayOfWeek,y=Average.Sales,fill=DayOfWeek)) +
  geom_bar(stat="identity") + ggtitle("Average sales by day of the week")
#We see that the sales is highest during the first day and it gradualy reduces except day 5 where
#there is slight raise form day 4

#Plot between sales and number of customers each day
ggplot(data=final.train,aes(x=Customers,y=Sales)) + geom_point(color="blue") 
          #+ ggtitle("Sales Vs No. of Customers")
#We see that sales and Number of customers are directly propotionsl

#Average sales by promotions
sales.by.promo <- aggregate(final.train$Sales,by = list(final.train$Promo),mean)
names(sales.by.promo) <- c("Promo","Average.Sales")
ggplot(data=sales.by.promo,aes(x=Promo,y=Average.Sales,fill=(as.integer(sales.by.promo$Promo)+1))) + 
  geom_bar(stat="identity") + ggtitle("Average Sales by promo")
#We can clearly see that sales is more during promotions and offers

#Average sales by State Holiday
sales.by.stateH <- aggregate(final.train$Sales,by = list(final.train$StateHoliday),mean)
sales.by.stateH <- sales.by.stateH[-1,]
names(sales.by.stateH) <- c("StateHoliday","Average.Sales")
ggplot(data=sales.by.stateH,aes(x=StateHoliday,y=Average.Sales,fill=c("blue","green","red"))) + 
  geom_bar(stat="identity") + ggtitle("Average sales by state holiday")
#We can clearly see that sales is more during public holidays and least during christmas holidays

#Average Sales by School Holidays
sales.by.schoolH <- aggregate(final.train$Sales,by = list(final.train$SchoolHoliday),mean)
names(sales.by.schoolH) <- c("SchoolHoliday","Average.Sales")
ggplot(data=sales.by.schoolH,aes(x=SchoolHoliday,y=Average.Sales,fill=SchoolHoliday)) + geom_bar(stat="identity") +
  ggtitle("Average sales by school holiday")

#Average sales by store type
sales.by.storeType <- aggregate(final.train$Sales,by = list(final.train$StoreType),mean)
names(sales.by.storeType) <- c("Store.Type","Average.Sales")
ggplot(data=sales.by.storeType,aes(x=Store.Type,y=Average.Sales,fill=Store.Type)) + geom_bar(stat="identity") +
  ggtitle("Average Sales by store type")
#We see that store type B has the highest sales among the 4 store types wheile store a,c,d have almost the 
#same average sales

#Average sales by assortment type
sales.by.assortment <- aggregate(final.train$Sales,by = list(final.train$Assortment),mean)
names(sales.by.assortment) <- c("Assortment","Average.Sales")
ggplot(data=sales.by.assortment,aes(x=Assortment,y=Average.Sales,fill=Assortment)) + geom_bar(stat="identity") +
  ggtitle("Average Sales by assortment type")
#We can see that Assortment type B has the highest sales

#
#sales.by.distance <- aggregate(final.train$Sales,by = list(final.train$CompetitionDistance),mean)
#names(sales.by.distance) <- c("CompDistance","Average.Sales")
#ggplot(data=sales.by.distance,aes(x=CompDistance,y=Average.Sales)) + geom_point(color="blue") + 
  #ggtitle("Average sales by Competition Distance")

#Average sales by moonth
sales.by.month <- aggregate(final.train$Sales,by = list(final.train$Month),mean)
names(sales.by.month) <- c("Month","Average.Sales")
ggplot(data=sales.by.month,aes(x=Month,y=Average.Sales,fill=Month)) + geom_bar(stat="identity") + 
  ggtitle("Average Sales by Month")
#We see that December has the highest sales

#Average sales By date
sales.by.date <- aggregate(final.train$Sales,by = list(final.train$Day),mean)
names(sales.by.date) <- c("Date","Average.Sales")
ggplot(data=sales.by.date,aes(x=Date,y=Average.Sales,fill=Date)) + geom_bar(stat="identity") + 
  ggtitle("Average sales by Date")
#We see that the sales is high during the first and last week of every month

#Average sales per year
sales.by.year <- aggregate(final.train$Sales,by = list(final.train$Year),mean)
names(sales.by.year) <- c("Year","Average.Sales")
ggplot(data=sales.by.year,aes(x=Year,y=Average.Sales,fill=Year)) + geom_bar(stat="identity") 
#+ 
 # ggtitle("Average sales by Year")
#We can see that sales is almost same with just a small increase thorugh each year

#Average sales through each day of every month
sales.by.monthDay <- aggregate(final.train$Sales,by=list(final.train$Month,final.train$Day),mean)
names(sales.by.monthDay) <- c("Month","Date","Average.Sales")
ggplot(data=sales.by.monthDay,aes(Month,Average.Sales,fill=Date)) + geom_bar(stat="identity") + facet_wrap(~Date) +
  ggtitle("Sales per date by month")
#We can clearly see that first and last week of every month the sales are high

#Average sales by store Promotion
sales.by.storeP <- aggregate(final.train$Sales,by = list(final.train$Store,final.train$Promo),mean)
names(sales.by.storeP) <- c("Store","isPromo","Average.Sales")
ggplot(data=sales.by.storeP,aes(Store,Average.Sales,color=isPromo)) + geom_point() 
#+
 # ggtitle("Average Sales of each store by promo")
#We can clearly see that sore with promotion and offers have higher sales that stores with no
#promotion





