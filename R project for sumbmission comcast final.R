library(readxl)
data <- read.csv("E:/R project submission/Comcast Telecom Complaints data.csv")
head(data)
str(data)
View(data)


#here i'm using lubridate library for formatting the Date Column

library(lubridate)

li<-parse_date_time(x = data$Date,
                    orders = c("d m y", "d B Y", "m/d/y"),
                    locale = Sys.getlocale("LC_TIME"))
data2<-data
data2$Date <- li


data2$Month <- format(as.Date(data2$Date), "%m")
data2$Month<- month.abb[as.integer(data2$Month)]

head(data2)


View(data2)


#Analysis part

#using dplyr

library(dplyr)
data_date <- data2 %>% group_by(Date) %>% dplyr::summarise(frequency=n())
df <-data_date[order(-data_date$frequency),]
dff<-head(df)
dff
View(dff)

#dff shows that the highest number of compaints reported on 2015-06-24 i.e., 218


library(ggplot2)

ggplot(data_date, aes(Date, frequency, group = 1)) + 
  geom_point() + 
  geom_line() +
  xlab("Date") + 
  ylab("Number of Complaints")

#Provide the trend chart for the number of complaints at monthly and daily granularity levels.

#(Monthly) The trend chart shows that the highest complaints reported on June 2015

ggplot(dff, aes(Date, frequency, group = 1)) + 
  geom_point() + 
  geom_line() +
  xlab("Date") + 
  ylab("Number of Complaints")

#(Monthly) The trend chart shows that the highest complaints reported on 24th June


data_month<-data2 %>% 
  group_by(Month) %>% dplyr :: summarise(frequency = n())
data_month


data2$Month <- as.factor(data2$Month)
levels(data2$Month)

ggplot(data_month, aes(Month, frequency, group = 1)) + 
  geom_point() + 
  geom_line() +
  xlab("Month") + 
  ylab("Number of Complaints")

#Again its showing that June has the maximum number of complaints

data3<-data2%>% mutate(Customer.Complaint = tolower(Customer.Complaint))
CustTable <- table(data3$Customer.Complaint)
CustTable <- data.frame(CustTable)
filtered<-CustTable %>% 
  rename(
    CustomerComplaintType = Var1,
    Frequency = Freq
  )
final <- filtered %>% arrange(desc(Frequency))


final_most<-head(final,20)
final_most

View(final_most)   #Complaints types


#Which complaint types are maximum i.e., around internet, network issues, or across any other domains.

ggplot(head(final_most,6), aes(CustomerComplaintType, Frequency)) +
  geom_bar(stat = "identity")

#by looking the graph, the maximum complaint types are-
#comcast data caps, comcast internet, and comcast service

#Create a new categorical variable with value as Open and Closed. Open & Pending is to be categorized as Open and Closed & Solved is to be categorized as Closed

install.packages("stringr")
library(stringr)
install.packages("tidyverse")
library(tidyverse)

levels(data$Status)

library(plyr)
library(dplyr)
data$Status_New<-revalue(data$Status, c(Pending = "Open", Solved = "Closed"))
head(data)
View(data)

#so we have created "Status1" column which has only two levels.
#merged the pending requests as "open" and solved requests as "closed

levels(data$State)


tab <- table(data$State,data$Status_New)
tab <- cbind(tab, Total = rowSums(tab))
head(tab,15)
View(head(tab,15))

#Provide state wise status of complaints in a stacked bar chart

library(gridExtra)
ggplot(data, aes(y = State)) + geom_bar(aes(fill = Status_New))

#Georgia and Florida are the two states where Comcast issues are resolved and closed.

levels(data$Received.Via)

ggplot(data, aes(y = Received.Via )) + geom_bar(aes(fill = Status_New))


df1 <- table(data$Received.Via, data$Status_New)
df1 <- cbind(df1, Total = rowSums(df1))
df1
View(df1)


#Provide the percentage of complaints resolved till date, which were received through the Internet and customer care calls

slices <- c(864, 255)
lbls <- c("Closed", "Open")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Received Via Call")

#So, for Complaints recieved via  Call
#Closed = 77%. Opened = 23%

slices <- c(843, 262)
lbls <- c("Closed", "Open")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Received Via Internet")

#So, for Complaints recieved via Internet
#Closed = 76%. Opened = 24%


