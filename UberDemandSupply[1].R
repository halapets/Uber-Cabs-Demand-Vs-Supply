#reading the uber data
Uber <- read.csv("Uber Request Data.csv",header = TRUE,
                 stringsAsFactors = FALSE,
                 na.strings = c("NA","na","-",".",""," ","_"))
library(ggplot2)
library(lubridate)
library(dplyr)

summary(Uber)

#==============================================Data Preparation===================================

#------------------------------------Missing value Treatment for missing Driver ID-------------------

#summary shows that there are missing values for Driver ID
#finding the index of the mssing values
index_driverid <- which(is.na(Uber$Driver.id))#2650 missing values

#reviewing the subset (first and last 50 obs) where driver ID is missing
head(Uber[index_driverid,],50)
tail(Uber[index_driverid,],50)

unique(Uber[index_driverid,"Status"])#Status"No Car available has missing driver IDs
unique(Uber[index_driverid,"Drop.timestam"])
#Status"No Car available has missing driver IDs and Drop time

#all the obsevations where Driver Id is missing has "No Car Available"
#status and hence no drop time also recorded.

#hence missing values for Driver ID are labelled as "Missing"
Uber[index_driverid,"Driver.id"] <- "Missing"

length(unique(Uber$Driver.id))#301 unique driver ids

#-------------------------------------------Request ID----------------------------------------------

#finding if any duplicates in request id
index_requestid <- which(duplicated(Uber$Request.id))
#No duplicated values. Every cab request is unique.

#--------------------------------------------Request Timestamp------------------------------------

#breaking down date into month,hour,year,weekday and hour for further analysis
tail(Uber$Request.timestamp,20)
head(Uber$Request.timestamp,20)

#it's observed that there are two date formats
#hence making the string in one format before converting it to Date format

Uber$Request.Date <- gsub("/","-",Uber$Request.timestamp)

#converting date part of the string to date format
Uber$Request.Date <- as.Date(Uber$Request.Date,format="%d-%m-%Y")
str(Uber$Request.Date)

#extracting month
Uber$Request.Month <- month(Uber$Request.Date)
unique(Uber$Request.Month)
#data is only for July month

#extracting weekday
Uber$Request.Day <- weekdays(Uber$Request.Date)
unique(Uber$Request.Day)
#data is for Monday to Friday

#extracting year
Uber$Request.Year <- year(Uber$Request.Date)
unique(Uber$Request.Year)
#data is for 2016 year

#extracting hour part of the string as different variable for analysis
Uber$Request.Time <- substr(Uber$Request.timestamp,regexpr(':',
                                                           Uber$Request.timestamp)-2,
                            regexpr(':',Uber$Request.timestamp)-1)

unique(Uber$Request.Time)
#It is observed that for some data points time is recorded as "0".
#it is observed that the single digit time is duplicated for some cases
#due to zero addition in the beginning for some of single digit time

#Reviewing the sample of these records

head(Uber[Uber$Request.Time == "00",],20)
head(Uber[Uber$Request.Time == "0",],20)
#this "0" time represents 12 PM

#treatment for duplicated single digits
Uber$Request.Time <- as.numeric(Uber$Request.Time)
Uber$RequestTime <- rep(0,nrow(Uber))

for (i in 1:nrow(Uber)){
  if (Uber$Request.Time[i]== 01)
  {
    Uber$RequestTime[i]= 1
  } else if (Uber$Request.Time[i]== 02)
  {
    Uber$RequestTime[i]= 2
  } else if(Uber$Request.Time[i]== 03)
  {
    Uber$RequestTime[i]= 3
  } else if(Uber$Request.Time[i]== 04)
  {
    Uber$RequestTime[i]= 4
  } else if(Uber$Request.Time[i]== 05)
  {
    Uber$RequestTime[i]= 5
  } else if (Uber$Request.Time[i]== 06)
  {
    Uber$RequestTime[i]= 6
  } else if(Uber$Request.Time[i]== 07)
  {
    Uber$RequestTime[i]= 7
  } else if (Uber$Request.Time[i]== 08)
  {
    Uber$RequestTime[i]= 8
  } else if (Uber$Request.Time[i]== 09)
  {
    Uber$RequestTime[i]= 9
  } else 
  {
    Uber$RequestTime[i]= Uber$Request.Time[i] 
  }
}

unique(Uber$RequestTime)

#----------------------------------------------------DropTime Stamp---------------------------------

#Under Drop Time Stamp there is NA value for the trip status cancelled and no cars available 
#which is intuitively correct.

#finding index of the missing values
index_droptimestamp <- which(is.na(Uber$Drop.timestamp))
length(index_droptimestamp)/nrow(Uber)#58% missing drop time

#labelling the missing values as "missing"
Uber$Drop.timestamp[index_droptimestamp] <- "Missing"


#----------------------------------------------------New Derived variable-Uber Supply----------------------------
#Assumption used to create Supply Variabl:Supply= Trip Completed
Uber$Supply <- ifelse(Uber$Status == "Trip Completed",1,0)

##Demand for Cab = No of Can Requests


#==================================================EDA===========================================

#-------------------------------------------Pickup Point------------------------------------------
#analysis around Uber Demand Vs Supply for Pickup Point

#Identifying frequency (Cab requests demand) by Pickup Point
table(Uber$Pickup.point)

Pickup_Demand <- Uber%>%group_by(Pickup.point)%>%
  summarise(count=n(),Perc_Demand=round((count/nrow(Uber)*100)))

ggplot(Pickup_Demand,aes(x=Pickup.point,y=count))+
  geom_bar(stat="identity",aes(fill= as.factor(Pickup.point)))+
  xlab("PickupPoint")+ ylab(NULL)+
  ggtitle("Cab Requests by Pickup Point",
  subtitle = "There is no sigificant difference for Cab Demand by Pickup Point")+
  theme(title = element_text(hjust = 0.5,face = "bold"))+
  theme_bw()+ theme(axis.text.y = element_blank())+
  geom_label(label=paste(Pickup_Demand$count,"Cabs (",Pickup_Demand$Perc_Demand,"%)",sep=""))+
  labs(fill="Pickup Point")

#the frequency for City pick up-3507 (52% of Total Cab Requests)
#the frequency for Airport pickup-3238 (48% of total Cab Requests)
#Hypothesis- There is no significant difference for Cab Demand (Cab Requests) 
#from Airport to City and City to Airport. 


#Identifying Cab Supply by Pickup Point
Pickup_Supply <- Uber%>%group_by(Pickup.point)%>%
  summarise(count=n(),Supply=sum(Supply),
            Supply_Perce=round(Supply/nrow(Uber)*100))

ggplot(Pickup_Supply,aes(x=Pickup.point,y=Supply))+
  geom_bar(stat="identity",aes(fill=as.factor(Pickup.point)))+xlab("Pickup.point")+
  ylab(NULL)+
  ggtitle("Trips Completed (Supply) by Pickup Point",
          subtitle = "There is no sigificant difference for Cab Supply by Pickup Point")+
  theme_bw()+
  theme(title = element_text(hjust = 0.5,face="bold"))+
  theme(axis.text.y = element_blank())+
  geom_label(label= paste(Pickup_Supply$Supply,"Cabs (",
                          Pickup_Supply$Supply_Perce,"%)",sep=""))+
  labs(fill="Pickup Point")

#supply of Cabs is 20-22% of total cab requests

#-------------------------------------------Trip Status---------------------------------------------------
#Identifying frequency by status
table(Uber$Status)
prop.table(table(Uber$Status))

Status <- Uber%>%group_by(Status)%>%summarise(count=n())%>%
  mutate(Perc_frequency=round((count*100)/nrow(Uber)))

ggplot(Status,aes(x=Status,y=Perc_frequency))+
  geom_bar(stat="identity",aes(fill= as.factor(Status)))+xlab("Cab Request Status")+
  ylab(NULL)+ggtitle("Percentage of Cab Requests by Cab Request Status",
  subtitle = "42% Requested Trips Completed and 58% not completed")+
  theme_bw()+
  theme(title = element_text(hjust = 0.5,face="bold"))+
  theme(axis.text.y = element_blank())+
  geom_label(label= paste(Status$Perc_frequency,"%",sep=""),position = position_dodge(0.9))+
  labs(fill="Cab Request Status")

#Cancelled trips-1264,No Cars Available-2650,Trip Completed-2831
#42% of the cab requestes (trips) are completed,
#58% of the cab requests have No Cars Available status or cancelled.
#Cab requests with No Cars Available  and cancelled status are on higher side than
#the cab requests with "Trip Completed Status".Demand is higher than Supply

#-----------------------------------Analysing Pickup Point and Trip Status---------------------------

x <- table(Uber$Pickup.point,Uber$Status)

write.csv(x,"Pickup Point Vs Trip Status.csv")

#No of trips cancelled for Airport to city-198
#No of Trips Cancelled for city to airport-1066*
#No of trips where no cars available for Airport to City-1713*
#no of trips where no cars available for city to airport-937

#Out of cancelled 1264 trips,1066 trips are city to airport trips 
#Out of No car avaialble requestes 2650, 1713 requests are from Airport to City 
#and 937 requests are from city to airport
#Supply shortage of cabs for City to Airport Trips is due to cancellation of cabs 
#and Cars unavailability
#Supply shortage of Cabs for Airport to city is majorly due to Cars unavailability

#------------------------------------------Weekdays----------------------------------------------
#identifying frequency of trips across weekdays

Weekdays <- Uber%>%group_by(Request.Day)%>%summarise(count=n())%>%
  mutate(Perc_frequency=round((count*100)/nrow(Uber)))

ggplot(Weekdays,aes(x=Request.Day,y=count))+
  geom_bar(stat="identity",aes(fill=as.factor(Request.Day)))+
  xlab("WeekDay")+
  ylab(NULL)+ggtitle("Cab Requests by WeekDay")+theme_minimal()+
  theme(title = element_text(hjust = 0.5,face="bold"))+
  theme(axis.text.y = element_blank())+
  labs(fill="Weekdays")+
  geom_label(label= paste(Weekdays$count,"Cabs",sep=""),position = position_dodge(0.9))

#Identifying Demand trend of Cabs by Weekday
Weekday_demand <- Uber%>%group_by(Request.Day)%>%
  summarise(Demand=n(),Perc_Demand=round(Demand/nrow(Uber)*100))

ggplot(Weekday_demand,aes(x=Request.Day,y=Perc_Demand))+
  geom_bar(stat="identity",aes(fill= as.factor(Request.Day)))+xlab("Weekdays")+
  ylab(NULL)+ggtitle("Percentage of Cab Requests by Weekdays",
  subtitle = "Not Significant Variation in Cab requests by Weekdays")+
  theme_minimal()+
  theme(title = element_text(hjust = 0.5,face="bold"))+
  theme(axis.text.y = element_blank())+
  labs(fill="Weekdays")+
  geom_label(label= paste(Weekday_demand$Demand),position = position_dodge(0.9))


#Identifying Supply trend of Cabs by Weekday  

Weekday_Supply <- Uber%>%group_by(Request.Day)%>%
  summarise(Demand=n(),Supply=sum(Supply),perc_supply=round(Supply/Demand*100))


ggplot(Weekday_Supply,aes(x=Request.Day,y=Supply))+
  geom_bar(stat="identity",aes(fill=as.factor(Request.Day)))+xlab("Weekdays")+
  ylab(NULL)+ggtitle("% Supply for Demand by Weekdays",
  subtitle = "Not Significant Variation in % Supply for Demand across Weekdays" )+
  theme_bw()+
  theme(title = element_text(hjust = 0.5,face="bold"))+
  theme(axis.text.y = element_blank())+
  geom_label(label= paste(Weekday_Supply$Supply,"Cabs (",
                          Weekday_Supply$perc_supply,"%)",sep=""),size=3.5)+
  labs(fill="Weekdays")

#frequency of trips, cabs requested(Demand),Supply vs Demand % aross weekdays 
#are similar

#--------------------------------------Demand and Supply Trend Across Time---------------------------
#identifying frequency of trips across time
hist(as.numeric(Uber$RequestTime))
Time <- Uber%>%group_by(RequestTime)%>%summarise(count=n())%>%arrange(desc(count))
#frequency of trips requested are higher during 5 To 10 PM and 5 to 10 AM 
#compared to the rest of the time


ggplot(data = Uber, aes(x = RequestTime, fill = Status)) +
  geom_bar() +  theme(title = element_text(face="bold")) +
  labs(title ="Cab Demand/Supply Pattern by Time", 
  subtitle = "Peak hrs 5-10AM: High Cancellations, Peak hrs 5-10PM: High Cab unavailability ")+ 
  ylab( "Cab requests")+xlab("Hrs")

#henec creating Time Groups based on frequency of Requests(Business Demand) and
#Cab Demand/Supply Pattern by Time
Uber$TimeWindow <- rep(0,nrow(Uber))

for (i in 1:nrow(Uber)){
  if (Uber$RequestTime[i] >= 5 & Uber$RequestTime[i] <= 10){
    Uber$TimeWindow[i] = "5AM to 10AM"
  } else if (Uber$RequestTime[i] >= 17 & Uber$RequestTime[i] <= 22){
    Uber$TimeWindow[i] = "5PM to 10PM"
  } else {
    Uber$TimeWindow[i] = "11AM to 4PM"
  }
}

#Identifying the Cab requests Frequency (Demand) across Time zones 
TimeWindow_Demand <- Uber%>%group_by(TimeWindow)%>% 
  summarize(Demand=n(),Perc_Demand=round(Demand/nrow(Uber)*100))

ggplot(TimeWindow_Demand,aes(x=TimeWindow,y=Demand))+
  geom_bar(stat="identity",aes(fill= as.factor(TimeWindow)))+xlab("Time")+
  ylab(NULL)+ggtitle("No of Cab requests by TimeWindow",
  subtitle = "Cab requests are higher between 5 to 10 AM and 5 to 10 PM")+
  theme_bw()+
  theme(title = element_text(hjust = 0.5,face="bold"))+
  theme(axis.text.y = element_blank())+
  geom_label(label= paste(TimeWindow_Demand$Demand,"Cabs (",
  TimeWindow_Demand$Perc_Demand,"%)",sep=""),position = position_dodge(0.9))+
  labs(fill="TimeWindow")

#Identifying the Supply Trend across Time zones 
TimeWindow_Supply<- Uber%>%group_by(TimeWindow)%>% summarize(Demand=n(),Supply=sum(Supply),
                                                             perc_Supply=round(Supply/nrow(Uber)*100))

ggplot(TimeWindow_Supply,aes(x=TimeWindow,y=Supply))+
  geom_bar(stat="identity",aes(fill=as.factor(TimeWindow)))+xlab("Time")+
  ylab(NULL)+ggtitle("No of Trips completed by TimeWindow",
  subtitle = "Supply of Cab between 5 to 10 AM and 5 to 10 PM is same as rest of the day")+
  theme_bw()+
  theme(title = element_text(hjust = 0.5,face="bold"))+theme(axis.text.y = element_blank())+
  geom_label(label= paste(TimeWindow_Supply$Supply,"Cabs (",TimeWindow_Supply$perc_Supply,"%)",
  sep=""))+labs(fill="TimeWindow")

# Demand of Cabs for 5AM to 10 AM and 5PM to 10PM is higher almost 74% of total cabs requested
#However, there is no increase in Supply with respect to increase in demand for these timewindows.
#the supply of cabs across this time window is 28% of the cabs requested

#----------------------Demand and Supply across Time windows by Pickup Point-------------------
UberDemandSupply<- Uber%>%group_by(TimeWindow,Request.Day,Pickup.point)%>% 
  summarize(Demand=n(),Supply=sum(Supply))

ggplot(UberDemandSupply,aes(x=Demand,y=Supply))+
  geom_point(aes(size=TimeWindow,color=as.factor(TimeWindow)))+xlab("Demand")+
  ylab("Supply")+ggtitle("Demand Vs Supply across TimeIndows and Pickup Point ")+theme_bw()+
  theme(title = element_text(hjust = 0.5,face="bold"))+
  labs(color="TimeWindow",size="TimeWindow")+
  facet_grid(Pickup.point~.)

#from 5AM to 10AM, there is increase in demand of cabs for City to Airport pickup whereas
#from 5PM to 10PM, there is increase in demand of cabs for Airport to City whereas 
#there is no corresponding proportional increase in the supply for both the cases

ggplot(UberDemandSupply,aes(x=Demand,y=Supply))+
  geom_point(aes(size=TimeWindow,color=as.factor(TimeWindow)))+xlab("Demand")+
  ylab("Supply")+ggtitle("Demand Vs Supply across time zones by Weekdays")+theme_bw()+
  theme(title = element_text(hjust = 0.5,face="bold"))+
  labs(color="TimeWindow",size="TimeWindow")+
  facet_grid(Pickup.point~Request.Day)
#there is no impact of weekdays on Demand and Supply of Cabs 



