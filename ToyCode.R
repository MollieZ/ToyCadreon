#Created a project, so wd is set. Please setwd to where the data file is to perfrom below code
#getwd()
#setwd() #Set to the data file is

###Library
library(xlsx)
library(ggplot2)
library(lubridate)

#Impport Data
RawData<-read.xlsx("toy_sales_data.xlsx",1,header = T)
str(RawData)
summary(RawData)
PlannedData<-read.xlsx("toy_sales_data.xlsx",2,header = T)

#Plot
ggplot(RawData, aes(month)) + 
  geom_line(aes(y = sales, colour = "Sales")) + 
  geom_line(aes(y = tv_spend, colour = "TV Investment")) +
  geom_line(aes(y = digital_spend, colour = "Digital Investment")) +
  ylab("Sales/Investment") +
  ggtitle("Sales and Media Spend") +theme(plot.title = element_text(hjust = 0.5))

#COrrelation
cor(RawData[,2:4]) #Positive, higher in digital spend

#Model
Model1<-lm(sales ~ tv_spend + digital_spend +trend + xmas,RawData)
summary(Model1)
plot(Model1, which = c(1, 2))
Mod1Res<-as.data.frame(coef(summary(Model1)))
VisData<-RawData
VisData$Fitted<-predict(Model1)
ggplot(VisData,aes(month)) +
  geom_line(aes(y=sales)) +
  geom_line(aes(y=Fitted,col="Predicted"))

#TV contribution for last two years
TvSale<-sum(RawData$tv_spend*coef(Model1)[2])
TotPreSal<-sum(VisData$Fitted)
TvPer<-TvSale/TotPreSal
TvROI<-TvSale/sum(VisData$tv_spend)

#Plan for 2018, asumming trend has stopped
PlannedData$trend<-24
PlannedData$xmas<-0
predict(Model1, PlannedData, interval="predict") 


Model2<-lm(sales ~ tv_spend + digital_spend +trend + xmas-1,RawData)
summary(Model2)
plot(Model2, which = c(1, 2))

