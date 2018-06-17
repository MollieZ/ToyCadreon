#Created a project, so wd is set. Please setwd to where the data file is to perfrom below code
#getwd()
#setwd() #Set to the data file is

###Library
library(xlsx)
library(ggplot2)

#Impport Data
RawData<-read.xlsx("toy_sales_data.xlsx",1,header = T)
str(RawData)
summary(RawData)
apply(RawData[,2:4],2,sum)
PlannedData<-read.xlsx("toy_sales_data.xlsx",2,header = T)

#Plot sales against media spend
ggplot(RawData, aes(month)) + 
  geom_line(aes(y = sales, colour = "Sales")) + 
  geom_line(aes(y = tv_spend, colour = "TV Investment")) +
  geom_line(aes(y = digital_spend, colour = "Digital Investment")) +
  ylab("Sales/Investment") +
  ggtitle("Sales and Media Spend") +theme(plot.title = element_text(hjust = 0.5))

#COrrelation between sales and media spend
cor(RawData[,2:4]) #Positive, higher in digital spend

#Model
Model1<-lm(sales ~ tv_spend + digital_spend +trend + xmas,RawData)
summary(Model1)
plot(Model1, which = c(1, 2))
Mod1Res<-as.data.frame(coef(summary(Model1)))
VisData<-RawData
VisData$Fitted<-predict(Model1)
ggplot(VisData,aes(month)) +
  geom_line(aes(y=sales,col = "Actual")) +
  geom_line(aes(y=Fitted,col="Predicted")) +
  scale_color_manual(values = c("black","red"))  + 
  ggtitle("Sales Vs Predicted") +theme(plot.title = element_text(hjust = 0.5))

#TV contribution for last two years
TvSale<-sum(RawData$tv_spend*coef(Model1)[2])
TotPreSal<-sum(VisData$Fitted)
TvPer<-TvSale/TotPreSal
TvROI<-TvSale/sum(VisData$tv_spend)

#Plan for 2018, asumming trend has stopped
PlannedData$trend<-24
PlannedData$xmas<-0
predict(Model1, PlannedData, interval="predict") 


#######Advanced Model########################################
#Apply s transformation to media data
a_tv<-200
b_tv<-0.00001
tv_s_fun<-function(x){a_tv*x/(1+b_tv*x)}
plot(tv_s_fun(1:3000000),type = "l")

a_dig<-150
b_dig<-0.000001
dig_s_fun<-function(x){a_dig*x/(1+b_dig*x)}
plot(dig_s_fun(1:2000000),type = "l")

RawData$tv_s<-lapply(RawData$tv_spend,tv_s_fun)
RawData$dig_s<-lapply(RawData$digital_spend,dig_s_fun)

#apply ad-stock
tv_ads<-0.1
dig_ads<-0.001
RawData$tv_s_ads<-as.numeric(filter(x=RawData$tv_s,filter = tv_ads, method = "recursive"))
RawData$dig_s_ads<-as.numeric(filter(x=RawData$dig_s,filter = dig_ads, method = "recursive"))

Model2<-lm(sales ~ tv_s_ads + dig_s_ads +trend + xmas-1,RawData)
summary(Model2)
plot(Model2, which = c(1, 2))

VisData$Fitted2<-predict(Model2)
ggplot(VisData,aes(month)) +
  geom_line(aes(y=sales)) +
  geom_line(aes(y=Fitted2,col="Predicted"))

#TV contribution for last two years using new model
TvSale2<-sum(RawData$tv_s_ads*coef(Model2)[1])
TotPreSal2<-sum(VisData$Fitted2)
TvPer2<-TvSale2/TotPreSal2
TvROI2<-TvSale2/sum(VisData$tv_spend)


#Plan for 2018 based on new model, asumming trend has stopped
PlannedData$trend<-24
PlannedData$xmas<-0
PlannedData$tv_s<-lapply(PlannedData$tv_spend,tv_s_fun)
PlannedData$dig_s<-lapply(PlannedData$digital_spend,dig_s_fun)
PlannedData$tv_s_ads<-as.numeric(filter(x=PlannedData$tv_s,filter = tv_ads, method = "recursive"))
PlannedData$dig_s_ads<-as.numeric(filter(x=PlannedData$dig_s,filter = dig_ads, method = "recursive"))
predict(Model2, PlannedData, interval="predict") 

###Compare spend with transformed spend
#ggplot(RawData, aes(month)) + 
#  geom_line(aes(y = tv_spend, colour = "TV Investment")) +
#  geom_line(aes(y = tv_s_ads, colour = "TV trans")) 

