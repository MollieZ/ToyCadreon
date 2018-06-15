#Created a project in my workspaces
#getwd()

###Library
library(xlsx)
library(ggplot2)
library(lubridate)

#Impport Data
RawData<-read.xlsx("toy_sales_data.xlsx",1,header = T)
str(RawData)
summary(RawData)
PlannedData<-read.xlsx("toy_sales_data.xlsx",2,header = T)

ggplot(RawData, aes(month)) + 
  geom_line(aes(y = sales, colour = "Sales")) + 
  geom_line(aes(y = tv_spend, colour = "TV Investment")) +
  geom_line(aes(y = digital_spend, colour = "Digital Investment"))
