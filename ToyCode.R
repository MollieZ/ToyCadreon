#Created a project in my workspaces
#getwd()

###Library
library(xlsx)
library(lubridate)

#Impport Data
RawData<-read.xlsx("toy_sales_data.xlsx",1,header = T)
str(RawData)
summary(RawData)
PlannedData<-read.xlsx("toy_sales_data.xlsx",2,header = T)

