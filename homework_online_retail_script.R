# Title       : Homework_Cleansing_Data_Online_Retail.R 
# Author      : Nayla
# Data source : https://archive.ics.uci.edu/ml/datasets/online+retail


#Import dataset
library(readxl)
Online_Retail <- read_excel("Online Retail.xlsx")
View(Online_Retail)

#cek ringkasan data dan missing value
summary(Online_Retail)

#terdapat missing value pada variable CustomerID
library(tidyverse)
library(lubridate)
library(DataExplorer)

#cek plot prosentase missing value per variabel
plot_missing(Online_Retail)

#terdapat missing value pada variable Description dan CustomerID

#drop +- 25% data CustomerID yang kosong, diasumsikan tidak terjadi transaksi jika ter-capture CustomerID
ol_data <- Online_Retail[!is.na(Online_Retail$CustomerID),]
summary(ol_data)
plot_missing(ol_data)

#merapikan dataset 
recency <- ol_data %>% group_by(CustomerID) %>% arrange(desc(InvoiceDate)) %>% filter(row_number()==1) %>% mutate(recency = as.numeric(as.duration(interval(InvoiceDate,ymd("2011-12-31"))))/86400) %>% select(CustomerID, recency)  
monetary <- ol_data %>% group_by(CustomerID) %>% summarise(monetary=sum(UnitPrice*Quantity))    
frequency <- ol_data %>% group_by(CustomerID) %>% summarise(frequency = n_distinct(InvoiceNo)) 

#join variable
data_RFM <- recency %>% left_join(frequency,by="CustomerID") %>% left_join(monetary,by="CustomerID")

summary(data_RFM)

#export to csv
write.csv(data_RFM,"homework_online_retail_clean.csv",quote=F,row.names = F)
