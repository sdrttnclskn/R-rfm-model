#Loading data
d = read.csv('data.csv')
#including Libraries
install.packages("data.table")
library(data.table)
install.packages("tidyr")
library(tidyr)
install.packages("knitr")
library(knitr)
#Data Pre processing
#Replacing ) with NA
d = d %>%
  mutate(Quantity = replace(Quantity, Quantity<=0, NA),
         UnitPrice = replace(UnitPrice, UnitPrice<=0, NA))
#Dropping NUll values
d = d %>%
  drop_na()
#Converting vales as Factors
d = d  %>% 
  mutate(InvoiceDate=as.Date(InvoiceDate, '%m/%d/%Y %H:%M'), CustomerID=as.factor(CustomerID), 
         Country=as.factor(Country)) 
# Calculating total amount
d = d  %>% 
  mutate(total_dolar = Quantity*UnitPrice)
#dropping the coloumns
d$StockCode=NULL
d$Description=NULL
d$Quantity=NULL
d$Country=NULL
d$UnitPrice=NULL
#RFM analysis 
#install.packages("didrooRFM")
library(didrooRFM)
customerData <- data.frame(d$InvoiceNo,d$CustomerID,d$InvoiceDate,d$total_dolar)
x=findRFM(customerData, recencyWeight = 4, frequencyWeight = 4,
          monetoryWeight = 4)
