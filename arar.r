library(forecast)
library(itsmr)
library(tseries)

# for making data readable in R
#write.table(data_excess_cc,file="data_excess_cc.csv",sep = ",",col.names=TRUE)
table<-read.csv("data.csv",header=TRUE,sep=",")
head(table)

# for stationarity
KPassValue = kpss.test(table$avgloss) # if p-value is smaller than 0.05(standard) then its not

p_value =KPassValue$p.value
if(p_value <0.05) 
{
  print("Non Standard values")
}
#stationary and vice versa.

# for arima
Ar<- arar( table$avgloss,h=10,opt=2 ) 
 # opt means it will give values and graph  both.
# in summary  pred value gives the forecasted value
# for checking how effectively forecasted values are we have to see
#arar$ pred value with actual value.

