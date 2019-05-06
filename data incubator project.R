##Data Incubator Project
library(ggplot2)

setwd("C:/Users/Pablo/Documents/Career/Data Incubator")
complaint_data=as.data.table(read.csv("Consumer_Complaints.csv"))

##Do more complaints hurt stock prices?

complaint_data[,complaints:=1]
complaint_data$Date.received=as.Date(complaint_data$Date.received,format="%m/%d/%Y")

number_complaints=complaint_data[,sum(complaints),by=Date.received]
setnames(number_complaints,"Date.received","Date")


FS_ETF=as.data.table(read.csv("Financial Services ETF.csv"))
FS_ETF$Date=as.Date(FS_ETF$Date,format="%m/%d/%Y")

##Calculate returns ETF

FS_ETF[,log_ret:=log(shift(Adj.Close,type="lead")/Adj.Close)]
FS_ETF[,log_ret:=shift(log_ret)]
data=merge(FS_ETF,number_complaints,by="Date")
data[,log_complaints:=log(complaints)]
setnames(data,"V1","complaints")


qplot(log_ret,complaints,data=data,col=I("blue"),main="Financial Services Returns
       and Number of Complaints")+theme_bw()+
  geom_smooth(col=I("green"),method="lm",na.rm=TRUE)+
  geom_smooth(col=I("red"),na.rm=TRUE, span = 0.3) 




