##Data Incubator

library(data.table)


setwd("C:/Users/Pablo/Documents/Career/Data Incubator")

data=as.data.table(read.csv("NYPD_Motor_Vehicle_Collisions.csv", sep=";"))
setnames(data,"ï..DATE","date")
setorder(data,date)

data[,date:=as.Date(date,"%m/%d/%Y")]

##Question 1 What is the total number of persons injured 
##in the dataset (up to December 31, 2018?)

injured=data[date<="2018-12-31",sum(NUMBER.OF.PERSONS.INJURED,na.rm=TRUE)]
injured

##What proportion of collisions
##in 2016 resulted in injury or death of a cyclist?

data[,year:=year(date)]
data[,month:=month(date)]

cyclyst_deathinjury=data[year==2016,
                         sum(cbind(NUMBER.OF.CYCLIST.INJURED,
                                   NUMBER.OF.CYCLIST.KILLED))]
cyclyst_deathinjury

##Obtain the number of vehicles involved in each collision in 2016. 
##Group the collisions by zip code and compute the sum of all 
##vehicles involved in collisions in each zip code, then report the maximum of these values.


data[,collision:=1]
collision_2016=data[year==2016,cbind(collision,ZIP.CODE)]
collision_2016=as.data.table(collision_2016)  
collision_2016=collision_2016[complete.cases(ZIP.CODE),]

total_collision2016=collision_2016[,sum(collision),by=ZIP.CODE]
setnames(total_collision2016,"V1","number_collision")
max_collision=max(total_collision2016$number_collision)
max_collision

##Do winter driving conditions lead to more multi-car collisions? Compute the
##rate of multi car
##collisions as the proportion of the number of collisions involving 3
##or more cars to the total number of collisions for each month of 2017. 
##Calculate the chi-square test statistic for testing whether a collision 
##is more likely to involve 3 or more cars in January than in May.

collision_2017=data[year==2017 & as.character(data$VEHICLE.TYPE.CODE.3)!="", 
              cbind(collision,month)]

collision_2017=as.data.table(collision_2017)
total_collision2017=collision_2017[,sum(collision),by=month]
setnames(total_collision2017,"V1","total")

total_collision2017$rate=total_collision2017$total/sum(total_collision2017$total)*100

##Null hypothesis the rate in January and May are equal to each other.

chi_square=(total_collision2017$rate[1]-total_collision2017$rate[5])^2/
  total_collision2017$rate[1]
  

pvalue=1-pchisq(chi_square,1)
pvalue

##What proportion of all collisions in 2016 occured in Brooklyn? 
##Only consider entries with a non-null value for BOROUGH.

collision_2016=data[year==2016 & as.character(BOROUGH)!="",sum(collision),by=BOROUGH]
collision_2016$V1[2]/sum(collision_2016$V1)

##For each borough, compute the number of accidents per capita involving alcohol in 2017. 
##Report the highest rate among the 5 boroughs. 
##Use populations as given by https://en.wikipedia.org/wiki/Demographics_of_New_York_City.

borough=c("BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND")
population=as.integer(c(1471160,2648771,1664727,2358582, 479458))
population_borough=as.data.table(matrix(cbind(borough,population),nrow=5,ncol=2))
colnames(population_borough)=c("BOROUGH","Population")

data_2017=data[year==2017 & as.character(BOROUGH)!="",]
data_2017_alcohol=data_2017[CONTRIBUTING.FACTOR.VEHICLE.1=="Alcohol Involvement"
                            | CONTRIBUTING.FACTOR.VEHICLE.2=="Alcohol Involvement"
                            | CONTRIBUTING.FACTOR.VEHICLE.3=="Alcohol Involvement"
                            | CONTRIBUTING.FACTOR.VEHICLE.4=="Alcohol Involvement"
                            | CONTRIBUTING.FACTOR.VEHICLE.5=="Alcohol Involvement",]

data_2017_alcohol=unique(data_2017_alcohol,by="UNIQUE.KEY")
alcohol_borough=data_2017_alcohol[,sum(collision),by=BOROUGH]
setnames(alcohol_borough,"V1","collisions")
alcohol_borough=merge(alcohol_borough,population_borough, by="BOROUGH")
alcohol_borough$rate=alcohol_borough$collisions/as.integer(
  alcohol_borough$Population)

max(alcohol_borough$rate)


##Consider the total number of collisions each year from 2013-2018. 
##Is there an apparent trend? Fit a linear regression for 
##the number of collisions per year and report its slope.

setorder(data,year)
collisions_yearly=data[,sum(collision),by=year]
setnames(collisions_yearly,"V1","collisions")

reg1=lm(collisions_yearly$collisions~collisions_yearly$year)
summary(reg1)

