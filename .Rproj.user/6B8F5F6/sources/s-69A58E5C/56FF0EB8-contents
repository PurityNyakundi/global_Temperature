library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
library(broom)
library(ranger)


Temp<-read_xlsx("global_temperature.xlsx")
Temp

str(Temp)

glimpse(Temp)

ggplot(Temp,aes(y = degrees_celsius,x = year),fill = color)+
  geom_point()+
  geom_smooth(method = "lm",se = FALSE)+
  ggtitle("degrees_celsious against year")+
  theme(panel.background = element_rect(fill = "white",colour = "black"))
#so obvious that the global temperature are increasing over the years.
#The linear model approves the same

cor(Temp$year,Temp$degrees_celsius,use = "everything",method = "pearson")

cor(Temp$year,Temp$degrees_celsius,use = "everything",method = "spearman")
 
#the above two methods correlation show almost same corelation coefficient.
#The two variables are moderately corelated

#Check more on the linear model

model<-lm(degrees_celsius ~ year,data = Temp)
model
summary(model)

glance(model)
newYear<-data.frame(year = 2012)
prediction<-predict(model,newYear)
print(prediction)


ggplot(Temp,aes(y = degrees_celsius,x = year),fill = "blue")+
  geom_line()+
  ggtitle("degrees_celsious against year")+
  theme(panel.background = element_rect(fill = "white",colour = "black"))

ggplot(Temp,aes(y = degrees_celsius,x =as.factor(year)),fill = "blue")+
  geom_point()+
  ggtitle("degrees_celsious against year")+
  theme(panel.background = element_rect(fill = "white",colour = "black"))
#Trying to use random forest but seems my R squared is too low
cmodel<-ranger(degrees_celsius ~ year,Temp,
               num.trees = 500,
               respect.unordered.factors = "order")
cmodel
rt<-predict(cmodel,newYear)
print(rt)
