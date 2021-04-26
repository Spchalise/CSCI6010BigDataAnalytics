
#libraries
library(ggplot2)
library(lubridate)
library(dplyr)

#folder path
setwd("/Users/sri/OneDrive - East Carolina University/Spring2021/CSCI6010FinalProject")

#read csv file
data<-read.csv("Data/Covid19USA_revised1.csv",header=TRUE)

#Visualization
head(data)

str(data)
#convert date from factor to date
data1<-subset(data, !(state %in% c('Alaska','Hawaii')))
data1$date<-ymd(data1$date)

data1<-
  data1 %>% group_by(date) %>%
  summarize(deaths=sum(deaths_per_day,na.rm=TRUE),confirmed=sum(confirmed_per_day,na.rm=TRUE))

str(data1$date)
head(data1$date)

data1$date<-ymd(data1$date)
str(data1$date)

#visualize deaths
deaths_ts<-data1$deaths
plot.ts(deaths_ts, main="Cumulative daily deaths", ylab="Deaths")

traindata<-head(data1,386)
testdata<-tail(data1,28)

deaths_ts_train<-traindata["deaths"]
deaths_ts_test<-testdata["deaths"]



####################################arima model
#load the forecasting package
library(fpp2)
#declare time series object
library(zoo)
Y<-zoo(traindata$deaths, seq(from = as.Date("2020-01-21"), to = as.Date("2021-02-09"), by = 1))
###############################
# Preliminary analysis#Time plot
###############################

autoplot(Y)+ggtitle("Time Plot:Covid 19 Deaths per day")+
  ylab("Count")

###Data has some trne. Investigate transformation
#Take the first difference to remove the trend
DY<-diff(Y)

#Time plot of differenced data
autoplot(DY)+ggtitle("Time Plot:Covid 19 Deaths per day")+
  ylab("Count")
#Now series appears trend stationary, use to investigate seasonality: error
ggseasonplot(DY)+
  ggtitle("Seasonal Plot:Covid 19 Deaths per day")+
  ylab("Count")

#####################################################
#Snaive method#residual SD=490
######################################################

fit<-snaive(Y) 
print(summary(fit))
checkresiduals(fit)
#########################################################
#ETS method#residual SD=461
##########################################################

fit_ets<-ets(Y) 
print(summary(fit_ets))
checkresiduals(fit_ets)


#############################################################
#Fit arima model ####Residual SD=400
##############################################################
fit_arima<-auto.arima(Y,stepwise=FALSE,approximation=FALSE,trace=TRUE) 
print(summary(fit_arima))
checkresiduals(fit_arima)

#############################################################
#Forecast with arima model 
##############################################################
fcst<-forecast(fit_arima,h=28)
autoplot(fcst)
print(summary(fcst))


#############################################################
#Compare actual deaths with forecasted deaths
##############################################################
testdata1<-testdata
testdata1$date<-ymd(testdata1$date)
testdata1$predicted_deaths<-fcst$mean

library(scales)



#############################################################
#one day ahead forecast: simple model 1
##############################################################
deaths_ar1 <- arima(Y , order = c(1, 0, 0),include.mean = FALSE)
deaths_ar1_forecast <- predict(deaths_ar1, n.ahead = 28)$pred
plot(deaths_ar1_forecast)


#############################################################
# 7 day moving avearge#https://rpubs.com/JSHAH/481706
##############################################################
deaths_ma7 <- arima(Y, order = c(0,0,7))
lines(deaths_ma7,col="red")
deaths_ma7_forecast <- predict(deaths_ma7, n.ahead=28)$pred
plot(deaths_ma7_forecast)
testdata1$predicted_deaths_1day<-deaths_ar1_forecast
testdata1$predicted_deaths_ma7<- deaths_ma7_forecast 


testdata1<-
  testdata1 %>% 
  mutate(se=(deaths-predicted_deaths)^2,ss=(deaths - mean(predicted_deaths)) ^ 2,
         se1=(deaths-predicted_deaths_1day)^2,ss1=(deaths-mean(predicted_deaths_1day,na.rm=TRUE)) ^ 2,
         se2=(deaths-predicted_deaths_ma7)^2,ss2=(deaths-mean(predicted_deaths_ma7,na.rm=TRUE)) ^ 2)




testdata1_stat<-
  testdata1 %>% summarize(mse=sum(se,na.rm=TRUE),tss=sum(ss,na.rm=TRUE),r2=(1-mse/tss),
                               mse1=sum(se1,na.rm=TRUE),tss1=sum(ss1,na.rm=TRUE),r21=(1-mse1/tss1),
                               mse2=sum(se2,na.rm=TRUE),tss2=sum(ss2,na.rm=TRUE),r22=(1-mse2/tss2))



#write.csv(testdata1_stat,"testdata1_stat.csv",row.names=FALSE)




g2<-ggplot(data=testdata1,aes(x=date))+
  geom_point(aes(y=deaths,color="blue"))+
  geom_line(aes(y=deaths,color="blue"))+
  geom_point(aes(y=predicted_deaths,color="red"))+
  geom_line(aes(y=predicted_deaths,color="red"))+
  geom_point(aes(y=predicted_deaths_1day,color="orange"))+
  geom_line(aes(y=predicted_deaths_1day,color="orange"))+
  geom_point(aes(y=predicted_deaths_ma7,color="purple"))+
  geom_line(aes(y=predicted_deaths_ma7,color="purple"))+
  xlab("")+ylab("Daily deaths")+
  scale_color_manual(name="",values=c("blue"="black","red"="red","orange"="orange","purple"="purple"),labels=c("blue"="Actual","red"="ARIMA (3,1,2)","orange"="One day lag","purple"="7 days average"))+
  ggtitle("b) Model Evaluation using Test dataset")+
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("week"),limits = c(min(testdata1$date), max = max(testdata1$date)),expand=c(0,0))+
  theme_classic()+
  theme(legend.position=c(0.5,0.9),plot.title = element_text(hjust = 0.5))

g2
#ggsave("graphs/time_series_arima_model_testing_april162021.png")

#############################################################
#training data
##############################################################
traindata1<-traindata
traindata1$fitted_deaths<-fit_arima$fitted
traindata1$fitted_deaths_1daylag<- traindata$deaths - deaths_ar1$residuals
traindata1$fitted_deaths_7dayma<-traindata$deaths - deaths_ma7$residuals


traindata1<-
  traindata1 %>% 
  mutate(se=(deaths-fitted_deaths)^2,ss=(deaths-mean(fitted_deaths,na.rm=TRUE)) ^ 2,
         se1=(deaths-fitted_deaths_1daylag)^2,ss1=(deaths-mean(fitted_deaths_1daylag,na.rm=TRUE)) ^ 2,
         se2=(deaths-fitted_deaths_7dayma)^2,ss2=(deaths-mean(fitted_deaths_7dayma,na.rm=TRUE)) ^ 2)




  traindata1_stat<-
  traindata1 %>% summarize(mse=sum(se,na.rm=TRUE),tss=sum(ss,na.rm=TRUE),r2=(1-mse0/tss0),
                           mse1=sum(se1,na.rm=TRUE),tss1=sum(ss1,na.rm=TRUE),r21=(1-mse1/tss1),
                           mse2=sum(se2,na.rm=TRUE),tss2=sum(ss2,na.rm=TRUE),r22=(1-mse2/tss2))


  #write.csv(traindata1_stat,"traindata1_stat.csv",row.names=FALSE)


traindata1$date<-ymd(traindata1$date)

g1<-ggplot(data=traindata1,aes(x=date))+
  geom_point(aes(y=deaths,color="blue"))+
  geom_line(aes(y=deaths,color="blue"))+
  geom_point(aes(y=fitted_deaths,color="red"))+
  geom_line(aes(y=fitted_deaths,color="red"))+
  geom_point(aes(y=fitted_deaths_1daylag,color="orange"))+
  geom_line(aes(y=fitted_deaths_1daylag,color="orange"))+
  geom_point(aes(y=fitted_deaths_7dayma,color="purple"))+
  geom_line(aes(y=fitted_deaths_7dayma,color="purple"))+
  xlab("")+ylab("Daily deaths")+
  scale_color_manual(name="",values=c("blue"="black","red"="red","orange"="orange","purple"="purple"),labels=c("blue"="Actual","red"="ARIMA (3,1,2)","orange"="One day lag","purple"="7 days average"))+
  ggtitle("a) Model Building using Training dataset")+
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("month"))+
  theme_classic()+
  theme(legend.position=c(0.5,0.8),plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45))






#############################################################
#one day ahead foreacst #### combined
##############################################################
library(ggpubr)
ggarrange(g1,g2,
          #labels = c("a) Cumulative Confirmed", "b) Cumulative Deaths", "c) Daily Confirmed","d) Daily Deaths"),
          ncol = 1,nrow = 2)

#ggsave("graphs/time_series_arima_model_forecast_april162021.png")



#############################################################
#train data each model
##############################################################

library(reshape2)
traindata1_long <- melt(traindata1,
                  id.vars=c("date", "deaths"),
                  measure.vars=c("fitted_deaths", "fitted_deaths_1daylag", "fitted_deaths_7dayma" ))
  

traindata1_long$variable<-factor(traindata1_long$variable,labels=c("fitted_deaths"="ARIMA (3,1,2)", "fitted_deaths_1daylag"="lag one day", "fitted_deaths_7dayma"="7 day avearge" ))
g11<-ggplot(data=traindata1_long,aes(x=date))+
  geom_line(aes(y=deaths,color="blue"))+
  geom_line(aes(y=value,color="red"))+
  xlab("")+ylab("Daily deaths")+
  facet_wrap(~variable,nrow=3)+
  scale_color_manual(name="",values=c("blue"="black","red"="red"),labels=c("blue"="Actual deaths","red"="Fitted deaths"))+
  ggtitle("a) Model Building using Training dataset")+
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("month"))+
  theme(strip.background = element_blank())+
  theme_bw()+
  theme(legend.position="top")+
  theme(axis.text.x = element_text(angle = 45))

g11
#ggsave("graphs/time_series_arima_model.png")

