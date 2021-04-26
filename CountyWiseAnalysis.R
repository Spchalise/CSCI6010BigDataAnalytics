##################################################################
###HERE WE WILL CREATE STATEWISE MODEL
##################################################################
#libraries
library(ggplot2)
library(lubridate)
library(dplyr)
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
  data1 %>% group_by(id,date) %>%
  summarize(deaths=sum(deaths_per_day,na.rm=TRUE),confirmed=sum(confirmed_per_day,na.rm=TRUE))


str(data1$date)
head(data1$date)

data1$date<-ymd(data1$date)
str(data1$date)

#visualize deaths
deaths_ts<-data1$deaths
plot.ts(deaths_ts, main="Cumulative daily deaths", ylab="Deaths")
##################################################################
#2929 model - model for each location# use all 100% data
##################################################################
ids<-unique(data1$id) 
all_stat<-data.frame()
for (i in 1:length(ids)){
  print(i)
  location_id<-as.character(ids[i])
  data2<-subset(data1,as.character(id)==location_id)
  zeros=nrow(subset(data2,deaths==0))
  nonzeros=nrow(data2)-zeros
  if (nonzeros!=0){
  traindata<-data2 
  deaths_ts_train<-traindata["deaths"]
  deaths_ts_test<-testdata["deaths"]
##################################################################
#fit the AR model to this data
################################################################## 

deaths_ar <- arima(deaths_ts_train, order = c(2, 0, 0),include.mean = FALSE)
#visualize output
deaths_ar
summary(deaths_ar)
residuals <- residuals(deaths_ar)
deaths_fitted <- deaths_ts_train - data.frame(residuals)
ts.plot(deaths_ts_train)
points(deaths_fitted, type = "l", col = 2, lty = 2)


traindata1<-traindata
traindata1$estimated_deaths<-deaths_fitted$deaths

traindata1<-
  traindata1 %>% 
  mutate(se=(deaths-estimated_deaths)^2,ss=(deaths-mean(estimated_deaths,na.rm=TRUE)) ^ 2)


mse0=sum(traindata1$se,na.rm=TRUE)
tss0=sum(traindata1$ss,na.rm=TRUE)
r20=(1-mse0/tss0)

summary_stat<-data.frame(id=location_id,ar1=deaths_ar$coef[1],ar2=deaths_ar$coef[2],mse=mse0,tss=tss0,r2=r20)

all_stat<-rbind(all_stat,summary_stat)

  }
  
}

#write.csv(all_stat,"output/all_stat_by_ids.csv",row.names=FALSE)
#all_stat<-read.csv("output/all_stat_by_ids.csv",header=TRUE)

id_latlong<-
  data %>% select (id,latitude,longitude) %>% distinct()
  
##################################################################
#combined with location
##################################################################
all_stat_location<-merge(all_stat,id_latlong,by="id")






##################################################################
##Map Library 
##################################################################
library(tibble)
library(dplyr)
library(lubridate)
library(TTR)
library(stringi)
library(lubridate)
library(ggplot2)
library(tictoc)
library(reshape2)
library(dataRetrieval)
library(ggmap)
library(rgdal)#for map
library(mapproj)


##################################################################
####US boundary#read usa boundary shapefile
##################################################################
usa_boundary<-readOGR("Data/shapefiles/lower48_states_outerboundary/lower48_states_outerboundary.shp", layer="lower48_states_outerboundary")
#Assign mercator co-ordinate system
usa_boundary <- spTransform(usa_boundary,CRS("+proj=longlat +datum=WGS84"))

#Content of Dataframe
data.frame(usa_boundary)
head(usa_boundary)

#Before plot, convert to dataframe
usa_boundarydata<-fortify(usa_boundary)


#read usa boundary shapefile
usa_boundary<-readOGR("Data/shapefiles/lower48_states_outerboundary/lower48_states_outerboundary.shp", layer="lower48_states_outerboundary")

#Assign mercator co-ordinate system
usa_boundary <- spTransform(usa_boundary,CRS("+proj=longlat +datum=WGS84"))

##################################################################
###Before plot, convert to dataframe
##################################################################

usa_boundarydata<-fortify(usa_boundary)
head(usa_boundarydata)


gpl<-all_stat_location
gpl$r2size<- ifelse(gpl$r2<0.1,"a", 
                          ifelse(gpl$r2>0.1 & gpl$r2<0.3,"b",
                                 ifelse(gpl$r2>=0.3 & gpl$r2<0.5,"c",
                                        ifelse(gpl$r2>0.5,"d",
                                               NA ))))

gpl$ar1size<- ifelse(gpl$ar1 <= 0.1,"a", 
                    ifelse(gpl$ar1>0.1 & gpl$ar1 <= 0.2,"b",
                           ifelse(gpl$ar1>0.2 & gpl$ar1 <0.3,"c",
                                  ifelse(gpl$ar1>0.3,"d",
                                                NA ))))



gg <- ggplot()


gg <- gg + geom_map(data=usa_boundarydata, map=usa_boundarydata, aes(map_id=id, x=long, y=lat), fill=NA, color="black",size=0.25)




gg<-gg+geom_point(aes(x = longitude, y = latitude,colour=r2size),size=1.25,data = gpl)+
  scale_colour_manual(name="Coefficient of Determination:r2",  
                      values = c("a"="grey","b"="orange","c"="blue","d"="green"),
                      labels = c("a"="Below 0.1", "b"="0.1 to 0.3", "c"="0.3 to 0.5", "d"="Above 0.5"))

gg1<-gg+coord_map(projection="albers", lat0=30, lat1=40)+ #oct 23: I used albers equal area conic projection
  #gg1<-gg+coord_map(projection = "mercator")+   #coord_fixed()
  theme_bw()+
  theme_void()+
  theme(legend.position = c(.9, 0.4),#May 30
        legend.key.size = unit(0.5, "cm"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=9),
        legend.background = element_rect(color = "white", 
                                         fill = "white", size = 0, linetype = "solid"))+
  guides(col = guide_legend(nrow = 5))+
  guides(colour = guide_legend(override.aes = list(size=2.5, alpha = 1)))



gg1

##ggsave("graphs/r2_comparison_across_each_location_map.png",dpi=300,width=8,height=6)




##################################################################
#######deaths
##################################################################

ggg <- ggplot()

ggg <- ggg + geom_map(data=usa_boundarydata, map=usa_boundarydata, aes(map_id=id, x=long, y=lat), fill=NA, color="black",size=0.25)
ggg<-ggg+geom_point(aes(x = longitude, y = latitude,fill=ar1size,shape=ar1size),size=1.25,data = gpl)+
   scale_fill_manual(name="ar1",
                      values = c("a"="grey","b"="blue","c"="green","d"="red"),
                      labels = c("a"="Below 0.1","b"="0.1 to 0.2", "c"="0.2 to 0.3","d"="Above 0.3"))+

    scale_shape_manual(name="ar1",
                      values = c("a"=21,"b"=22,"c"=23,"d"=24),
                      labels = c("a"="Below 0.1","b"="0.1 to 0.2", "c"="0.2 to 0.3","d"="Above 0.3"))
    
  
ggg1<-ggg+coord_map(projection="albers", lat0=30, lat1=40)+ 
  theme_bw()+
  theme_void()+
  theme(legend.position = c(.9, 0.4),#May 30
        legend.key.size = unit(0.5, "cm"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=9),
        legend.background = element_rect(color = "white", 
                                         fill = "white", size = 0, linetype = "solid"))+
  guides(col = guide_legend(nrow = 5))+
  guides(colour = guide_legend(override.aes = list(size=2.5, alpha = 1)))

ggg1

#ggsave("graphs/ar1_comparison_across_each_location_map.png",dpi=300,width=8,height=6)



