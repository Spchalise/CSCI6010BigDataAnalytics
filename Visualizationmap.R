
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
###Spatial Map
library(ggplot2)
library(reshape2)
library(rgdal)
library(mapproj)

#Working directory
setwd("/Users/sri/OneDrive - East Carolina University/Spring2021/CSCI6010Project")


######################################################################
#First need to read summary data file
#######################################################################
data0<-read.csv("Data/Covid19USA_revised1.csv",header=TRUE)

######################################################################
#let's create map of total confirmed cases and total deaths
#######################################################################

data<-subset(data0,date=="2021-03-09")

str(data)


######################################################################
#read usa boundary shapefile
#######################################################################
usa_boundary<-readOGR("Data/shapefiles/lower48_states_outerboundary/lower48_states_outerboundary.shp", layer="lower48_states_outerboundary")
#Assign mercator co-ordinate system
usa_boundary <- spTransform(usa_boundary,CRS("+proj=longlat +datum=WGS84"))

data.frame(usa_boundary)
head(usa_boundary)

#Before plot, convert to dataframe
usa_boundarydata<-fortify(usa_boundary)


######################################################################
#read usa boundary shapefile
#######################################################################
usa_boundary<-readOGR("Data/shapefiles/lower48_states_outerboundary/lower48_states_outerboundary.shp", layer="lower48_states_outerboundary")

#Assign mercator co-ordinate system
usa_boundary <- spTransform(usa_boundary,CRS("+proj=longlat +datum=WGS84"))



#Before plot, convert to dataframe
usa_boundarydata<-fortify(usa_boundary)
head(usa_boundarydata)
data1<-data[!is.na(data$latitude),]
data2<-data1[!is.na(data1$deaths),]
data3<-data2[!is.na(data2$confirmed),]

gpl <- subset(data2, !(state %in% c('Alaska','Hawaii')))
summary(gpl)
######################################################################
#Plotting Deaths
#######################################################################
gpl$psize<- ifelse(gpl$deaths<=0,"a", #gpl$deaths>0&gpl$deaths<5,"a",
                   ifelse(gpl$deaths>=0&gpl$deaths<100,"b", #gpl$deaths>0&gpl$deaths<5,"a",
                   ifelse(gpl$deaths>=100&gpl$deaths<1000,"c",
                          ifelse(gpl$deaths>=1000&gpl$deaths<10000,"d",
                                 ifelse(gpl$deaths>=10000,"e",
                                               NA )))))

####Plotting confirmed cases
gpl$psize1<- ifelse(gpl$confirmed<100,"a", 
                           ifelse(gpl$confirmed>=100&gpl$confirmed<1000,"b",
                                  ifelse(gpl$confirmed>=1000&gpl$confirmed<10000,"c",
                                         ifelse(gpl$confirmed>=10000&gpl$confirmed<100000,"d",
                                         ifelse(gpl$confirmed>=100000,"e",
                                                NA )))))


######################################################################
#CDeaths map
#######################################################################
gg <- ggplot()
gg <- gg + geom_map(data=usa_boundarydata, map=usa_boundarydata, aes(map_id=id, x=long, y=lat), fill=NA, color="black",size=0.25)
gg<-gg+geom_point(aes(x = longitude, y = latitude,colour=psize),size=1.25,data = gpl)+
  scale_colour_manual(name="Total Deaths",  
                      values = c("a"="green","b"="blue","c"="orange","d"="magenta","e"="red"),
                      labels = c("a"="Nil", "b"="Below 100", "c"="100 to 1000", "d"="1000 to 10000","e"="Above 10000"))+
  
  scale_size_manual(name="Total Deaths",
                    values = c("a"=0.1,"b"=0.2,"c"=0.3,"d"=0.5,"e"=1),
                               labels = c("a"="Nil", "b"="Below 100", "c"="100 to 1000", "d"="1000 to 10000","e"="Above 10000"))

gg1<-gg+coord_map(projection="albers", lat0=30, lat1=40)+
  theme_bw()+
  theme_void()+
 theme(legend.position = c(.9, 0.4),
      legend.key.size = unit(0.5, "cm"),
      legend.title=element_text(size=9),
      legend.text=element_text(size=9),
      legend.background = element_rect(color = "white", 
                                       fill = "white", size = 0, linetype = "solid"))+
  guides(col = guide_legend(nrow = 5))+
  guides(colour = guide_legend(override.aes = list(size=2.5, alpha = 1)))

gg1

######################################################################
#Confirmed map
#######################################################################
ggg <- ggplot()


ggg <- ggg + geom_map(data=usa_boundarydata, map=usa_boundarydata, aes(map_id=id, x=long, y=lat), fill=NA, color="black",size=0.25)



ggg<-ggg+geom_point(aes(x = longitude, y = latitude,colour=psize1),size=1.25,data = gpl)+
  scale_colour_manual(name="Total Confirmed  cases",  
                      values = c("a"="green","b"="blue","c"="orange","d"="magenta","e"="red"),
                      labels = c("a"="Below 100","b"="100 to 1000", "c"="1000 to 10000","d"="10000 to 100000","e"="Above 100000"))+
  
  scale_size_manual(name="Total Confirmed cases",
                    values = c("a"=0.1,"b"=0.2,"c"=0.3,"d"=0.5,"e"=1),
                    labels = c("a"="Below 100","b"="100 to 1000", "c"="1000 to 10000","d"="10000 to 100000","e"="Above 100000"))


ggg1<-ggg+coord_map(projection="albers", lat0=30, lat1=40)+
  theme_bw()+
  theme_void()+
  theme(legend.position = c(.9, 0.4),
        legend.key.size = unit(0.5, "cm"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=9),
        legend.background = element_rect(color = "white", 
                                         fill = "white", size = 0, linetype = "solid"))+
  #coord_fixed(xlim = c(-75, -110), ylim = c(25, 42.5))+
  guides(col = guide_legend(nrow = 5))+
  guides(colour = guide_legend(override.aes = list(size=2.5, alpha = 1)))

ggg1

####ggsave("graphs/confirmed_by_location.png",dpi=300,width=8,height=6)


  
  

