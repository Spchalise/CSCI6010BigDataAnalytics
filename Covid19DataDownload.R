#Reference:https://joss.theoj.org/papers/10.21105/joss.02376.pdf
#source R package: https://cloud.r-project.org/web/packages/COVID19/COVID19.pdf
##setwd("/Users/sri/Documents/CSCI6010/data/Projectdata")
#Working directory
setwd("/Users/sri/OneDrive - East Carolina University/Spring2021/CSCI6010Personal")
#install.packages("COVID19")
library(COVID19)
#covid19("USA")#this gives 413
data<-covid19("USA",level=3) #

write.csv(data,"Covid19USA.csv",row.names=FALSE)


#First need to read summary data file
data<-read.csv("Data/Covid19USA.csv",header=TRUE)

library(dplyr)
data1<-
  data %>% group_by(key_google_mobility) %>%
  mutate(confirmed_per_day=diff(c(0, confirmed)),deaths_per_day=diff(c(0, deaths))) %>%
  filter(!is.na(confirmed) & !is.na(deaths) & !is.na(latitude) & !is.na(longitude) & !is.na(date) & !is.na(key_google_mobility)) %>%
  mutate(confirmed_per_day = if_else(confirmed_per_day < 0, 0, confirmed_per_day),
         deaths_per_day = if_else(deaths_per_day < 0, 0, deaths_per_day)) %>%
  rename(country=administrative_area_level_1,state=administrative_area_level_2,county=administrative_area_level_3)


write.csv(data1,"Data/Covid19USA_revised1.csv",row.names=FALSE)
