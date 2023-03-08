packages <- c("data.table", "tidyverse", "sf", "sp", "spdep",
              "geofacet", "ggpubr", "ggthemes", 'readxl', 'lubridate')
lapply(packages, library, character.only = TRUE)

prec90 <- fread("1990dailypri.csv", header = T)
prec91 <- fread("1991dailypri.csv", header = T)
prec92 <- fread("1992dailypri.csv", header = T)
prec93 <- fread("1993dailypri.csv", header = T)
prec94 <- fread("1994dailypri.csv", header = T)
prec95 <- fread("1995dailypri.csv", header = T)
prec96 <- fread("1996dailypri.csv", header = T)
prec97 <- fread("1997dailypri.csv", header = T)
prec98 <- fread("1998dailypri.csv", header = T)
prec99 <- fread("1999dailypri.csv", header = T)
prec00 <- fread("2000dailypri.csv", header = T)
prec01 <- fread("2001dailypri.csv", header = T)
prec02 <- fread("2002dailypri.csv", header = T)
prec03 <- fread("2003dailypri.csv", header = T)
prec04 <- fread("2004dailypri.csv", header = T)
prec05 <- fread("2005dailypri.csv", header = T)
prec06 <- fread("2006dailypri.csv", header = T)
prec07 <- fread("2007dailypri.csv", header = T)
prec08 <- fread("2008dailypri.csv", header = T)
prec09 <- fread("2009dailypri.csv", header = T)
prec10 <- fread("2010dailypri.csv", header = T)
prec11 <- fread("2011dailypri.csv", header = T)
prec12 <- fread("2012dailypri.csv", header = T)
prec13 <- fread("2013dailypri.csv", header = T)
prec14 <- fread("2014dailypri.csv", header = T)
prec15 <- fread("2015dailypri.csv", header = T)
prec16 <- fread("2016dailypri.csv", header = T)
prec17 <- fread("2017dailypri.csv", header = T)
prec18 <- fread("2018dailypri.csv", header = T)
prec19 <- fread("2019dailypri.csv", header = T)


prec <- rbind(prec90, prec91, prec92, prec93,prec94, prec95, prec96, prec97,prec98, prec99,
              prec00, prec01, prec02, prec03,prec04, prec05, prec06, prec07,prec08, prec09,
              prec10, prec11, prec12, prec13,prec14, prec15, prec16, prec17,prec18, prec19)
colnames(prec) <- c('id','city_code', 'city_name', 'DATE', 'PREC')
prec<- prec[,c('city_code', 'DATE', 'PREC')]
prec$PREC <- prec$PREC*25.4 # inch to millimetre
head(prec)
summary(prec$PREC)

library(lubridate)
prec$date <-ymd(prec$DATE)
prec$time<-as.character(prec$date,format='%m%d') 

prec2 <- prec%>%
  group_by(city_code,time)%>%
  summarize(mean=mean(PREC),sd=sd(PREC))
summary(prec2)
prec2$time<-as.character(prec2$time,format='%m%d') 
fwrite(prec2, 'prec-daily average(normal&sd).csv', row.names = F)

precbase<-prec[prec$DATE>20130429&prec$DATE<20140430,]
precbase$base<-precbase$PREC


prec20 <- fread("2020dailypri.csv", header = T)
colnames(prec20) <- c('id','city_code', 'city_name', 'DATE', 'PREC')
prec20<- prec20[,c('city_code', 'DATE', 'PREC')]
prec20$PREC <- prec20$PREC*25.4

prec21 <- fread("2021dailypri.csv", header = T)
colnames(prec21) <- c('id','city_code', 'city_name', 'DATE', 'PREC')
prec21<- prec21[,c('city_code', 'DATE', 'PREC')]
prec21 <- prec21[prec21$DATE != 20210211,]
cities <- unique(prec21$city_code)
prec21$PREC <- prec21$PREC*25.4

# compare precipitation data 
prec0210 <- read_excel("20210210.xls")
prec0210$DATE <- 20210210
prec0213 <- read_excel("20210213.xls")
prec0213$DATE <- 20210213
prec0219 <- read_excel("20210219.xls")
prec0219$DATE <- 20210219
prec0221 <- read_excel("20210221.xls")
prec0221$DATE <- 20210221
preccom <- rbind(prec0210, prec0213,prec0219,prec0221)
preccom<- preccom[,c('city_code',  'DATE','MEAN')]
colnames(preccom)<- c('city_code',  'DATE','PREC')
preccom2<- merge(preccom,prec21,by=c('city_code','DATE'))
ggscatter(preccom2,x='PREC.x', y='PREC.y',add = 'reg.line',conf.int = TRUE)+
  stat_cor(method = 'pearson',label.x=1.5,label.y = -10)


prec0211 <- read_excel("20210211.xls")
prec0211$DATE <- 20210211
prec0212 <- read_excel("20210212.xls")
prec0212$DATE <- 20210212
prec0220 <- read_excel("20210220.xls")
prec0220$DATE <- 20210220
precnew <- rbind(prec0211, prec0212,prec0220)
precnew<- precnew[,c('city_code',  'DATE','MEAN')]
colnames(precnew)<- c('city_code',  'DATE','PREC')

prec21 <- rbind(prec21,precnew)


prec22 <- fread("2022dailypri.csv", header = T)
colnames(prec22) <- c('id','city_code', 'city_name', 'DATE', 'PREC')
prec22<- prec22[,c('city_code', 'DATE', 'PREC')]
prec22$PREC <- prec22$PREC*25.4

prec <- rbind(prec20, prec21, prec22)
prec$date <-ymd(prec$DATE)
prec$time<-as.character(prec$date,format='%m%d') 
prec$city_code <- as.integer(prec$city_code)
prec <- merge(prec, prec2, by=c('city_code', 'time'), all.x=T)
prec$anomaly <- prec$PREC-prec$mean
prec$anomaly[is.na(prec$anomaly)==T]<-0
summary(prec$anomaly)
prec$anomalys <- prec$anomaly/prec$sd
prec$anomalys[is.na(prec$anomalys)==T]<-0
summary(prec$anomalys)

precbase<-precbase[,c('city_code', 'time', 'base')]
prec<-merge(prec,precbase, by=c('city_code', 'time'), all.x=T)
prec$vari <- prec$PREC-prec$base
summary(prec$vari)
fwrite(prec, 'precanomaly-daily 20-22.csv', row.names = F)


ggplot(prec,aes(anomalys))+
  geom_histogram(bins=30,aes(x=anomalys,y=..density..),fill="lightblue",color="black")+
  geom_density(color="black")+
  labs(x="Value",y="Density",,title="anomalys")+theme_bw()


temp90 <- fread("1990dailytemp.csv", header = T)
temp91 <- fread("1991dailytemp.csv", header = T)
temp92 <- fread("1992dailytemp.csv", header = T)
temp93 <- fread("1993dailytemp.csv", header = T)
temp94 <- fread("1994dailytemp.csv", header = T)
temp95 <- fread("1995dailytemp.csv", header = T)
temp96 <- fread("1996dailytemp.csv", header = T)
temp97 <- fread("1997dailytemp.csv", header = T)
temp98 <- fread("1998dailytemp.csv", header = T)
temp99 <- fread("1999dailytemp.csv", header = T)
temp00 <- fread("2000dailytemp.csv", header = T)
temp01 <- fread("2001dailytemp.csv", header = T)
temp02 <- fread("2002dailytemp.csv", header = T)
temp03 <- fread("2003dailytemp.csv", header = T)
temp04 <- fread("2004dailytemp.csv", header = T)
temp05 <- fread("2005dailytemp.csv", header = T)
temp06 <- fread("2006dailytemp.csv", header = T)
temp07 <- fread("2007dailytemp.csv", header = T)
temp08 <- fread("2008dailytemp.csv", header = T)
temp09 <- fread("2009dailytemp.csv", header = T)
temp10 <- fread("2010dailytemp.csv", header = T)
temp11 <- fread("2011dailytemp.csv", header = T)
temp12 <- fread("2012dailytemp.csv", header = T)
temp13 <- fread("2013dailytemp.csv", header = T)
temp14 <- fread("2014dailytemp.csv", header = T)
temp15 <- fread("2015dailytemp.csv", header = T)
temp16 <- fread("2016dailytemp.csv", header = T)
temp17 <- fread("2017dailytemp.csv", header = T)
temp18 <- fread("2018dailytemp.csv", header = T)
temp19 <- fread("2019dailytemp.csv", header = T)

temp <- rbind(temp90, temp91, temp92, temp93, temp94, temp95, temp96, temp97,temp98, temp99,
              temp00, temp01, temp02, temp03, temp04, temp05, temp06, temp07,temp08, temp09,
              temp10, temp11, temp12, temp13, temp14, temp15, temp16, temp17,temp18, temp19)
colnames(temp) <- c('id','city_code', 'city_name', 'DATE', 'TEMP')
temp <- temp[,c('city_code', 'DATE', 'TEMP')]

temp$TEMP <- (temp$TEMP-32)*5/9
head(temp)
temp$city_code <- as.integer(temp$city_code)
summary(temp$TEMP)

library(lubridate)
temp$date <-ymd(temp$DATE)
temp$time<-as.character(temp$date,format='%m%d') 


tempbase<-temp[temp$DATE>20130429&temp$DATE<20140430,]
tempbase$base<-tempbase$TEMP
tempbase<-tempbase[,c('city_code', 'time', 'base')]

temp2 <- temp%>%
  group_by(city_code,time)%>%
  summarize(mean=mean(TEMP),sd=sd(TEMP))
fwrite(temp2, 'temp-daily average(normal&sd).csv', row.names = F)

temp20 <- fread("2020dailytemp.csv", header = T)
colnames(temp20) <- c('id','city_code', 'city_name', 'DATE', 'TEMP')
temp20<- temp20[,c('city_code', 'DATE', 'TEMP')]
temp20$TEMP <- (temp20$TEMP-32)*5/9

temp21 <- fread("2021dailytemp.csv", header = T)
colnames(temp21) <- c('id','city_code', 'city_name', 'DATE', 'TEMP')
temp21<- temp21[,c('city_code', 'DATE', 'TEMP')]
temp21 <- temp21[temp21$DATE != 20210211,]
cities <- unique(temp21$city_code)
temp21$TEMP <- (temp21$TEMP-32)*5/9


# compare temperature data with ERA-5
tem0210 <- read_excel("0210tem.xls")
tem0210$DATE <- 20210210
tem0213 <- read_excel("0213tem.xls")
tem0213$DATE <- 20210213
tem0219 <- read_excel("0219tem.xls")
tem0219$DATE <- 20210219
tem0221 <- read_excel("0221tem.xls")
tem0221$DATE <- 20210221
temcom <- rbind(tem0210, tem0213,tem0219,tem0221)
temcom<- temcom[,c('city_code', 'DATE', 'MEAN')]
colnames(temcom)<- c('city_code', 'DATE', 'TEMP')
temcom$TEMP <- temcom$TEMP-273.15
temcom2<- merge(temcom,temp21,by=c('city_code','DATE'))

ggscatter(temcom2,x='TEMP.x', y='TEMP.y',add = 'reg.line',conf.int = TRUE)+
  stat_cor(method = 'pearson',label.x=1.5,label.y = -10)

tem0211 <- read_excel("0211tem.xls")
tem0211$DATE <- 20210211
tem0212 <- read_excel("0212tem.xls")
tem0212$DATE <- 20210212
tem0220 <- read_excel("0220tem.xls")
tem0220$DATE <- 20210220
temnew <- rbind(tem0211, tem0212,tem0220)
temnew<- temnew[,c('city_code', 'DATE', 'MEAN')]
colnames(temnew)<- c('city_code', 'DATE', 'TEMP')
temnew$TEMP <- temnew$TEMP-273.15

temp21 <- rbind(temp21,temnew)


temp22 <- fread("2022dailytemp.csv", header = T)
colnames(temp22) <- c('id','city_code', 'city_name', 'DATE', 'TEMP')
temp22<- temp22[,c('city_code', 'DATE', 'TEMP')]
temp22$TEMP <- (temp22$TEMP-32)*5/9

temp3 <- rbind(temp20, temp21, temp22)

temp3$date <-ymd(temp3$DATE)
temp3$time<-as.character(temp3$date,format='%m%d') 
temp3$city_code <- as.integer(temp3$city_code)

temp3 <- merge(temp3, temp2, by=c('city_code', 'time'), all.x=T)

temp3$anomaly <- temp3$TEMP-temp3$mean
summary(temp3$anomaly)
temp3$anomalys <- temp3$anomaly/temp3$sd
temp3$anomalys[is.na(temp3$anomalys)==T]<-0
summary(temp3$anomalys)


tmax20 <- fread("2020dailytmax.csv", header = T)
colnames(tmax20) <- c('id','city_code', 'city_name', 'DATE', 'Tmax')
tmax20 <- tmax20[,c('city_code', 'DATE', 'Tmax')]
tmax20$Tmax <- (tmax20$Tmax-32)*5/9

tmax21 <- fread("2021dailytmax.csv", header = T)
colnames(tmax21) <- c('id','city_code', 'city_name', 'DATE', 'Tmax')
tmax21 <- tmax21[,c('city_code', 'DATE', 'Tmax')]
cities <- unique(tmax21$city_code)
tmax21$Tmax <- (tmax21$Tmax-32)*5/9
summary(tmax21)

tmax0211 <- read_excel("0211max.xls")
tmax0211$DATE <- 20210211
tmax0212 <- read_excel("0212max.xls")
tmax0212$DATE <- 20210212
tmax0220 <- read_excel("0220max.xls")
tmax0220$DATE <- 20210220
tmaxnew <- rbind(tmax0211, tmax0212,tmax0220)
colnames(tmaxnew) <- c('id','city_code', 'zone_code','Count', 'area','Tmax','DATE')
tmaxnew<- tmaxnew[,c('city_code', 'DATE', 'Tmax')]
tmaxnew$city_code <- as.numeric(tmaxnew$city_code)
tmaxnew$Tmax <- tmaxnew$Tmax-273.15


tmax22 <- fread("2022dailytmax.csv", header = T)
colnames(tmax22) <- c('id','city_code', 'city_name', 'DATE', 'Tmax')
tmax22 <- tmax22[,c('city_code', 'DATE', 'Tmax')]
cities <- unique(tmax22$city_code)
tmax22$Tmax <- (tmax22$Tmax-32)*5/9
summary(tmax22)

tmax <- rbind(tmax20,tmax21,tmaxnew,tmax22)

temp <- merge(temp3, tmax, by=c('city_code', 'DATE'), all.x=T)

# tmin20 <- fread("2020dailytmin.csv", header = T)
# colnames(tmin20) <- c('id','city_code', 'city_name', 'DATE', 'Tmin')
# tmin20<- tmin20[,c('city_code', 'DATE', 'Tmin')]
# cities <- unique(tmin20$city_code)
# tmin20$Tmin <- (tmin20$Tmin-32)*5/9
# 
# tmin21 <- fread("2021dailytmin.csv", header = T)
# colnames(tmin21) <- c('id','city_code', 'city_name', 'DATE', 'Tmin')
# tmin21<- tmin21[,c('city_code', 'DATE', 'Tmin')]
# cities <- unique(tmin21$city_code)
# tmin21$Tmin <- (tmin21$Tmin-32)*5/9
# 
# tmin0211 <- read_excel("0211min.xls")
# tmin0211$DATE <- 20210211
# tmin0212 <- read_excel("0212min.xls")
# tmin0212$DATE <- 20210212
# tmin0220 <- read_excel("0220min.xls")
# tmin0220$DATE <- 20210220
# tminnew <- rbind(tmin0211, tmin0212,tmin0220)
# colnames(tminnew) <- c('id','city_code', 'zone_code','Count', 'area','Tmin','DATE')
# tminnew<- tminnew[,c('city_code', 'DATE', 'Tmin')]
# tminnew$city_code <- as.numeric(tminnew$city_code)
# tminnew$Tmin <- tminnew$Tmin-273.15
# 
# tmin <- rbind(tmin20, tmin21,tminnew)
# 
# temp <- merge(temp, tmin, by=c('city_code', 'DATE'), all.x=T)

temp<-merge(temp, tempbase, by=c('city_code', 'time'), all.x=T)
temp$vari<-temp$TEMP-temp$base

fwrite(temp, 'tempanomaly-daily 20-22.csv', row.names = F)

tempo <- fread("tempanomaly-daily 20-22o.csv", header = T)
plot(temp3$anomalys,tempo$anomalys)

ggplot(temp3,aes(anomaly))+
  geom_histogram(bins=30,aes(x=anomalys,y=..density..),fill="lightblue",color="black")+
  geom_density(color="black")+
  labs(x="Value",y="Density",,title="anomalys")+theme_bw()
