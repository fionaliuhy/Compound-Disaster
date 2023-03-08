rm(list=ls())

## data collation
packages <- c("data.table", "tidyverse", "sf", "sp", "spdep",
              "geofacet", "ggpubr", "ggthemes", 'readxl', 'lubridate')
lapply(packages, library, character.only = TRUE)

map <- read_sf("data/city_cn/city_cn.shp")
sf::sf_use_s2(FALSE) # to fix 4 features with invalid spherical geometry
map$area <- st_area(map)/1000000 #km2

map$prov_code <- as.integer(substr(map$city_code,1,2))
prov_code <- unique(map$prov_code)
dat <- cbind(city_code=map$city_code, city=map$city, prov_code=map$prov_code, city_area=map$area)
dat[duplicated(dat[,c('city_code')])==T,]

## climate
prec <- fread("data/climate/precanomaly-daily 20-22.csv", header = T)
temp <- fread("data/climate/tempanomaly-daily 20-22.csv", header = T)


climate <- merge(temp, prec, by = c('city_code', 'DATE'))
climate<- climate[,c('city_code', 'DATE', 'TEMP','Tmax','anomaly.x','anomalys.x','vari.x','PREC','anomaly.y','anomalys.y','vari.y')]
colnames(climate) <- c('city_code', 'DATE', 'TEMP','Tmax','TEMPa','TEMPas',"tempv", 'PREC','PRECa','PRECas',"precv")
head(climate)
str(climate)
climate[duplicated(climate[,c('city_code', 'DATE')])==T,]
climate <- unique(climate)

climate <- climate[order(climate$DATE, decreasing = F),]
str(climate)
## case data
case1 <- read_excel('data/test data/case22.xlsx')
case1$case<-case1$新增无症状+case1$新增确诊-case1$新增无症状转确诊
case1<-case1[,c('省份','城市','日期',"case")]
colnames(case1)<-c('province','city','date','cases')
case11<-case1[case1$province==c('北京市')|case1$province==c('天津市')|case1$province==c('上海市')|case1$province==c('重庆市'),]
case1<-setdiff(case1,case11)

case11 <- case11%>%
  group_by(province,date)%>%
  summarize(cases=sum(cases))
case11$city<-case11$province

case1<-rbind(case1,case11)
case1 <- case1%>%
  group_by(province,city,date)%>%
  summarize(cases=sum(cases))

case1<-merge(case1,dat,by=c('city'),all.x=T)

case1$DATE<-ymd(case1$date)
case1$DATE<-as.character(case1$DATE,format='%Y%m%d')
case1$DATE<-as.integer(case1$DATE)

case1<-case1[,c('city_code','DATE','cases')]

case <- read_excel('data/test data/daily new cases.xlsx',  sheet = 'daily new cases' )

case$FID <- case$city <- NULL
case[duplicated(case$city_code)==T,] #340500

case <- reshape2::melt(case, id.vars = c('city_code'), value.name = 'cases')
colnames(case) <- c('city_code', 'DATE', 'cases')
case$city_code <- as.integer(case$city_code)
case$DATE <- as.character(case$DATE)
case$DATE <- as.integer(case$DATE)
case$cases <- as.integer(case$cases)
case<-case[case$DATE>20210202&case$DATE<20210318,]

case<-rbind(case1,case)

# outflow
outflow <- read_excel('data/test data/mobility scale.xlsx',  sheet = 'move out',)
# correct wrong city code
# outflow$`city code`[outflow$city == '温州市'] <- '330300'
outflow$city <- NULL
outflow[duplicated(outflow$`city_code`)==T,]

outflow <- reshape2::melt(outflow, id = c('city_code'), value.name = 'outflow')
colnames(outflow) <- c('city_code', 'DATE', 'outflow')
outflow$city_code <- as.integer(outflow$city_code)
outflow$DATE <- as.character(outflow$DATE)
outflow$DATE <- as.integer(outflow$DATE)
str(outflow)
outflow[duplicated(outflow[,c('city_code', 'DATE')])==T,]

# inflow
inflow <- read_excel('data/test data/mobility scale.xlsx',  sheet = 'move in',)
# correct wrong city code
# inflow$`city code`[inflow$city == '温州市'] <- '330300'
inflow$city <- NULL
inflow[duplicated(inflow$`city_code`)==T,]

inflow <- reshape2::melt(inflow, id = c('city_code'), value.name = 'inflow')
colnames(inflow) <- c('city_code', 'DATE', 'inflow')
inflow$city_code <- as.integer(inflow$city_code)
inflow$DATE <- as.character(inflow$DATE)
inflow$DATE <- as.integer(inflow$DATE)
str(inflow)

# intra-city mobility
intra <- read_excel('data/test data/mobility scale.xlsx',  sheet = 'intra-city mobility',)

intra$city <- NULL
intra[duplicated(intra$city_code)==T,]

intra <- reshape2::melt(intra, id = c('city_code'), value.name = 'intraflow')
colnames(intra) <- c('city_code', 'DATE', 'intraflow')
intra$city_code <- as.integer(intra$city_code)
intra$DATE <- as.character(intra$DATE)
intra$DATE <- as.integer(intra$DATE)
str(intra)

# merge
mobility <- merge(outflow, inflow, by=c('city_code', 'DATE'))
mobility <- merge(mobility, intra, by=c('city_code', 'DATE'))
mobility$in_outflow <- mobility$inflow + mobility$outflow
mobility$netflow <- mobility$inflow - mobility$outflow

## covid interventions
# Stringency index
stringency <- read_excel('data/test data/Policy index_province.xlsx',  sheet = 'StringencyIndex',)
stringency <- reshape2::melt(stringency, id = c('province','prov_code'), value.name = 'Stringency')
colnames(stringency) <- c('province','prov_code', 'DATE', 'Stringency')
stringency$prov_code <- as.integer(stringency$prov_code)
stringency$DATE <- as.character(stringency$DATE)
stringency$DATE <- as.integer(stringency$DATE)
str(stringency)

stringency22 <- fread("data/test data/OxCGRT_CHN_differentiated_withnotes_2022.csv", header = T)
stringency22<-stringency22[,c('RegionName','Date','StringencyIndex_NonVaccinated')]
colnames(stringency22)<- c('province', 'DATE', 'Stringency')
stringency22$DATE <- as.character(stringency22$DATE)
stringency22$DATE <- as.integer(stringency22$DATE)
prv<-unique(stringency[,c('province','prov_code')])
summary(stringency22)

stringency22<-merge(stringency22,prv,by=c('province'),all.x=T)

stringency<-rbind(stringency,stringency22)

## socioeconomic factors
pop <- read_excel('data/test data/Socio Economic Index.xls',  sheet = 'Socio Economic Index')

pop$pop_2019 <- pop$`permanently reside population (2019)`
pop$pop_2019 <- as.double(pop$pop_2019)

pop$gdp <- pop$`GDP(2019)`
pop$gdp <- as.numeric(pop$gdp)

pop$density <- pop$`urban population density(2019)`

pop$density <- as.numeric(pop$density)
pop <- pop[, c('city_code', 'pop_2019','density','gdp')]
pop$city_code <- as.integer(pop$city_code)

# remove cities with no data
pop <- pop[is.na(pop$density) ==F & is.na(pop$gdp) ==F,]
pop$city_code <- as.integer(pop$city_code)
pop[duplicated(pop[,c('city_code')])==T,]
pop <- unique(pop) # 340500 

pop <- merge(pop, dat[,c('city_code', 'prov_code', 'city_area')], by='city_code', all.x=T)
pop$city_area <- as.numeric(pop$city_area)
pop[duplicated(pop[,c('city_code')])==T,]
str(pop)
# festival and holiday
holiday <- read_excel('data/test data/festival and holiday2.xlsx',  sheet = 'Sheet1')
holiday$holiday <- 1
holiday$DATE <- as.integer(holiday$Date)
holiday$Note <- holiday$Date <- NULL 
str(holiday)


## merge
data <- merge(mobility, case, by=c('city_code', 'DATE'), all.x=T)
data$cases[is.na(data$cases)==T] <- 0

data[duplicated(data[,c('city_code', 'DATE')])==T,]
data <- merge(data, climate, by=c('city_code', 'DATE'), all.x=T)
data[duplicated(data[,c('city_code', 'DATE')])==T,]
data <- merge(data, pop, by='city_code')
data <- merge(data, stringency, by=c('prov_code', 'DATE'), all.x=T)
data$Stringency[is.na(data$Stringency)==T] <- 0
data[duplicated(data[,c('city_code', 'DATE')])==T,]
data <- merge(data, holiday, by='DATE', all.x=T)
data$holiday[is.na(data$holiday)==T] <- 0


data <- data.table(data)
str(data)

data1<- data
data1$dates<- ymd(data1$DATE)
data1$year<- year(data1$date)
data1$week<- week(data1$date)

base19 <- data1[((data1$dates>='2019-01-14')&(data1$dates<='2019-01-20')),]
base20 <- data1[((data1$dates>='2020-01-03')&(data1$dates<='2020-01-09')),]
interbase19 <- base19%>%
  group_by(city_code,year)%>%
  summarize(inout19=mean(in_outflow),in19=mean(inflow),out19=mean(outflow))
interbase20 <- base20%>%
  group_by(city_code,year)%>%
  summarize(inout20=mean(in_outflow),in20=mean(inflow),out20=mean(outflow))
plot(interbase19$in19,interbase20$in20)
plot(interbase19$out19,interbase20$out20)
interbase <- merge(interbase19,interbase20,by='city_code')


coeff <- read.csv('data/test data/weekly coefficient cities.csv')

coeff <-merge (coeff,interbase, by='city_code',all.x=T)


# read updated dates
DATE <- read.csv('data/test data/dates_update_14day_lag_2021.csv')
DATE$DATE <- ymd(DATE$DATE)
data$DATE<-ymd(data$DATE)
DATE <- DATE[DATE$DATE %in% data1$dates,] 

data <- merge(data, DATE, by='DATE')
# sort data
data <- data[order(data$DATE, data$city_code),]
length(unique(data$city_code))
str(data)
data$week<-data$ISOweek
data <- merge(data,coeff, by=c('city_code','week'),all.x=T)


data <- data[data$city_code != 330300,]

data$corrected20in <- data$in20*data$cofin
data$corrected20out <- data$out20*data$cofout

data$rein<- data$inflow.x/data$corrected20in
data$reout<- data$outflow.x/data$corrected20out

data$case2 <- log(10000*data$cases/data$pop_2019)
data$case2 [data$case2==-Inf] <- 0
data$gdp <- log(data$gdp)
data$density <-log(data$density)
data$gdp_pc_2019 <-log(data$gdp_pc_2019)


fwrite(data, 'data/test data/data_all_in&out 21.csv', row.names = F)

# 

# 
# library(Hmisc)
# data <- data[,c('rein','reout','case2','Stringency','TEMP','TEMPa','PREC','PRECa','holiday','gdp','density','pop_2019')]
# colnames(data) <- c('rein', 'reout','case2','Stringency','temp','TEMPa','prec','PRECa','holiday','gdp','density','pop_2019')
# dd= as.matrix(data)
# rcorr(dd)
# 
# library(car)
# fit <-lm(data$rein~data$TEMP+data$PREC+data$case2+data$Stringency+data$gdp+data$holiday+data$density)
# vif(fit)
# fit <-lm(data$reout~data$TEMPa+data$PRECa+data$case2+data$Stringency+data$gdp+data$holiday+data$density)
# vif(fit)
# 
