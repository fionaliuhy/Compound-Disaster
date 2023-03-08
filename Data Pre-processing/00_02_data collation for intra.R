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
climate<- climate[,c('city_code', 'DATE', 'TEMP','Tmax','anomaly.x','anomalys.x','PREC','anomaly.y','anomalys.y')]
colnames(climate) <- c('city_code', 'DATE', 'TEMP','Tmax','TEMPa','TEMPas', 'PREC','PRECa','PRECas')
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
# outflow$`city code`[outflow$city == '温州???'] <- '330300'
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
# inflow$`city code`[inflow$city == '温州???'] <- '330300'
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
mobility <- merge(intra,inflow, by=c('city_code', 'DATE'))
mobility <- merge(mobility, outflow,  by=c('city_code', 'DATE'))
mobility$in_outflow <- mobility$inflow + mobility$outflow
mobility$netflow <- mobility$inflow - mobility$outflow

###calculate the pre-pandemic baseline

base20<-mobility[mobility$DATE>20200101&mobility$DATE<20200123,]
intrabase20 <- base20%>%
  group_by(city_code)%>%
  summarize(intrabase20=mean(intraflow))

base19<-mobility[mobility$DATE>20190111&mobility$DATE<20190202,]
intrabase19 <- base19%>%
  group_by(city_code)%>%
  summarize(intrabase19=mean(intraflow))

base1920<-merge(intrabase20, intrabase19, by=c('city_code'), all.x=T)

base21<-mobility[mobility$DATE>20210106&mobility$DATE<20210128,]
base21<- merge(base21, case, by=c('city_code', 'DATE'), all.x=T)
base21<-base21[base21$cases==0,]
intrabase21 <- base21%>%
  group_by(city_code)%>%
  summarize(intrabase21=mean(intraflow))

base192021<-merge(base1920, intrabase21, by=c('city_code'), all.x=T)


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

pop$gdp_pc_2019 <- pop$`GDP per capita(2019)`
pop$gdp_pc_2019 <- as.numeric(pop$gdp_pc_2019)

pop$gdp <- pop$`GDP(2019)`
pop$gdp <- as.numeric(pop$gdp)

pop$urbanization_rate_2019 <- pop$`urbanization rate(2019)`
pop$pri_eco_2019 <- pop$`Contribution of primary industry(2019)`
pop$sec_eco_2019 <- pop$`Contribution of secondary industry(2019)`
pop$ter_eco_2019 <- pop$`Contribution of tertiary industry(2019)`
pop$density <- pop$`urban population density(2019)`

pop$density <- as.numeric(pop$density)
pop <- pop[, c('city_code', 'pop_2019', 'gdp_pc_2019','urbanization_rate_2019','pri_eco_2019','sec_eco_2019','ter_eco_2019','density','gdp')]
pop$city_code <- as.integer(pop$city_code)


# remove cities with no data
pop <- pop[is.na(pop$density) ==F & is.na(pop$urbanization_rate_2019) ==F& is.na(pop$gdp) ==F,]
pop$city_code <- as.integer(pop$city_code)
pop[duplicated(pop[,c('city_code')])==T,]
pop <- unique(pop) # 340500 

pop <- merge(pop, dat[,c('city_code', 'prov_code', 'city_area')], by='city_code', all.x=T)
pop$city_area <- as.numeric(pop$city_area)
pop[duplicated(pop[,c('city_code')])==T,]
str(pop)
# festival and holiday
holiday <- read_excel('data/test data/festival and holiday.xlsx',  sheet = 'Sheet1')
holiday$holiday <- 1
holiday$DATE <- as.integer(holiday$Date)
holiday$Note <- holiday$Date <- NULL 
str(holiday)




## merge
data <- merge(climate,mobility, by=c('city_code', 'DATE'), all.x=T)

data <- merge(data, base1920, by=c('city_code'), all.x=T)
data$city_code<-as.integer(data$city_code)
case$city_code<-as.integer(case$city_code)
data <- merge(data, case, by=c('city_code', 'DATE'), all.x=T)
data$cases[is.na(data$cases)==T] <- 0
data <- merge(data, pop, by='city_code')
data$prov_code<-as.integer(data$prov_code)
stringency$prov_code<-as.integer(stringency$prov_code)
data <- merge(data, stringency, by=c('prov_code', 'DATE'), all.x=T)

summary(data$Stringency)


data <- merge(data, holiday, by='DATE', all.x=T)
data$holiday[is.na(data$holiday)==T] <- 0

data[duplicated(data[,c('city_code', 'DATE')])==T,]

data <- data.table(data)
str(data)

# read updated dates
DATE <- read.csv('data/test data/dates_update_21day_lag_2021.csv')
DATE$DATE <- ymd(DATE$DATE)
data$DATE<-ymd(data$DATE)
DATE <- DATE[DATE$DATE %in% data$DATE,] 

data <- merge(data, DATE, by='DATE')
# sort data
data <- data[order(data$DATE, data$city_code),]
length(unique(data$city_code))
str(data)


data <- data[data$city_code != 330300,]
data$intrarecovery <- data$intraflow/data$intrabase20
data$case2 <- log(10000*data$cases/data$pop_2019)
data$case2 [data$case2==-Inf] <- 0
data$gdp <- log(data$gdp)
data$density <-log(data$density)
data$gdp_pc_2019 <-log(data$gdp_pc_2019)


fwrite(data, 'data/test data/data_all21.csv', row.names = F)

##### Collinearity analysis
#library(Hmisc)
#data <- data_all[,c('intrarecovery', 'case2','Stringency','TEMPa','PRECa','holiday','gdp')]
#colnames(data) <- c('intrarecovery', 'case2','Stringency','TEMPa','PRECa','holiday','gdp')
#dd= as.matrix(data)
#rcorr(dd)

#library(car)
#fit <-lm(data$intrarecovery~data$TEMPa+data$PRECa+data$case2+data$Stringency+data$gdp+data$holioday)
#vif(fit)
