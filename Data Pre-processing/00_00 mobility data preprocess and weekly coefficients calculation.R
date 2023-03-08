### china mobility study using Baidu LBS data in 2013-2014 L2 (2012 shapefile)
library(data.table) 
library(lubridate)
library(ggplot2)


#### identify the change patterns of mobility intensities in 2019 
#outflow
outflow <- read_excel('data/test data/mobility scale.xlsx',  sheet = 'move out',)
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

intra$dates <- ymd(intra$DATE)
intra$year<- year(intra$dates)
intra$week<-week(intra$dates)
intra19 <- intra[intra$year==2019,]
intra19<-intra19[is.na(intra19$intraflow)==F,]

inflow$dates<-ymd(inflow$DATE)
inflow$year<- year(inflow$dates)
inflow$week<-week(inflow$dates)
inflow19<- inflow[inflow$year==2019,]
inflow19<-inflow19[is.na(inflow19$inflow)==F,]

outflow$dates<-ymd(outflow$DATE)
outflow$year<- year(outflow$dates)
outflow$week<-week(outflow$dates)
outflow19<- outflow[outflow$year==2019,]
outflow19<-outflow19[is.na(outflow19$outflow)==F,]

# daily changes of mobility intensities
intra19d <- intra19%>%
  group_by(dates)%>%
  summarize(mean=median(intraflow),upper=quantile(intraflow, p = 0.75) ,lower=quantile(intraflow, p = 0.25) )
intra19d$group <- 'intraflow'

inflow19d <- inflow19%>%
  group_by(dates)%>%
  summarize(mean=median(inflow),upper=quantile(inflow, p = 0.75) ,lower=quantile(inflow, p = 0.25) )
inflow19d$group <- 'inflow'

outflow19d <- outflow19%>%
  group_by(dates)%>%
  summarize(mean=median(outflow),upper=quantile(outflow, p = 0.75) ,lower=quantile(outflow, p = 0.25) )
outflow19d$group <- 'outflow'

daily <- rbind(intra19d, inflow19d, outflow19d)

fig1<-ggplot(daily, aes(x = dates,group=group)) +
  geom_ribbon(aes(ymin =lower,ymax = upper, fill=group), alpha = 0.3) +
  geom_line(aes(y = mean, color=group))+
  geom_vline(aes(xintercept=as.Date('2019-02-04')), colour="#990000", size=0.4)+
  geom_vline(aes(xintercept=as.Date('2019-01-21')), colour="black", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2019-02-10')), colour="#990000", size=0.4)+
  geom_vline(aes(xintercept=as.Date('2019-03-01')), colour="black", linetype="dashed", size=0.3)+
  labs(title='a Daily mobility intensity in 2019') +
  scale_y_continuous(expand = c(0,0),name=expression('Mobility intensity'),limits = c(0,6))+
  theme(panel.background = element_blank(), legend.position="bottom",legend.title=element_blank(),axis.line=element_line(),
        axis.text.x= element_text(angle=60, hjust=1))+guides(fill=guide_legend(reverse=TRUE))

ggsave(fig1, filename = "figin/fig_S01_daily mobility intensity in 2019.pdf", height = 8, width = 8, units = "cm")

# weekly changes of mobility intensities
intra19w <- intra19%>%
  group_by(week)%>%
  summarize(mean=median(intraflow),upper=quantile(intraflow, p = 0.75) ,lower=quantile(intraflow, p = 0.25) )
intra19w$group <- 'intraflow'

inflow19w <- inflow19%>%
  group_by(week)%>%
  summarize(mean=median(inflow),upper=quantile(inflow, p = 0.75) ,lower=quantile(inflow, p = 0.25) )
inflow19w$group <- 'inflow'

outflow19w <- outflow19%>%
  group_by(week)%>%
  summarize(mean=median(outflow),upper=quantile(outflow, p = 0.75) ,lower=quantile(outflow, p = 0.25) )
outflow19w$group <- 'outflow'

week <- rbind(intra19w, inflow19w, outflow19w)

fig2<-ggplot(week, aes(x = week,group=group)) +
  geom_ribbon(aes(ymin =lower,ymax = upper, fill=group), alpha = 0.3) +
  geom_line(aes(y = mean, color=group))+
  labs(title='b Weekly mobility intensity in 2019') +
  scale_y_continuous(expand = c(0,0),name=expression('Mobility intensity'),limits = c(0,6))+
  theme(panel.background = element_blank(), legend.position="bottom",legend.title=element_blank(),axis.line=element_line(),
        axis.text.x= element_text(angle=60, hjust=1))+guides(fill=guide_legend(reverse=TRUE))
ggsave(fig2, filename = "figin/fig_S01_weekly mobility intensity in 2019.pdf", height = 8, width = 8, units = "cm")



LBS <- fread('data/test data/BD_LBS_L2_netflow_daily.csv')
head(LBS)
min(LBS$date)
max(LBS$date)
LBS$year <-year(LBS$date)
LBS$week <-week(LBS$date)
LBS$day <-yday(LBS$date)
LBS$month <-month(LBS$date)

LBS$inflow <- log10(LBS$inflow)
LBS$outflow <- log10(LBS$outflow)

# mapping the daily inflow/outflow change to find if there exists abnormal data
lbs <- LBS[is.na(LBS$inflow)==F,]
inflow <- lbs%>%
  group_by(date)%>%
  summarize(median=median(inflow),upper=quantile(inflow, p = 0.75) ,lower=quantile(inflow, p = 0.25) )
inflow$group <- 'inflow'

lbs <- LBS[is.na(LBS$outflow)==F,]
outflow <- lbs%>%
  group_by(date)%>%
  summarize(median=median(outflow),upper=quantile(outflow, p = 0.75) ,lower=quantile(outflow, p = 0.25) )
outflow$group <- 'outflow'

daily1314 <- rbind(inflow, outflow)

##mapping the temporal mobility change and labelled the public holidays
par1<-ggplot(daily1314, aes(x = date,group=group)) +
  geom_ribbon(aes(ymin = lower,ymax = upper, fill=group), alpha = 0.3) +
  geom_line(aes(y = median, color=group))+
  geom_vline(aes(xintercept=as.Date('2014-01-31')), colour="#990000", size=0.4)+
  geom_vline(aes(xintercept=as.Date('2014-02-06')), colour="#990000", size=0.4)+
  geom_vline(aes(xintercept=as.Date('2014-01-06')), colour="black", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2014-02-24')), colour="black", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2013-10-01')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2013-10-07')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2013-04-04')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2013-04-06')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2013-04-29')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2013-05-01')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2013-06-10')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2013-06-12')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2013-09-19')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2013-09-21')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2014-01-01')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2014-04-05')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2014-04-07')), colour="grey40", linetype="dashed", size=0.3)+
  labs(x='day',title='2013-2014') +
  scale_y_continuous(expand = c(0,0),name=expression('inflow')
                     )+
  theme(panel.background = element_blank(), legend.position="bottom",legend.title=element_blank(),axis.line=element_line(),
        axis.text.x= element_text(angle=60, hjust=1))+
  guides(fill=guide_legend(reverse=TRUE))

par1


# pre-process the data
LBS$inflow[LBS$date=='2013-08-02'] <- (LBS$inflow[LBS$date=='2013-08-01'] +LBS$inflow[LBS$date=='2013-08-03'])/2
LBS$outflow[LBS$date=='2013-08-02'] <- (LBS$outflow[LBS$date=='2013-08-01'] +LBS$outflow[LBS$date=='2013-08-03'])/2

LBS$outflow[LBS$date=='2014-04-03'] <- (LBS$outflow[LBS$date=='2014-04-02'] +LBS$outflow[LBS$date=='2014-04-04'])/2
LBS$outflow[LBS$date=='2014-04-03'] <- (LBS$outflow[LBS$date=='2014-04-02'] +LBS$outflow[LBS$date=='2014-04-04'])/2

# map daily and weekly mobility intensities in 2013-2014
lbs <- LBS[is.na(LBS$inflow)==F,]
inflow <- lbs%>%
  group_by(date)%>%
  summarize(median=median(inflow),upper=quantile(inflow, p = 0.75) ,lower=quantile(inflow, p = 0.25) )
inflow$group <- 'inflow'

lbs <- LBS[is.na(LBS$outflow)==F,]
outflow <- lbs%>%
  group_by(date)%>%
  summarize(median=median(outflow),upper=quantile(outflow, p = 0.75) ,lower=quantile(outflow, p = 0.25) )
outflow$group <- 'outflow'

daily1314 <- rbind(inflow, outflow)

par1<-ggplot(daily1314, aes(x = date,group=group)) +
  geom_ribbon(aes(ymin = lower,ymax = upper, fill=group), alpha = 0.3) +
  geom_line(aes(y = median, color=group))+
  geom_vline(aes(xintercept=as.Date('2014-01-31')), colour="#990000", size=0.4)+
  geom_vline(aes(xintercept=as.Date('2014-02-06')), colour="#990000", size=0.4)+
  geom_vline(aes(xintercept=as.Date('2014-01-06')), colour="black", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2014-02-24')), colour="black", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2013-10-01')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2013-10-07')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2013-04-04')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2013-04-06')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2013-04-29')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2013-05-01')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2013-06-10')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2013-06-12')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2013-09-19')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2013-09-21')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2014-01-01')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2014-04-05')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2014-04-07')), colour="grey40", linetype="dashed", size=0.3)+
  labs(title='a Daily intercity mobility intensity in 2013-2014') +
  scale_y_continuous(expand = c(0,0),name=expression('Mobility intensity'), limits=c(4.5,6.0))+
  theme(panel.background = element_blank(), legend.position="bottom",legend.title=element_blank(),axis.line=element_line(),
        axis.text.x= element_text(angle=60, hjust=1))+
  guides(fill=guide_legend(reverse=TRUE))

ggsave(par1, filename = "figin/fig_S02_daily intercity mobility intensity in 2013-2014.pdf", height = 8, width = 8, units = "cm")


lbs <- LBS[is.na(LBS$inflow)==F,]
inweek <- lbs%>%
  group_by(year,week)%>%
  summarize(median=median(inflow),upper=quantile(inflow, p = 0.75) ,lower=quantile(inflow, p = 0.25))
inweek$group <- 'infloweek'
inweek$week <- inweek$week+(inweek$year-2013)*52

lbs <- LBS[is.na(LBS$outflow)==F,]
outweek <- lbs%>%
  group_by(year,week)%>%
  summarize(median=median(outflow),upper=quantile(outflow, p = 0.75) ,lower=quantile(outflow, p = 0.25))
outweek$group <- 'outfloweek'
outweek$week <- outweek$week+(outweek$year-2013)*52

week1314 <-rbind(inweek, outweek)

par2<-ggplot(week1314, aes(x = week,group=group)) +
  geom_ribbon(aes(ymin = lower,ymax = upper, fill=group), alpha = 0.3) +
  geom_line(aes(y = median, color=group))+
  labs(title='b Weekly intercity mobility intensity in 2013-2014') +
  scale_y_continuous(expand = c(0,0),name=expression('Mobility intensity'), limits = c(4.5,6.0))+
  theme(panel.background = element_blank(), legend.position="bottom",legend.title=element_blank(),axis.line=element_line(),
        axis.text.x= element_text(angle=60, hjust=1))+
  guides(fill=guide_legend(reverse=TRUE))

ggsave(par2, filename = "figin/fig_S02_weekly intercity mobility intensity in 2013-2014.pdf", height = 8, width = 8, units = "cm")

#including the annual change of intercity mobility to tranform the mobility data

lbs13 <-LBS[LBS$date >= as.Date('2013-04-23') & LBS$date <= as.Date('2013-04-29'),]
lbs14 <-LBS[LBS$date >= as.Date('2014-04-23') & LBS$date <= as.Date('2014-04-29'),]
lbs13base<-lbs13%>%
  group_by(ZONECODE)%>%
  summarize(in13=mean(inflow) ,out13=mean(outflow) )
lbs14base<-lbs14%>%
  group_by(ZONECODE)%>%
  summarize(in14=mean(inflow) ,out14=mean(outflow) )
lbs13_14 <- merge(lbs13base,lbs14base,by='ZONECODE',na.rm = T)


#calculate the mobility base in 2014
lbsbase <- LBS[(LBS$date >= as.Date('2014-01-09')& LBS$date <= as.Date('2014-01-15')),]
lbsbase<-lbsbase%>%
  group_by(ZONECODE)%>%
  summarize(inbase=mean(inflow),outbase=mean(outflow))

####calculate the weekly variation coefficient of intercity mobility 
lbsnew <- LBS
lbsnew2<-lbsnew%>%
  group_by(ZONECODE,year,week)%>%
  summarize(inflow=mean(inflow),outflow=mean(outflow))

lbsnew2 <-merge(lbsnew2, lbs13_14, by='ZONECODE', all.x=T)
lbsnew2 <-merge(lbsnew2, lbsbase, by='ZONECODE', all.x=T)

lbsnew2$incor <- lbsnew2$inflow+ (2014-lbsnew2$year)*(lbsnew2$in14-lbsnew2$in13)
lbsnew2$outcor <- lbsnew2$outflow+ (2014-lbsnew2$year)*(lbsnew2$out14-lbsnew2$out13)


del1 <- which((lbsnew2$year==2013)&(lbsnew2$week==17))
lbsnew2<-lbsnew2[-del1,]

del2 <- which((lbsnew2$year==2014)&(lbsnew2$week==18))
lbsnew2<-lbsnew2[-del2,]

# total number of weeks
nweek <- length(unique(lbsnew2$week))
# total number of cities
ncity <- length(unique(lbsnew2$city_code))

x<- rep(1:ncity, ntime)

lbsnew2$cofin <- lbsnew2$incor/lbsnew2$inbase
lbsnew2$cofout <- lbsnew2$outcor/lbsnew2$outbase

code <- read_excel('data/test data/matched code.xls',sheet='matched code')
code <- code[,c('ZONECODE','city','city_code')]
xnew <- merge(code,lbsnew2,by='ZONECODE',na.rm = T)

fwrite(xnew, 'data/test data/weekly coefficient cities.csv', row.names = F)

