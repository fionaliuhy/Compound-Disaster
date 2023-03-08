rm(list=ls())

## data collation
packages <- c("data.table", "tidyverse", "sf", "sp", "spdep",
              "geofacet", "ggpubr", "ggthemes", 'readxl', 'lubridate')
lapply(packages, library, character.only = TRUE)


# load the intracity mobility intensity recovery data
dataintra<- read.csv("intra recovery/data/test data/data_all14.csv", header = T)
dataintra <- dataintra[dataintra$year==2021,]
data1<-dataintra[is.na(dataintra$intrarecovery)==F,]
data1 <- data1%>%
  group_by(dates)%>%
  summarize(mean=mean(intrarecovery),upper=quantile(intrarecovery, p = 0.75) ,lower=quantile(intrarecovery, p = 0.25) )
data1$group <- 'intrarecovery'


# load the inflow/outflow intensity recovery data
datainter<- read.csv("inter recovery/data/test data/data_all_in&out.csv", header = T)
datainter <- datainter[datainter$year.x==2021,]
datainter <- datainter[is.na(datainter$time)==F,]
data2<-datainter[is.na(datainter$rein)==F,]
data2 <- data2%>%
  group_by(dates)%>%
  summarize(mean=mean(rein),upper=quantile(rein, p = 0.75) ,lower=quantile(rein, p = 0.25) )
data2$group <- 'inflowrecovery'

data3<-datainter[is.na(datainter$reout)==F,]
data3 <- data3%>%
  group_by(dates)%>%
  summarize(mean=mean(reout),upper=quantile(reout, p = 0.75) ,lower=quantile(reout, p = 0.25) )
data3$group <- 'outflowrecovery'

data0 <- rbind(data1,data2,data3)

# plot the mobility intensity recovery trajectories of three types of movement
data0$mean <- as.numeric(data0$mean)
data0$upper <-as.numeric(data0$upper)
data0$lower <-as.numeric(data0$lower)
data0$dates <-ymd(data0$dates)
data0$year <- year(data0$dates)
data0<- data0[data0$year>2020,]

fwrite(data0, 'data0.csv', row.names = F)

fig<-ggplot(data0, aes(x = dates,group=group)) +
  geom_ribbon(aes(ymin = lower,ymax = upper, fill=group), alpha = 0.3) +
  geom_line(aes(y = mean, color=group))+
  # plot the Chunyun period in 2021
  geom_vline(aes(xintercept=as.Date('2021-01-28')), colour="black", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-03-08')), colour="black", linetype="dashed", size=0.3)+
  # plot the spring festival holiday in 2021
  geom_vline(aes(xintercept=as.Date('2021-02-11')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-02-17')), colour="grey40", linetype="dashed", size=0.3)+
  # plot the national day in 2021 and one day before and after it
  geom_vline(aes(xintercept=as.Date('2021-09-30')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-10-08')), colour="grey40", linetype="dashed", size=0.3)+
  # plot the Qingming in 2021 and one day before and after it 
  geom_vline(aes(xintercept=as.Date('2021-04-02')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-06')), colour="grey40", linetype="dashed", size=0.3)+
  # plot the Labor day in 2021 and one day before and after it           
  geom_vline(aes(xintercept=as.Date('2021-04-30')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-05-06')), colour="grey40", linetype="dashed", size=0.3)+
  # plot the Duanwu in 2021 and one day before and after it    
  geom_vline(aes(xintercept=as.Date('2021-06-11')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-06-15')), colour="grey40", linetype="dashed", size=0.3)+
  # plot the Zhongqiu in 2021 and one day before and after it   
  geom_vline(aes(xintercept=as.Date('2021-09-18')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-22')), colour="grey40", linetype="dashed", size=0.3)+

  labs(x="date",title='mobility intensity recovery trajectories') +
  scale_y_continuous(expand = c(0,0),name=expression('mobility recovery degree'),limits = c(0.2,3.5))+
  # scale_x_discrete(expand = c(0,0),name=expression('date'),breaks = c(20210101,20211230,7))+
  theme(panel.background = element_blank(), legend.position="bottom",legend.title=element_blank(),axis.line=element_line(),
        axis.text.x= element_text(angle=60, hjust=1))+
  guides(fill=guide_legend(reverse=TRUE))

fig

# map the number of daily new cases in 2021 
case <- dataintra[,c("dates","cases")]
case<-  case%>%
  group_by(dates)%>%
  summarize(cases=sum(cases))
case$dates <-ymd(case$dates)
fig2<-ggplot(case, aes(x = dates)) +
  geom_col(aes(y = cases))+
  theme(panel.background = element_blank(), legend.position="bottom",legend.title=element_blank(),axis.line=element_line(),
        axis.text.x= element_text(angle=60, hjust=1))
fig2

# map the mobility recovery trajectories and daily new cases for Beijing
beijingra <- dataintra[dataintra$city_code==110000,]
beijingra0 <- beijingra[,c("dates","intrarecovery")]
beijingter <-datainter[datainter$city_code==110000,]
beijingter <- beijingter[,c("dates","rein","reout")]
beijing <-merge(beijingra0,beijingter,by='dates',all=TRUE)
beijing0<- reshape2::melt(beijing, id = c('dates'), value.name = 'recovery degree')
beijing0$group <-beijing0$variable
beijing0$dates<-ymd(beijing0$dates)
fig3<-ggplot(beijing0, aes(x = dates,group=group)) +
  geom_line(aes(y = beijing0$`recovery degree`, color=group))+
  geom_vline(aes(xintercept=as.Date('2021-01-28')), colour="black", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-03-08')), colour="black", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-02-11')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-02-17')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-30')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-10-08')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-02')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-06')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-30')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-05-06')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-06-11')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-06-15')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-18')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-22')), colour="grey40", linetype="dashed", size=0.3)+
  labs(x="date",title='beijing') +
  scale_y_continuous(expand = c(0,0),name=expression('mobility recovery degree'),limits = c(0.0,3.5))+
  theme(panel.background = element_blank(), legend.position="bottom",legend.title=element_blank(),axis.line=element_line(),
        axis.text.x= element_text(angle=60, hjust=1))+
  guides(fill=guide_legend(reverse=TRUE))

fig3

beijingra$dates <-ymd(beijingra$dates)
fig4<-ggplot(beijingra, aes(x = dates)) +
  geom_col(aes(y = cases))+
  scale_y_continuous(expand = c(0,0),limits = c(0,30))+
  theme(panel.background = element_blank(), legend.position="bottom",legend.title=element_blank(),axis.line=element_line(),
        axis.text.x= element_text(angle=60, hjust=1))
fig4


# map the mobility recovery trajectories and daily new cases for Zhengzhou
zhengzhoura <- dataintra[dataintra$city_code==410100,]
zhengzhoura0 <- zhengzhoura[,c("dates","intrarecovery")]
zhengzhouter <-datainter[datainter$city_code==410100,]
zhengzhouter <- zhengzhouter[,c("dates","rein","reout")]
zhengzhou <-merge(zhengzhoura0,zhengzhouter,by='dates',all=TRUE)
zhengzhou0<- reshape2::melt(zhengzhou, id = c('dates'), value.name = 'recovery degree')
zhengzhou0$group <-zhengzhou0$variable
zhengzhou0$dates<-ymd(zhengzhou0$dates)
fig5<-ggplot(zhengzhou0, aes(x = dates,group=group)) +
  geom_line(aes(y = zhengzhou0$`recovery degree`, color=group))+
  geom_vline(aes(xintercept=as.Date('2021-01-28')), colour="black", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-03-08')), colour="black", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-02-11')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-02-17')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-30')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-10-08')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-02')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-06')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-30')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-05-06')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-06-11')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-06-15')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-18')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-22')), colour="grey40", linetype="dashed", size=0.3)+
  labs(x="date",title='zhengzhou') +
  scale_y_continuous(expand = c(0,0),name=expression('mobility recovery degree'),limits = c(0.0,3.5))+
  theme(panel.background = element_blank(), legend.position="bottom",legend.title=element_blank(),axis.line=element_line(),
        axis.text.x= element_text(angle=60, hjust=1))+
  guides(fill=guide_legend(reverse=TRUE))

fig5

zhengzhoura$dates <-ymd(zhengzhoura$dates)
fig6<-ggplot(zhengzhoura, aes(x = dates)) +
  geom_col(aes(y = cases))+
  scale_y_continuous(expand = c(0,0),limits = c(0,30))+
  theme(panel.background = element_blank(), legend.position="bottom",legend.title=element_blank(),axis.line=element_line(),
        axis.text.x= element_text(angle=60, hjust=1))
fig6

# map the mobility recovery trajectories and daily new cases for Shenzhen
shenzhenra <- dataintra[dataintra$city_code==440300,]
shenzhenra0 <- shenzhenra[,c("dates","intrarecovery")]
shenzhenter <-datainter[datainter$city_code==440300,]
shenzhenter <- shenzhenter[,c("dates","rein","reout")]
shenzhen <-merge(shenzhenra0,shenzhenter,by='dates',all=TRUE)
shenzhen0<- reshape2::melt(shenzhen, id = c('dates'), value.name = 'recovery degree')
shenzhen0$group <-shenzhen0$variable
shenzhen0$dates<-ymd(shenzhen0$dates)
fig7<-ggplot(shenzhen0, aes(x = dates,group=group)) +
  geom_line(aes(y = shenzhen0$`recovery degree`, color=group))+
  geom_vline(aes(xintercept=as.Date('2021-01-28')), colour="black", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-03-08')), colour="black", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-02-11')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-02-17')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-30')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-10-08')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-02')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-06')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-30')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-05-06')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-06-11')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-06-15')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-18')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-22')), colour="grey40", linetype="dashed", size=0.3)+
  labs(x="date",title='shenzhen') +
  scale_y_continuous(expand = c(0,0),name=expression('mobility recovery degree'),limits = c(0.0,3.5))+
  theme(panel.background = element_blank(), legend.position="bottom",legend.title=element_blank(),axis.line=element_line(),
        axis.text.x= element_text(angle=60, hjust=1))+
  guides(fill=guide_legend(reverse=TRUE))

fig7

shenzhenra$dates <-ymd(shenzhenra$dates)
fig8<-ggplot(shenzhenra, aes(x = dates)) +
  geom_col(aes(y = cases))+
  scale_y_continuous(expand = c(0,0),limits = c(0,30))+
  theme(panel.background = element_blank(), legend.position="bottom",legend.title=element_blank(),axis.line=element_line(),
        axis.text.x= element_text(angle=60, hjust=1))
fig8


# map the mobility recovery trajectories and daily new cases for Harbin
harbinra <- dataintra[dataintra$city_code==230100,]
harbinra0 <- harbinra[,c("dates","intrarecovery")]
harbinter <-datainter[datainter$city_code==230100,]
harbinter <- harbinter[,c("dates","rein","reout")]
harbin <-merge(harbinra0,harbinter,by='dates',all=TRUE)
harbin0<- reshape2::melt(harbin, id = c('dates'), value.name = 'recovery degree')
harbin0$group <-harbin0$variable
harbin0$dates<-ymd(harbin0$dates)
fig9<-ggplot(harbin0, aes(x = dates,group=group)) +
  geom_line(aes(y = harbin0$`recovery degree`, color=group))+
  geom_vline(aes(xintercept=as.Date('2021-01-28')), colour="black", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-03-08')), colour="black", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-02-11')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-02-17')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-30')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-10-08')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-02')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-06')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-30')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-05-06')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-06-11')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-06-15')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-18')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-22')), colour="grey40", linetype="dashed", size=0.3)+
  labs(x="date",title='harbin') +
  scale_y_continuous(expand = c(0,0),name=expression('mobility recovery degree'),limits = c(0.0,3.5))+
  theme(panel.background = element_blank(), legend.position="bottom",legend.title=element_blank(),axis.line=element_line(),
        axis.text.x= element_text(angle=60, hjust=1))+
  guides(fill=guide_legend(reverse=TRUE))

fig9

harbinra$dates <-ymd(harbinra$dates)
fig10<-ggplot(harbinra, aes(x = dates)) +
  geom_col(aes(y = cases))+
  scale_y_continuous(expand = c(0,0),limits = c(0,30))+
  theme(panel.background = element_blank(), legend.position="bottom",legend.title=element_blank(),axis.line=element_line(),
        axis.text.x= element_text(angle=60, hjust=1))
fig10


# # map the mobility recovery trajectories and daily new cases for Chengdu
chengdura <- dataintra[dataintra$city_code==230100,]
chengdura0 <- chengdura[,c("dates","intrarecovery")]
chengduter <-datainter[datainter$city_code==230100,]
chengduter <- chengduter[,c("dates","rein","reout")]
chengdu <-merge(chengdura0,chengduter,by='dates',all=TRUE)
chengdu0<- reshape2::melt(chengdu, id = c('dates'), value.name = 'recovery degree')
chengdu0$group <-chengdu0$variable
chengdu0$dates<-ymd(chengdu0$dates)
fig11<-ggplot(chengdu0, aes(x = dates,group=group)) +
  geom_line(aes(y = chengdu0$`recovery degree`, color=group))+
  geom_vline(aes(xintercept=as.Date('2021-01-28')), colour="black", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-03-08')), colour="black", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-02-11')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-02-17')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-30')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-10-08')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-02')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-06')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-30')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-05-06')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-06-11')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-06-15')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-18')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-22')), colour="grey40", linetype="dashed", size=0.3)+
  labs(x="date",title='chengdu') +
  scale_y_continuous(expand = c(0,0),name=expression('mobility recovery degree'),limits = c(0.0,3.5))+
  theme(panel.background = element_blank(), legend.position="bottom",legend.title=element_blank(),axis.line=element_line(),
        axis.text.x= element_text(angle=60, hjust=1))+
  guides(fill=guide_legend(reverse=TRUE))

fig11

chengdura$dates <-ymd(chengdura$dates)
fig12<-ggplot(chengdura, aes(x = dates)) +
  geom_col(aes(y = cases))+
  scale_y_continuous(expand = c(0,0),limits = c(0,30))+
  theme(panel.background = element_blank(), legend.position="bottom",legend.title=element_blank(),axis.line=element_line(),
        axis.text.x= element_text(angle=60, hjust=1))
fig12

# map the mobility recovery trajectories and daily new cases for Xi'an
xianra <- dataintra[dataintra$city_code==610100,]
xianra0 <- xianra[,c("dates","intrarecovery")]
xianter <-datainter[datainter$city_code==610100,]
xianter <- xianter[,c("dates","rein","reout")]
xian <-merge(xianra0,xianter,by='dates',all=TRUE)
xian0<- reshape2::melt(xian, id = c('dates'), value.name = 'recovery degree')
xian0$group <-xian0$variable
xian0$dates<-ymd(xian0$dates)
fig13<-ggplot(xian0, aes(x = dates,group=group)) +
  geom_line(aes(y = xian0$`recovery degree`, color=group))+
  geom_vline(aes(xintercept=as.Date('2021-01-28')), colour="black", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-03-08')), colour="black", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-02-11')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-02-17')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-30')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-10-08')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-02')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-06')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-30')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-05-06')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-06-11')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-06-15')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-18')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-22')), colour="grey40", linetype="dashed", size=0.3)+
  labs(x="date",title='xian') +
  scale_y_continuous(expand = c(0,0),name=expression('mobility recovery degree'),limits = c(0.0,3.5))+
  theme(panel.background = element_blank(), legend.position="bottom",legend.title=element_blank(),axis.line=element_line(),
        axis.text.x= element_text(angle=60, hjust=1))+
  guides(fill=guide_legend(reverse=TRUE))

fig13

xianra$dates <-ymd(xianra$dates)
fig14<-ggplot(xianra, aes(x = dates)) +
  geom_col(aes(y = cases))+
  scale_y_continuous(expand = c(0,0),limits = c(0,300))+
  theme(panel.background = element_blank(), legend.position="bottom",legend.title=element_blank(),axis.line=element_line(),
        axis.text.x= element_text(angle=60, hjust=1))
fig14

# map the mobility recovery trajectories and daily new cases for Nanjing
nanjingra <- dataintra[dataintra$city_code==320100,]
nanjingra0 <- nanjingra[,c("dates","intrarecovery")]
nanjingter <-datainter[datainter$city_code==320100,]
nanjingter <- nanjingter[,c("dates","rein","reout")]
nanjing <-merge(nanjingra0,nanjingter,by='dates',all=TRUE)
nanjing0<- reshape2::melt(nanjing, id = c('dates'), value.name = 'recovery degree')
nanjing0$group <-nanjing0$variable
nanjing0$dates<-ymd(nanjing0$dates)
fig15<-ggplot(nanjing0, aes(x = dates,group=group)) +
  geom_line(aes(y = nanjing0$`recovery degree`, color=group))+
  geom_vline(aes(xintercept=as.Date('2021-01-28')), colour="black", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-03-08')), colour="black", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-02-11')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-02-17')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-30')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-10-08')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-02')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-06')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-04-30')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-05-06')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-06-11')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-06-15')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-18')), colour="grey40", linetype="dashed", size=0.3)+
  geom_vline(aes(xintercept=as.Date('2021-09-22')), colour="grey40", linetype="dashed", size=0.3)+
  labs(x="date",title='nanjing') +
  scale_y_continuous(expand = c(0,0),name=expression('mobility recovery degree'),limits = c(0.0,3.5))+
  theme(panel.background = element_blank(), legend.position="bottom",legend.title=element_blank(),axis.line=element_line(),
        axis.text.x= element_text(angle=60, hjust=1))+
  guides(fill=guide_legend(reverse=TRUE))

fig15

nanjingra$dates <-ymd(nanjingra$dates)
fig16<-ggplot(nanjingra, aes(x = dates)) +
  geom_col(aes(y = cases))+
  scale_y_continuous(expand = c(0,0),limits = c(0,300))+
  theme(panel.background = element_blank(), legend.position="bottom",legend.title=element_blank(),axis.line=element_line(),
        axis.text.x= element_text(angle=60, hjust=1))
fig16