### R script to explore exposure-lag-response relationships

source("00_load_packages_data.R")
# install.packages("viridis")
library(viridis)
library(ggside)
library(ggplot2)
library(gghalves)
library(ggalt)
## load best fitting model but no interactions
# Tempa + Preca  + incidence + policy DLNM
load("output21/model_1.1.RData")
base<-model

##### incidence ----

# Coefficient and covariance
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

#\ find position of the terms associated with incidence crossbasis
indt <- grep("basis_cases2", model$names.fixed)

# set value
x1 <- min(data$case2[is.na(data$case2)==F&data$case2>0])
cen_c1 <- round(x1,0)

# extract predictions from the ContainmentHealthIndex DLNM centred on no new cases
predt <- crosspred(basis_cases2, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag =1, cen = cen_c1) 

# lag response for different Tmin scenarios (Main text Fig 3b)
pdf("fig21/fig 3a.pdf", width = 6, height = 6)

# get exposures values
vars <- exp(predt$predvar)/100
vars<-vars[vars>0.03]
vars<-round(vars,2)

vars2<-max(vars)
# obtain suppression risk (SR) fit and upper and lower confidence limits for all exposure variables
SR <- 1-predt$matRRfit
SR.uci <- 1-predt$matRRlow
SR.lci <- 1-predt$matRRhigh

SR<-SR[7:42,]
SR.uci<-SR.uci[7:42,]
SR.lci<-SR.lci[7:42,]
# set SR range 


# define colours
co <-colorRampPalette(c('sienna1','grey80','grey50','grey30'))(22)

# define x values (lag, by lag)

plot(vars, SR[,1], col = co[1], type = "l", lwd = 1.5, 
     xlab = "Incidence", ylab = "SR", main = "", 
     ylim = range(0, 0.10), xlim=range(0.03, vars2),frame.plot = T, axes = F)
axis(1,at=vars,labels = vars)
axis(2)
xx <- c(vars, rev(vars))
yy <- c(SR.lci[,1], rev(SR.uci[,1]))
lines(vars, SR[,2], col = co[2], type = "l", lwd = 1.5)
lines(vars, SR[,3], col = co[3], type = "l", lwd = 1.5)
lines(vars, SR[,4], col = co[4], type = "l", lwd = 1.5)
lines(vars, SR[,5], col = co[5], type = "l", lwd = 1.5)
lines(vars, SR[,6], col = co[6], type = "l", lwd = 1.5)
lines(vars, SR[,7], col = co[7], type = "l", lwd = 1.5)
lines(vars, SR[,8], col = co[8], type = "l", lwd = 1.5)
lines(vars, SR[,9], col = co[9], type = "l", lwd = 1.5)
lines(vars, SR[,10], col = co[10], type = "l", lwd = 1.5)
lines(vars, SR[,11], col = co[11], type = "l", lwd = 1.5)
lines(vars, SR[,12], col = co[12], type = "l", lwd = 1.5)
lines(vars, SR[,13], col = co[13], type = "l", lwd = 1.5)
lines(vars, SR[,14], col = co[14], type = "l", lwd = 1.5)
lines(vars, SR[,15], col = co[15], type = "l", lwd = 1.5)
lines(vars, SR[,16], col = co[16], type = "l", lwd = 1.5)
lines(vars, SR[,17], col = co[17], type = "l", lwd = 1.5)
lines(vars, SR[,18], col = co[18], type = "l", lwd = 1.5)
lines(vars, SR[,19], col = co[19], type = "l", lwd = 1.5)
lines(vars, SR[,20], col = co[20], type = "l", lwd = 1.5)
lines(vars, SR[,21], col = co[21], type = "l", lwd = 1.5)
lines(vars, SR[,22], col = co[22], type = "l", lwd = 1.5)

mtext(side = 2, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()

max(SR)
max(SR.uci)
max(SR.lci)


data$inci<-100*data$case/data$pop_2019
data$inci<-as.numeric(data$inci)
data$Stringency<-as.numeric(data$Stringency)
data0<-data[data$inci>0,]

col<- c('blue', 'orange')
incipolicy<-ggplot(data0,aes(inci,Stringency))+
  geom_point(alpha=0.6,color='orchid')+
  # theme_classic(base_size = 25)+
  scale_x_continuous(limits = c(0,45),expand = c(0,0),
                     breaks = seq(0,45,10))+
  scale_y_continuous(limits = c(20,90),expand = c(0,0),
                     breaks = seq(20,90,10))+
  geom_half_violin(aes(inci,Stringency),fill=col[1],alpha = 0.3,side ='r',cex=0.3)+ 
  # geom_half_violin(aes(Stringency,inci),fill=col[2],alpha = 0.3,side = 'r',cex=0.3)+ 
  xlab("Incidence (per million people)")+
  ylab("Policy Stringency Index")+
  theme_bw() 
  
incipolicy

ggsave(incipolicy, filename = "fig21/incipolicy2021.pdf")
