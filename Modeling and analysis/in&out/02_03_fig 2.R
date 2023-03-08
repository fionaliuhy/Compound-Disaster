### R script to explore exposure-lag-response relationships

source("00_load_packages_data.R")
# install.packages("viridis")
library(viridis)
## load best fitting model but no interactions
load("output21/in/model_1.3.RData")
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

predt <- crosspred(basis_cases2, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag =1, cen = cen_c1) 

pdf("fig21/fig 2b.pdf", width = 6, height = 6)

# get exposures values
vars <- exp(predt$predvar)/100
vars<-round(vars,2)

# obtain suppression risk (SR) fit and upper and lower confidence limits for all exposure variables
SR <- 1-predt$matRRfit
SR.uci <- 1-predt$matRRlow
SR.lci <- 1-predt$matRRhigh

# set SR range 
s1 <- 0
s2 <- max(SR.uci)

# define colours
co <-colorRampPalette(c('sienna1','grey80'))(8)

# define x values (lag, by lag)
plot(vars, SR[,1], col = co[1], type = "l", lwd = 1.5, 
     xlab = "Incidence", ylab = "SR", main = "", 
     ylim = range(0, 0.20), frame.plot = T, axes = F)
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

mtext(side = 2, at = s2*1.25, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()

max(SR)
max(SR.uci)
max(SR.lci)


#####policy stringency index ----
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

#\ find position of the terms associated with basis_policy crossbasis
indt <- grep("basis_policy", model$names.fixed)

# set value
cen_c1 <- round(min(data$Stringency), 0)

predt <- crosspred(basis_policy, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 1, cen = cen_c1) 

pdf("fig21/fig 2e.pdf", width = 6, height = 6)

# get exposures values
vars <- predt$predvar

# obtain suppression risk (SR) fit and upper and lower confidence limits for all exposure variables
SR <- 1-predt$matRRfit
SR.uci <- 1-predt$matRRlow
SR.lci <- 1-predt$matRRhigh
SRall<-1-predt$allRRfit

max(SR.lci)

# set SR range 
s1 <- 0
s2 <- max(SR.uci)

# define colours
co <-colorRampPalette(c('blue','lightblue'))(7)

# define x values (lag, by lag)

plot(vars, SR[,1], col = co[1], type = "l", lwd = 1.5, 
     xlab = "policy stringency index", ylab = "SR", main = "", 
     ylim = range(0, 0.25), frame.plot = T, axes = F)
axis(1,at=vars,labels = vars)
axis(2)
xx <- c(vars, rev(vars))
yy <- c(SR.lci[,1], rev(SR.uci[,1]))
lines(vars, SR[,2], col = co[2], type = "l", lwd = 1.5)
lines(vars, SR[,3], col = co[3], type = "l",lwd = 1.5)
lines(vars, SR[,4], col = co[4], type = "l", lwd = 1.5)
lines(vars, SR[,5], col = co[5], type = "l", lwd = 1.5)
lines(vars, SR[,6], col = co[6], type = "l", lwd = 1.5)
lines(vars, SR[,7], col = co[7], type = "l", lwd = 1.5)
mtext(side = 2, at = s2*1.25, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()

max(SR)
max(SR.uci)
max(SR.lci)


load("output21/out/model_1.3.RData")
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

predt <- crosspred(basis_cases2, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag =1, cen = cen_c1) 

pdf("fig21/fig 2c.pdf", width = 6, height = 6)

# get exposures values
vars <- exp(predt$predvar)/100
vars<-vars[vars>0.03]
vars<-round(vars,2)

# obtain suppression risk (SR) fit and upper and lower confidence limits for all exposure variables
SR <- 1-predt$matRRfit
SR.uci <- 1-predt$matRRlow
SR.lci <- 1-predt$matRRhigh


SR<-SR[7:42,]
SR.uci<-SR.uci[7:42,]
SR.lci<-SR.lci[7:42,]

# set SR range 
s1 <- 0
s2 <- max(SR.uci)

# define colours
co <-colorRampPalette(c('sienna1','grey80'))(8)

# define x values (lag, by lag)
plot(vars, SR[,1], col = co[1], type = "l", lwd = 1.5, 
     xlab = "Incidence", ylab = "SR", main = "", 
     ylim = range(0, 0.10), frame.plot = T, axes = F)
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
mtext(side = 2, at = s2*1.25, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()

max(SR)
max(SR.uci)
max(SR.lci)

#####policy stringency index ----
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

#\ find position of the terms associated with basis_policy crossbasis
indt <- grep("basis_policy", model$names.fixed)

# set value
cen_c1 <- round(min(data$Stringency), 0)

# extract predictions from the policy stringency index DLNM centred on overall mean  
predt <- crosspred(basis_policy, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 1, cen = cen_c1) 

pdf("fig21/fig 2f.pdf", width = 6, height = 6)

# get exposures values
vars <- predt$predvar

# obtain suppression risk (SR) fit and upper and lower confidence limits for all exposure variables
SR <- 1-predt$matRRfit
SR.uci <- 1-predt$matRRlow
SR.lci <- 1-predt$matRRhigh
SRall<-1-predt$allRRfit

max(SR.lci)

# set SR range 
s1 <- 0
s2 <- max(SR.uci)

# define colours
co <-colorRampPalette(c('blue','lightblue'))(7)

# define x values (lag, by lag)

plot(vars, SR[,1], col = co[1], type = "l",lwd = 1.5, 
     xlab = "policy stringency index", ylab = "SR", main = "", 
     ylim = range(0, 0.25), frame.plot = T, axes = F)
axis(1,at=vars,labels = vars)
axis(2)
xx <- c(vars, rev(vars))
yy <- c(SR.lci[,1], rev(SR.uci[,1]))
lines(vars, SR[,2], col = co[2], type = "l", lwd = 1.5)
lines(vars, SR[,3], col = co[3], type = "l",lwd =1.5)
lines(vars, SR[,4], col = co[4], type = "l", lwd = 1.5)
lines(vars, SR[,5], col = co[5], type = "l",lwd = 1.5)
lines(vars, SR[,6], col = co[6], type = "l",lwd = 1.6)
lines(vars, SR[,7], col = co[7], type = "l",lwd = 1.6)
mtext(side = 2, at = s2*1.25, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()

max(SR)
max(SR.uci)
max(SR.lci)
