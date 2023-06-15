### R script to perform sensitivity analysis, testing exposure-lag-response associations using different variables

# load packages and pre-processed data
source("00_load_packages_data.R")

### plot lag response associations -----
###epidemic model+extreme temp
load("output21/in/model_1.3.RData")
model1.0 <- model
load("output21/in/model_2.10.RData")
model1.1 <- model
load("output21/in/model_2.12.RData")
model1.2 <- model

load("output21/out/model_1.3.RData")
model1.0 <- model
load("output21/out/model_2.13.RData")
model1.1 <- model
load("output21/out/model_2.14.RData")
model1.2 <- model

load("output21/out/model_1.3.RData")
model1.0 <- model
load("output21/out/model_2.15.RData")
model1.1 <- model
load("output21/out/model_2.16.RData")
model1.2 <- model


###epidemic model+extreme preca
load("output21/in/model_1.3.RData")
model1.0<- model
load("output21/in/model_2.20.RData")
model1.1 <- model
load("output21/in/model_2.22.RData")
model1.2 <- model
load("output21/in/model_2.21.RData")
model1.3 <- model

load("output21/in/model_1.3.RData")
model1.0<- model
load("output21/in/model_2.23.RData")
model1.1 <- model
load("output21/in/model_2.24.RData")
model1.2 <- model


####density
load("output21/in/model_3.112.RData")
model1.1 <- model
load("output21/in/model_3.122.RData")
model1.2 <- model
# load GDP interaction models
load("output21/in/model_3.132.RData")
model1.3 <- model

#######gdp
load("output21/in/model_4.102.RData")
model1.1 <- model
load("output21/in/model_4.112.RData")
model1.2 <- model
# load GDP interaction models
load("output21/in/model_4.122.RData")
model1.3 <- model

pdf("fig21/in 只有疫情+gdp.pdf", width = 18, height = 6)
par(mfrow = c(1,3))
mod.name<-c("model1.0","model1.1","model1.2")
lab <- c("a", "b","c")
table1 <- as.data.frame(matrix(NA, 3, 5))
colnames(table1) <- c("Setting", 
                      "lag", "sr", "lci","uci")
table1[,1] <- c("norm","low","high")

j=1
for (j in c(1,2,3))
{
  
  model <- eval(parse(text = as.name(mod.name[j]))) 
  
  # extract coeficients and variance-covariance matrix
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  indt <- grep("basis_cases2", model$names.fixed)
  
  # set value
  x1 <- min(data$case2[is.na(data$case2)==F&data$case2>0])
  cen_c1 <- round(x1,0)

  predp <- crosspred(basis_cases2, coef = coef[indt], vcov=vcov[indt,indt],
                     model.link = "log", bylag =0.25, cen = 1.14) 
  vars<-predp$predvar
  
  # obtain RR fit and upper and lower confidence limits for all exposure variables
  sr <-1-predp$matRRfit
  sr.uci <- 1-predp$matRRlow
  sr.lci <- 1-predp$matRRhigh
  
  sr[sr<0]<-0
  sr.uci[sr.uci<0]<-0
  sr.lci[sr.lci<0]<-0
  
  # define colors
  col1 <- brewer.pal(12, "Paired")[7]
  tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))
  
  col2 <- brewer.pal(12, "Paired")[4]
  tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))
  
  col3 <- brewer.pal(12, "Paired")[2]
  tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))
  
  # define x values (lag, by lag)
  lagbylag <- seq(0, nlag, 0.25)
  
  #  set fixed incidence
  plot(lagbylag, sr[24,],
       col = col1, type = "l", lwd = 1, 
       xlab = "Lag", ylab = "SR", main = "", 
       ylim = c(0,0.15), frame.plot = T, axes = F)
  axis(1, at = 0:nlag, labels = 0:nlag)
  axis(2)
  # points(lagbylag, rr[mn,], col = col1, pch = 20)
  xx <- c(lagbylag, rev(lagbylag))
  yy <- c(sr.lci[24,], rev(sr.uci[24,]))
  polygon(xx, yy, col = tcol1, border = tcol1)
  
  
  abline(h = 1, lty = 3)
  legend("top", legend = c("1.1"), col = c(col1), lwd = 2, lty = 1, bty = "n", y.intersp = 1.5)
  mtext(side = 2, text = lab[j], cex = 1.2, las = 2, at = max(0.3, 2.7)*1.1, line = 2)
  
  table1$lag[j] <- round(which.min(sr[24,]),1)-1
  table1$sr[j]  <- round(max(sr[24,]), 2)
  table1$lci[j] <- round(max(sr.lci[24,]), 2)
  table1$uci[j] <- round(max(sr.uci[24,]), 2)
}
dev.off() 
write.csv(table1, file = "fig21/inflow table incidence prec ipccd.csv", quote = FALSE, row.names= FALSE)


