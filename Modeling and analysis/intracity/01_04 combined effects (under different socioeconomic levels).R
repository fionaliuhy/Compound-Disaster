### R script testing exposure-lag-response associations using different variables

# load packages and pre-processed data
source("00_load_packages_data.R")

#### plot lag response associations for incidence interacted with extreme weather conditions
# load best fitting model with basis_incidence
load("output21/model_1.1.RData")
model0.0 <- model
# extreme weather
load("output21/model_2.10.RData")
model1.1 <- model
load("output21/model_2.12.RData")
model1.2 <- model
#extreme precipitation anomaly
load("output21/model_2.21.RData")
model1.3 <- model
load("output21/model_2.22.RData")
model1.4 <- model


# create model name and label strings
mod.name <- c("model0.0","model1.1", "model1.2", "model1.3", "model1.4")

lab <- c("a","b","c","d","e")

# make table to save relative effect for combined effects scenarios
table1 <- as.data.frame(matrix(NA, 5, 4))
colnames(table1) <- c("Setting","sr", "lci","uci")
table1[,1] <- c("norm","templow","temphigh","preca50","preca20")

j=1
for (j in 1:length(mod.name))
{
  
  model <- eval(parse(text = as.name(mod.name[j]))) 
  
  # extract coefficients and variance-covariance matrix
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  # create indicators for terms associated with incidence cross basis
  indt <- grep("basis_cases2", model$names.fixed)
  
  # set value
  x1 <- min(data$case2[is.na(data$case2)==F&data$case2>0])
  cen_c1 <- round(x1,0)
  
  predtbase <- crosspred(basis_cases2, coef = coef[indt], vcov=vcov[indt,indt],
                         model.link = "log", bylag =1, cen = cen_c1) 
  
  srb <- 1-predtbase$matRRfit
  srb.uci <- 1-predtbase$matRRlow
  srb.lci <- 1-predtbase$matRRhigh
  
  srb[srb<0]<-0
  srb.uci[srb.uci<0]<-0
  srb.lci[srb.lci<0]<-0
  
  ###set fixed incidence
  # srb<-srb[26,1:10]
  # srb.uci<-srb.uci[26,1:10]
  # srb.lci<-srb.lci[26,1:10]
  # get exposures values

  table1$sr[j]  <- round(max(srb), 2)
  table1$lci[j] <- round(max(srb.lci), 2)
  table1$uci[j] <- round(max(srb.uci), 2)
}
# save relative risk results
write.csv(table1, file = "fig21/table intra incidence sr.csv", quote = FALSE, row.names= FALSE)


#### plot lag response associations for incidence interacted with heat and density

load("output21/model_3.10.RData")
model0 <- model
load("output21/model_3.11.RData")
model1.1 <- model
load("output21/model_3.12.RData")
model1.2 <- model
load("output21/model_3.13.RData")
model1.3 <- model

# create model name and label strings
mod.name <- c("model0 ","model1.1", "model1.2","model1.3")
lab <- c("a", "b", "c","d") 

table1 <- as.data.frame(matrix(NA, 4, 4))
colnames(table1) <- c("Setting","sr", "lci","uci")
table1[,1] <- c("2000","3000","5300","8000")

j=1
for (j in 1:length(mod.name))
{
  
  model <- eval(parse(text = as.name(mod.name[j]))) 
  
  # extract coefficients and variance-covariance matrix
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  indt <- grep("basis_cases2", model$names.fixed)
  
  # set value
  x1 <- min(data$case2[is.na(data$case2)==F&data$case2>0])
  cen_c1 <- round(x1,0)
  
  predtbase <- crosspred(basis_cases2, coef = coef[indt], vcov=vcov[indt,indt],
                         model.link = "log", bylag =1, cen = cen_c1) 
  
  srb <- 1-predtbase$matRRfit
  srb.uci <- 1-predtbase$matRRlow
  srb.lci <- 1-predtbase$matRRhigh
  
  srb[srb<0]<-0
  srb.uci[srb.uci<0]<-0
  srb.lci[srb.lci<0]<-0
  
  ### could set fixed incidence here
  # srb<-srb[,1:10]
  # srb.uci<-srb.uci[,1:10]
  # srb.lci<-srb.lci[,1:10]
  
  
  # get exposures values

  table1$sr[j]  <- round(max(srb), 2)
  table1$lci[j] <- round(max(srb.lci), 2)
  table1$uci[j] <- round(max(srb.uci), 2)

}
# save relative risk results
write.csv(table1, file = "fig21/intra high incid temp+dens.csv", quote = FALSE, row.names = FALSE)


#### plot lag response associations for incidence interacted with heat and gdp
load("output21/model_4.10.RData")
model1.0 <- model
load("output21/model_4.11.RData")
model1.1 <- model
load("output21/model_4.12.RData")
model1.2 <- model

mod.name <- c("model1.0","model1.1", "model1.2")
lab <- c("a", "b", "c") 

table1 <- as.data.frame(matrix(NA, 3, 4))
colnames(table1) <- c("Setting", "sr", "lci","uci")
table1[,1] <- c("low gdp","md gdp","high gdp")
j=1
for (j in 1:length(mod.name))
{
  
  model <- eval(parse(text = as.name(mod.name[j]))) 
  
  # extract coefficients and variance-covariance matrix
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  indt <- grep("basis_cases2", model$names.fixed)
  
  # set value
  x1 <- min(data$case2[is.na(data$case2)==F&data$case2>0])
  cen_c1 <- round(x1,0)

  predtbase <- crosspred(basis_cases2, coef = coef[indt], vcov=vcov[indt,indt],
                         model.link = "log", bylag =1, cen = cen_c1) 
  
  srb <- 1-predtbase$matRRfit
  srb.uci <- 1-predtbase$matRRlow
  srb.lci <- 1-predtbase$matRRhigh
  
  srb[srb<0]<-0
  srb.uci[srb.uci<0]<-0
  srb.lci[srb.lci<0]<-0
 ###could set fixed incidence here
  # srb<-srb[,1:10]
  # srb.uci<-srb.uci[,1:10]
  # srb.lci<-srb.lci[,1:10]
  
  
  # get exposures values

  table1$sr[j]  <- round(max(srb), 2)
  table1$lci[j] <- round(max(srb.lci), 2)
  table1$uci[j] <- round(max(srb.uci), 2)

}
# save relative risk results
write.csv(table1, file = "fig21/intra high incid gdp.csv", quote = FALSE, row.names = FALSE)

