# R script to run INLA models of increasing complexity
# WARNING: the script may take over a day to run

# Step 0: load packages and pre-processed data 
# Step 1: formulate a baseline model including spatiotemporal random effects and test different combinations of DLNM variables

# Step 0: load packages and pre-processed data
source("00_load_packages_data.R")

# run models of increasing complexity in INLA

# Step 1: fit a baseline model including spatiotemporal random effects

## formulate a base model including: 
# rw1: province-specific random effects to account for day-of-week variation (random walk cyclic prior) - with Random walk model of order 1 (RW1)
# https://inla.r-inla-download.org/r-inla.org/doc/latent/rw1.pdf
# bym2: week-specific spatial random effects to account for inter-week variation in spatial overdisperson and dependency structures (modified Besag-York-Mollie prior bym2)
# https://inla.r-inla-download.org/r-inla.org/doc/latent/bym2.pdf




## baseline model
baseformula <- Y ~ 1 + f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE,
                         scale.model = TRUE,  hyper = precision.prior2) +
  f(S1, model = "bym2", replicate = T2, graph = "output/map.graph",
    scale.model = TRUE, hyper = precision.prior2)+Vh 

baseformula2 <- Y ~ 1 + f(T1, replicate = S1, model = "rw1", cyclic = TRUE,constr = TRUE,
                          scale.model = TRUE,  hyper = precision.prior2) +
  f(S1, model = "bym2", replicate = T2, graph = "output/map.graph",
    scale.model = TRUE, hyper = precision.prior2)+Vh

formulas <- list(baseformula,baseformula2)

# create model label string
lab <- c("base","base2")

models <- lapply(1:length(formulas),
                 function(i) {
                   model <- mymodel(formula = formulas[[i]], data = df, family = "Gaussian", config = FALSE)
                   save(model, file = paste0("output21_14/", lab[i],".RData"))})

# create table to store DIC and select best model
table0 <- data.table(Model  =  c( "base","base4"),
                     DIC = NA,
                     logscore = NA)


for(i in 1:length(formulas))
{
  load(paste0("output21/",lab[i],".RData"))
  table0$DIC[i] <- round(model$dic$dic, 2)
  table0$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
}

# view table
table0


# test baseline model
f1.0 <- update.formula(baseformula4, ~. + Vg)

# create a list of formulas
formulas <- list(f1.0)

# create model label string
lab <- c("model_1.0")

# WARNING: this may take a long time to run
models <- lapply(1:length(formulas),
                 function(i) {
                   model <- mymodel(formula = formulas[[i]], data = df, family = "Gaussian", config = FALSE)
                   save(model, file = paste0("output21/", lab[i],".RData"))})

# create table to store DIC and select best model
table0 <- data.table(Model  =  c("base","base4","model_1.0"),
                     DIC = NA,
                     logscore = NA)


for(i in 1:length(formulas))
{
  load(paste0("output21/",lab[i],".RData"))
  table0$DIC[i] <- round(model$dic$dic, 2)
  table0$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
}

# view table
table0

best.fit <- which.min(table1$DIC)

# Write results of model selection
fwrite(table0, file = "output21/best_model_selection0.csv", quote = FALSE,
       row.names = FALSE)

####set the baseline model
baseformula <- Y ~ 1 + f(T1, replicate = S1, model = "rw1", cyclic = TRUE,constr = TRUE,
                         scale.model = TRUE,  hyper = precision.prior2) +
  f(S1, model = "bym2", replicate = T2, graph = "output/map.graph",
    scale.model = TRUE, hyper = precision.prior2)+Vh+Vg

#define formulas by updating the baseline formula with different combinations of
#cross-basis functions of incidence and intervention index

f1.1 <- update.formula(baseformula, ~. + basis_cases2)
f1.10<- update.formula(baseformula, ~. + Vc)
f1.2 <- update.formula(baseformula, ~. + basis_policy)
f1.20<- update.formula(baseformula, ~. + psi)
f1.3 <- update.formula(baseformula, ~. + basis_cases2+basis_policy)

# create a list of formulas
formulas <- list(f1.1,f1.10,f1.2,f1.20,f1.3)

# create model label string
lab <- c( "model_1.1", "model_1.10","model_1.2","model_1.20","model_1.3")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
models <- lapply(1:length(formulas),
              function(i) {
                model <- mymodel(formula = formulas[[i]], data = df, family = "Gaussian", config = FALSE)
                save(model, file = paste0("output21/", lab[i],".RData"))})

# create table to store DIC and select best model
table1 <- data.table(Model  =  c( "model_1.1", "model_1.10","model_1.2","model_1.20","model_1.3"),
                     DIC = NA,
                     logscore = NA)

for(i in 1:length(formulas))
  {
  load(paste0("output21/",lab[i],".RData"))
  table1$DIC[i] <- round(model$dic$dic, 0)
  table1$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
}

# view table
table1

# define position of best fitting model
best.fit <- which.min(table1$DIC)

# Write results of model selection
fwrite(table1, file = "output21/best_model_selection1.csv", quote = FALSE,
       row.names = FALSE)


f1.4 <- update.formula(baseformula, ~. + basis_cases2+Vt)
f1.41 <- update.formula(baseformula, ~. + basis_cases2+Vtmax)
f1.42 <- update.formula(baseformula, ~. + basis_cases2+Vta)

f1.5 <- update.formula(baseformula, ~. + basis_cases2+Vp)
f1.51 <- update.formula(baseformula, ~. + basis_cases2+Vpa)

f1.6 <- update.formula(baseformula, ~. + basis_cases2+Vtmax+Vpa)

# create a list of formulas
formulas <- list(f1.4,f1.41,f1.42,f1.5,f1.51,f1.6)

# create model label string
lab <- c("model_1.4","model_1.41","model_1.42","model_1.5","model_1.51","model_1.6")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
models <- lapply(1:length(formulas),
                 function(i) {
                   model <- mymodel(formula = formulas[[i]], data = df, family = "Gaussian", config = FALSE)
                   save(model, file = paste0("output21/", lab[i],".RData"))})

# create table to store DIC and select best model
table2 <- data.table(Model  =  c("model_1.4","model_1.41","model_1.42","model_1.5","model_1.51","model_1.6"),
                     DIC = NA,
                     logscore = NA)

for(i in 1:length(formulas))
{
  load(paste0("output21/",lab[i],".RData"))
  table2$DIC[i] <- round(model$dic$dic, 2)
  table2$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
}

# view table
table2

# define position of best fitting model
best.fit <- which.min(table2$DIC)

# Write results of model selection
fwrite(table2, file = "output21/best_model_selection2.csv", quote = FALSE,
       row.names = FALSE)


# define formulas by updating the baseline formula with different combinations of 
#cross-basis functions of covid-19 incidence and extreme weather

f2.10 <- update.formula(baseformula, ~. + basis_cases2+temp_low_cases+Vtmax)
# f2.11 <- update.formula(baseformula, ~. + basis_cases2+temp_aver_cases+Vtmax)
f2.12 <- update.formula(baseformula, ~. + basis_cases2+temp_high_cases+Vtmax)

f2.20 <- update.formula(baseformula, ~. + basis_cases2 +preca_0_cases+Vpa)
f2.21 <- update.formula(baseformula, ~. + basis_cases2 +preca_high_cases+Vpa)
f2.22 <- update.formula(baseformula, ~. + basis_cases2 +preca_high2_cases+Vpa)

# f2.30 <- update.formula(baseformula, ~. + basis_cases2+basis_policy+temp_low_policy+Vtmax)
# f2.31 <- update.formula(baseformula, ~. + basis_cases2+basis_policy+ temp_aver_policy+Vtmax)
# f2.32 <- update.formula(baseformula, ~. + basis_cases2+basis_policy+ temp_high_policy+Vtmax)
# 
# f2.40 <- update.formula(baseformula, ~. + basis_cases2 +basis_policy+ preca_0_policy+Vpa)
# f2.41 <- update.formula(baseformula, ~. + basis_cases2 +basis_policy+ preca_high_policy+Vpa)

formulas <- list(f2.10,f2.12,f2.20,f2.21,f2.22)

# create model label string
lab <- c("model_2.10","model_2.12","model_2.20","model_2.21","model_2.22")

# # create a function to run a model for each formula in the list and save the model output to file
# # WARNING: this may take a long time to run
models <- lapply(1:length(formulas),
              function(i) {
                model <- mymodel(formula = formulas[[i]], data = df, family = "Gaussian", config = FALSE)
                save(model, file = paste0("output21/", lab[i],".RData"))})

# create table to store DIC and select best model
table3 <- data.table(Model  =  c("model_2.10","model_2.12","model_2.20","model_2.21","model_2.22"),
                     DIC = NA,
                     logscore = NA)

for(i in 1:length(formulas))
  {
  load(paste0("output21/",lab[i],".RData"))
  table3$DIC[i] <- round(model$dic$dic, 2)
  table3$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
}

# view table
table3

# define position of best fitting model
best.fit <- which.min(table3$DIC)

# Write results of model selection
fwrite(table2, file = "output21/best_model_selection3.csv", quote = FALSE,
       row.names = FALSE)
# 

####combined effect in cities with different socio-economic factors
f3.10 <- update.formula(baseformula, ~. +  basis_cases2+temp_high_cases_dens3+Vtmax+Vd)
f3.11 <- update.formula(baseformula, ~. +  basis_cases2+temp_high_cases_dens2+Vtmax+Vd)
f3.12 <- update.formula(baseformula, ~. +  basis_cases2+temp_high_cases_dens1+Vtmax+Vd)
f3.13 <- update.formula(baseformula, ~. +  basis_cases2+temp_high_cases_dens4+Vtmax+Vd)

formulas <- list(f3.10,f3.11,f3.12,f3.13)

# create model label string
lab <- c("model_3.10","model_3.11","model_3.12","model_3.13")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
models <- lapply(1:length(formulas),
                 function(i) {
                   model <- mymodel(formula = formulas[[i]], data = df, family = "Gaussian", config = FALSE)
                   save(model, file = paste0("output21/", lab[i],".RData"))})

# create table to store DIC and select best model
table4 <- data.table(Model  =  c("model_3.10","model_3.11","model_3.12","model_3.13"),
                      DIC = NA,
                      logscore = NA)

for(i in 1:length(formulas))
  {
  load(paste0("output21/",lab[i],".RData"))
  table4$DIC[i] <- round(model$dic$dic, 2)
  table4$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
}

# view table
table4

# define position of best fitting model
best.fit <- which.min(table4$DIC)

# Write results of model selection
fwrite(table4, file = "output21/best_model_selection3.csv", quote = FALSE,
       row.names = FALSE)

f4.10 <- update.formula(baseformula, ~. +  basis_cases2+ temp_high_cases_gdp3+Vtmax)
f4.11 <- update.formula(baseformula, ~. +  basis_cases2+ temp_high_cases_gdp2+Vtmax)
f4.12 <- update.formula(baseformula, ~. +  basis_cases2+ temp_high_cases_gdp1+Vtmax)

formulas <- list(f4.10,f4.11,f4.12)

# create model label string
lab <- c("model_4.10","model_4.11","model_4.12")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
models <- lapply(1:length(formulas),
                 function(i) {
                   model <- mymodel(formula = formulas[[i]], data = df, family = "Gaussian", config = FALSE)
                   save(model, file = paste0("output21/", lab[i],".RData"))})

table4 <- data.table(Model  =  c("model_4.10","model_4.11","model_4.12"),
                     DIC = NA,
                     logscore = NA)

for(i in 1:length(formulas))
{
  load(paste0("output21/",lab[i],".RData"))
  table4$DIC[i] <- round(model$dic$dic, 0)
  table4$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
}

# view table
table4
