## R script to prepare data and lagged variables for INLA-DLNM modelling

# install INLA
# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)

# load INLA
library(INLA)

#  select other packages
packages <- c("data.table", "tidyverse", "sf", "sp", "spdep",
              "dlnm", "tsModel", "hydroGOF","RColorBrewer", 
              "geofacet", "ggpubr", "ggthemes")

# install.packages
# lapply(packages, install.packages, character.only = TRUE)

# load packages
lapply(packages, library, character.only = TRUE)

## load data (could be replaced by data for oct 2020-jan 2021)
data_all <- read.csv("data/test data/data_all21.csv", header = T)

# load shape file 
map_all <- read_sf("data/city_cn/city_cn.shp")
map <- map_all[map_all$city_code %in% unique(data_all$city_code),]
# dim(map)

# Create adjacency matrix
sf::sf_use_s2(FALSE) # to fix 4 features with invalid spherical geometry
nb.map <- poly2nb(as_Spatial(map$geometry))
g.file <- "output/map.graph"
# if (!file.exists(g.file)) nb2INLA(g.file, nb.map)
nb2INLA(g.file, nb.map)

# Create lagged variables
# set max lag - by day
nlag = 21

# COVID-19 incidence
lag_cases2 <- tsModel::Lag(data_all$case2, group = data_all$city_code, k = 0:nlag)
# covid intervention stringency
lag_policy <- tsModel::Lag(data_all$Stringency, group = data_all$city_code, k = 0:nlag)

lag_cases2 <- lag_cases2[is.na(data_all$week)==F,]
lag_policy <- lag_policy[is.na(data_all$week)==F,]

data <- data_all[is.na(data_all$week)==F,]
head(data)

## define dimensions
# re-define time indicator 
unique(data$time)

# total number of days
ntime <- length(unique(data$time))
# total number of weeks
nweek <- length(unique(data$week))
# total number of cities
ncity <- length(unique(data$city_code))
# total number of provinces
nprov <- length(unique(data$prov_code))

# define cross-basis matrix (combining nonlinear exposure and lag functions)
# set lag knots
lagknot = equalknots(0:nlag, 2)


# COVID-19 incidence
var <- lag_cases2
basis_cases2 <- crossbasis(var,
                          argvar = list(fun="ns", knots = equalknots(data$case2, 2)),
                          arglag = list(fun="ns", knots = lagknot))
head(basis_cases2)

# covid intervention stringency
var <- lag_policy
basis_policy <- crossbasis(var,
                            argvar = list(fun="ns", knots = equalknots(data$Stringency, 2)),
                            arglag = list(fun="ns", knots = lagknot))
head(basis_policy)


##test linear interaction with extreme weather

temp_high <- data$Tmax-35
temp_low <- data$Tmax-4

preca_high <- data$PRECa-50
preca_high2 <- data$PRECa-20
preca<- data$PRECa-0


temp_high_cases<- basis_cases2*temp_high
temp_low_cases<- basis_cases2*temp_low

preca_high_cases<-basis_cases2*preca_high
preca_high2_cases<-basis_cases2*preca_high2
preca_0_cases<-basis_cases2*preca


##test linear interaction with different socioeconomic levels

dens_ind1 <- data$density - quantile(data$density, p = 0.75) # high 
dens_ind2 <- data$density - quantile(data$density, p = 0.5) # intermediate
dens_ind3 <- data$density - quantile(data$density, p = 0.25) # low
dens_ind4<-data$density-log(8000)

gdp_ind1 <- data$gdp - quantile(data$gdp, p = 0.75) # high
gdp_ind2 <- data$gdp - quantile(data$gdp, p = 0.5) # intermediate
gdp_ind3 <- data$gdp - quantile(data$gdp, p = 0.25) # low


temp_high_cases_dens1<- basis_cases2*temp_high*dens_ind1
temp_high_cases_dens2<- basis_cases2*temp_high*dens_ind2
temp_high_cases_dens3<- basis_cases2*temp_high*dens_ind3
temp_high_cases_dens4<- basis_cases2*temp_high*dens_ind4

temp_high_cases_gdp1<- basis_cases2*temp_high*gdp_ind1
temp_high_cases_gdp2<- basis_cases2*temp_high*gdp_ind2
temp_high_cases_gdp3<- basis_cases2*temp_high*gdp_ind3


# assign unique column names to cross-basis matrix for inla() model

colnames(basis_cases2) = paste0("basis_cases2.", colnames(basis_cases2))
colnames(basis_policy) = paste0("basis_policy.", colnames(basis_policy))

colnames(dens_basis1_cases) = paste0("dens_basis1_cases.", colnames(dens_basis1_cases))
colnames(dens_basis2_cases) = paste0("dens_basis2_cases.", colnames(dens_basis2_cases))
colnames(dens_basis3_cases) = paste0("dens_basis3_cases.", colnames(dens_basis3_cases))
colnames(dens_basis4_cases) = paste0("dens_basis4_cases.", colnames(dens_basis4_cases))

colnames(temp_high_cases) = paste0("temp_high_cases.", colnames(temp_high_cases))
colnames(temp_low_cases) = paste0("temp_low_cases.", colnames(temp_low_cases))

colnames(preca_high_cases) = paste0("preca_high_cases.", colnames(preca_high_cases))
colnames(preca_high2_cases) = paste0("preca_high2_cases.", colnames(preca_high2_cases))
colnames(preca_0_cases) = paste0("preca_0_cases.", colnames(preca_0_cases))

colnames(temp_high_cases_dens1) = paste0("temp_high_cases_dens1.", colnames(temp_high_cases_dens1))
colnames(temp_high_cases_dens2) = paste0("temp_high_cases_dens2.", colnames(temp_high_cases_dens2))
colnames(temp_high_cases_dens3) = paste0("temp_high_cases_dens3.", colnames(temp_high_cases_dens3))
colnames(temp_high_cases_dens4) = paste0("temp_high_cases_dens4.", colnames(temp_high_cases_dens4))

colnames(temp_high_cases_gdp1) = paste0("temp_high_cases_gdp1.", colnames(temp_high_cases_gdp1))
colnames(temp_high_cases_gdp2) = paste0("temp_high_cases_gdp2.", colnames(temp_high_cases_gdp2))
colnames(temp_high_cases_gdp3) = paste0("temp_high_cases_gdp3.", colnames(temp_high_cases_gdp3))

# create indices for INLA models
# note: for INLA models an index should start with 1 and with the max value equal to the length of unique values

# create city index 
data$city_index <- rep(1:ncity, ntime)

# create province index
# state length
k <- unique(data$prov_code)

for (j in 1:nprov){
  data$prov_index[data$prov_code == k[j]] <- j 
}

# create week index
# set first week for modelling to 1
data$week_index <- data$week

#### set up data and priors for INLA model
## set data for models - try intraflow first, then in_outflow
Y  <- data$intrarecovery # response variable
N  <- length(Y) # total number of data points
# E  <- data$pop_2019 # model offset 
# random variable
T1 <- data$weekday # for random effect to account for day-of-week effect
T2 <- data$week_index # for random effect to account for inter-week variability
S1 <- data$city_index # for city-level spatial random effect
S2 <- data$prov_index # for provincial interaction with daily random effect
# Other variables
Vg <- data$gdp
Vd <- data$density 
Vh <- data$holiday # holiday 
Vt <- data$TEMP
Vtmax <- data$Tmax
Vp<-data$PREC
Vta<-data$TEMPa
Vpa<-data$PRECa
Vc<-data$case2
psi<-data$Stringency

# create dataframe for model testing
df <- data.frame(Y, T1, T2, S1, S2, Vu, Vg, Vd ,Vh,Vt,Vp,Vta,Vpa,Vc,psi,Vtmax)
head(df)

# define priors
precision.prior2 <- list(prec = list(prior = "pc.prec", param = c(1, 0.01)))
# inla model function
mymodel <- function(formula, data = df, family = "Gaussian", config = FALSE)

  {
  model <- inla(formula = formula, data = data, family = family, 
       control.inla = list(strategy = 'adaptive',int.strategy='eb'), 
       control.compute = list(dic = TRUE, config = config, 
                              cpo = TRUE, return.marginals = FALSE),
       control.fixed = list(correlation.matrix = TRUE, 
                            prec.intercept = 1, prec =1),
       control.predictor = list(link = 1, compute = TRUE), 
       verbose = FALSE)
  model <- inla.rerun(model)
  return(model)
}
