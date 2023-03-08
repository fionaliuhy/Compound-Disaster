
####  part A:mean absolute error (MAE) test 
####  part B: cross-validation test

source("00_load_packages_data.R")
# load pre-defined grid of Chinese provinces for geofacet plots
grid <- read.csv("data/cn_province_grid.csv")
province <- read.csv("data/Provinces.csv")

# load shape file 
map_all <- read_sf("data/city_cn/city_cn.shp")
map_all$city_code <- as.integer(map_all$city_code)
map <- map_all[map_all$city_code %in% unique(data_all$city_code),]

data <- merge(data, province[,c('prov_code', 'region', 'Province2', 'Climate', 'Northern_southern', 'Inland_coastal_province')], by.x='prov_code', all.x=T)

####  part A:mean absolute error (MAE) test 
# load baseline and selected model
load("output21/in/model_1.0.RData")
basemodel <- model
load("output21/in/model_3.13.RData")

### compare selected model to the baseline model using the mean absolute error ----
# add baseline fitted model result summaries (2.5%, 50%, 97.5% percentiles) to data
data$base.fit <- basemodel$summary.fitted.values$`0.5quant`
data$base.fit.lci <- basemodel$summary.fitted.values$`0.025quant`
data$base.fit.uci <- basemodel$summary.fitted.values$`0.975quant`

# add selected fitted model result summaries (2.5%, 50%, 97.5% percentiles) to data
data$fit <- model$summary.fitted.values$`0.5quant`
data$fit.lci<-model$summary.fitted.values$`0.025quant`
data$fit.uci<-model$summary.fitted.values$`0.975quant`
head(data)

# compute mean absolute error (MAE) and compare base model to final model
MAE <- as.data.frame(matrix(NA, nrow = 313, ncol = 2))
names(MAE) <-c("base", "new")

# calculate the MAE for observed and mean fit mobility
for (i in 1:313){
  # mobility
  MAE$base[i] <- hydroGOF::mae(data$base.fit[data$city_index == i], 
                               data$rein[data$city_index == i], # or data$in_outflow, if test in_outflow
                               na.rm = TRUE)
  MAE$new[i] <- hydroGOF::mae(data$fit[data$city_index == i], 
                              data$rein[data$city_index == i], # or data$in_outflow, if test in_outflow
                              na.rm = TRUE)
}

# calculate difference between MAE from the baseline model and MAE from the selected model
MAE$diff <- MAE$base - MAE$new
mn <-min(MAE$diff)
mx <-max(MAE$diff)
write.csv(MAE,file = "fig21/MAE in incidence-temp-dens.csv",row.names = F)

MAE$value <- 1

# Specify the region where the difference is greater than or equal to 0 as '2', that is, the MAE of the new model is smaller (better than that of the base model)
MAE$value[MAE$diff >= 0 ] <- 2
head(MAE)

# plot map to show areas where the new model provided 'added value' over the basemodel (e.g. MAE is smaller for new model) 
value_map <- ggplot(map) + 
  geom_sf(data = map_all, aes(), fill=NA, colour='grey', lwd = 0.1) +
  geom_sf(aes(fill = factor(MAE$value)), lwd = 0) +
  scale_fill_manual(values = c("steelblue","darkgoldenrod1"), breaks = 1:2, 
                    labels = c("No added value", "Added value")) +
  labs(fill = "") +
  theme_void()

ggsave(value_map, filename = "fig21/mae in incidence-temp-dens.pdf")



####  part B: cross-validation test
wweek = as.integer(commandArgs(trailingOnly=TRUE)[1])
wweekday = as.integer(commandArgs(trailingOnly=TRUE)[2])
print(c(wweek, wweekday))
inla.setOption(num.threads = "4:1")


# Script to run INLA models in cross validation prediction mode

# Step 1: rerun the selected model (fitted with config = TRUE for sampling) 

# fit full final model to use with control.mode = list(result = model1, restart = TRUE)
formula <- Y ~ 1 + f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE,
                     scale.model = TRUE,  hyper = precision.prior2) +
  f(S1, model = "bym2", replicate = T2, graph = "output/map.graph",
    scale.model = TRUE, hyper = precision.prior2)+Vh +Vg+ basis_cases2+ basis_policy+temp_high_cases_dens4+Vtmax+Vd

model1 <- inla(formula, data = df, family = "Gaussian", 
               control.inla = list(strategy = 'adaptive',int.strategy='eb'), 
               control.compute = list(dic = TRUE, config = TRUE, 
                                      cpo = TRUE, return.marginals = FALSE),
               control.fixed = list(correlation.matrix = TRUE, 
                                    prec.intercept = 1, prec = 1),
               control.predictor = list(link = 1, compute = TRUE), 
               verbose = FALSE)
model1 <- inla.rerun(model1)
save(model1, file = "pred in/model1_config.RData")

if (FALSE) {
    model1 <- inla(formula, data = df, family = "Gaussian", 
                   control.inla = list(strategy = 'adaptive',int.strategy='eb'), 
                   control.compute = list(dic = TRUE, config = TRUE, 
                                          cpo = TRUE, return.marginals = FALSE),
                   control.fixed = list(correlation.matrix = TRUE, 
                                        prec.intercept = 1, prec = 1),
                   control.predictor = list(link = 1, compute = TRUE), 
                   verbose = FALSE)
    model1 <- inla.rerun(model1)
    save(model1, file = "pred in/model1_config.RData")
} else {
    load(file = "pred in/model1_config.RData")
    model1$misc$configs <- NULL
    gc()
}

# Step 2: produce cross-validated posterior predictive samples leaving out one week at a time

# define number of samples
s <- 1000
for (i in 1:46){
    if (TRUE) {
      # replace mobility data in testing period with NA for out of sample prediction
      casestopred <- data$rein# response variable
      idx.pred <- which(data$week_index == i)
      casestopred[idx.pred] <- NA # replace mobility in week of interest to NA
      mpred <- length(idx.pred)
      
      # set response variable and week indicator
      df$Y <- casestopred
      
      # final model
      formula <- Y ~ 1 + f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE,
                           scale.model = TRUE,  hyper = precision.prior2) +
        f(S1, model = "bym2", replicate = T2, graph = "output/map.graph",
          scale.model = TRUE, hyper = precision.prior2)+Vh +Vg+ basis_cases2+ basis_policy+temp_high_cases_dens4+Vtmax+Vd
      
      if (TRUE) {
          model <- inla(formula, data = df, family = "Gaussian", 
                        control.inla = list(strategy = 'adaptive',int.strategy='eb'), 
                        control.compute = list(dic = TRUE, config = TRUE, 
                                               cpo = TRUE, return.marginals = FALSE),
                        control.fixed = list(correlation.matrix = TRUE, 
                                             prec.intercept = 1, prec = 1),
                        control.predictor = list(link = 1, compute = TRUE), 
                        control.mode = list(result = model1, restart = TRUE),
                        verbose = TRUE)
          model <- inla.rerun(model)
      } else {
          model <- model1
      }
    xx <- inla.posterior.sample(s, model)
    idx.pred <- which(data$week_index == i)
    xx.s <-inla.posterior.sample.eval(function(...)c(theta[1],Predictor[idx.pred]),xx)
    mpred <-length(idx.pred)
    y.pred <- matrix (NA, mpred, s)
    for (s.idx in 1:s){
      xx.sample <- xx.s[,s.idx]
      y.pred[,s.idx] <- rnorm(mpred,mean=xx.sample[-1],sd=1/xx.sample[1])
    }
    preds <- list(week = i, idx.pred = idx.pred, 
                  mean = apply(y.pred, 1, mean), median = apply(y.pred, 1, median),
                  lci = apply(y.pred, 1, quantile, probs = c(0.025)),
                  uci = apply(y.pred, 1, quantile, probs = c(0.975)))
    save(preds, file = paste0("pred in/preds_",i,"week",".RData"))
} }


### Step 3: compare cross-validated posterior predictions to observations ----
# add cross-validation posterior predictive summaries (already processed) to data

data$pred.mean <- NA
data$pred.median <- NA
data$pred.lci <- NA
data$pred.uci <- NA

for (i in 1:46){ # no of weeks
    load(paste0("pred in/preds_",i,"week",".RData"))
    data$pred.mean[preds$idx.pred] <- preds$mean
    data$pred.median[preds$idx.pred] <- preds$median
    data$pred.lci[preds$idx.pred] <- preds$lci
    data$pred.uci[preds$idx.pred] <- preds$uci
}

# plot observed v fitted mobility recovery per province

data0 <- data[is.na(data$rein)==F,]
recovery_ts_prov <- 
  data0 %>% 
  group_by(week2, prov_code) %>%
  # calculate provincial level mobility
  summarise(recovery = mean(rein),
            fit = mean(pred.mean), 
            fit.lci = mean(pred.lci),
            fit.uci = mean(pred.uci)) %>% 
  mutate(inc = recovery,
         inc.fit = fit,
         inc.fit.lci = fit.lci,
         inc.fit.uci = fit.uci) %>% 
  # add the predefined province grid by prov code
  left_join(grid, by = c("prov_code" = "code_num")) %>% 
  ggplot() + 
  geom_ribbon(aes(x = week2, ymin = inc.fit.lci, ymax = inc.fit.uci), 
              fill = "tomato", alpha = 0.5) + 
  geom_line(aes(x = week2, y = inc.fit, col = "tomato")) +
  geom_line(aes(x = week2, y = inc, col = "#499894")) +
  xlab("Week") +
  ylab("Inflow recovery degree (*100%)") +
  scale_colour_identity(name = "",
                        breaks = c("#499894", "tomato"),
                        labels = c("Observed", "Posterior predicted"),
                        guide = "legend") +
  scale_x_continuous(breaks = c(1,15,30,46), labels = c("Week 1","Week 15","Week 30","Week 46")) +
  scale_y_continuous(breaks = c(0, 1.0, 2.0), labels = c("0.0", "1.0", "2.0")) + 
  theme_bw() + 
  theme(axis.text.x = element_text(size = rel(0.85)), legend.position = "bottom") +
  # organise by province name in grid file
  facet_geo( ~name, grid = grid)

ggsave("fig21/fig_S06_mobility_obs_pred_province inflow.pdf", height = 30, width = 25, units = "cm")
