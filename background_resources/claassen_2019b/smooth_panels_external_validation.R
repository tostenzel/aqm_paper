###### Estimating Estimating Smooth Country-Year Panels of Public Opinion
###### (c) Christopher Claassen 2018
###### Stan Estimation and External / Cross- Validation

library(countrycode)
library(MASS)
library(psych)
library(dplyr)
library(tidyr)
library(rstan)
library(loo)
library(rstanarm)
library(ggplot2)

# options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


## Read and edit data

# read support for dem data
supdem = read.csv("Support for democracy.csv")

# split and stack datasets
names(supdem)
sddem1 = gather(data=supdem, key="Item", value="Response", 6:33, na.rm=TRUE) 
sddem1$Sample = 0
sddem1[sddem1$Project=="AfB", "Sample"] = sddem1[sddem1$Project=="AfB", "AfB_sample"]
sddem1[sddem1$Project=="ArB", "Sample"] = sddem1[sddem1$Project=="ArB", "ArB_sample"]
sddem1[sddem1$Project=="AsB", "Sample"] = sddem1[sddem1$Project=="AsB", "AsB_sample"]
sddem1[sddem1$Project=="AsnB", "Sample"] = sddem1[sddem1$Project=="AsnB", "AsnB_sample"]
sddem1[sddem1$Project=="ESS", "Sample"] = sddem1[sddem1$Project=="ESS", "ESS_sample"]
sddem1[sddem1$Project=="LAPOP", "Sample"] = sddem1[sddem1$Project=="LAPOP", "LAPOP_sample"]
sddem1[sddem1$Project=="LB", "Sample"] = sddem1[sddem1$Project=="LB", "LB_sample"]
sddem1[sddem1$Project=="NDB", "Sample"] = sddem1[sddem1$Project=="NDB", "NDB_sample"]
sddem1[sddem1$Project=="Pew", "Sample"] = sddem1[sddem1$Project=="Pew", "Pew_sample"]
sddem1[sddem1$Project=="WVS", "Sample"] = sddem1[sddem1$Project=="WVS", "WVS_sample"]
sddem1[sddem1$Project=="CSES", "Sample"] = sddem1[sddem1$Project=="CSES", "CSES_sample"]

# trim stacked dataset
sddem1 = sddem1[, -c(6:16)]
sddem1$Response = ifelse(sddem1$Response==0, NA, sddem1$Response)
sddem1 = sddem1[!is.na(sddem1$Response), ] 

# drop countries with less than 2 years of data
cnt.obs.years = rowSums(table(sddem1$Country, sddem1$Year) > 0)
sddem2 = sddem1[sddem1$Country %in% levels(sddem1$Country)[cnt.obs.years > 1], ]

# create item by project indicators
sddem2 = unite(sddem2, ItemProj, c(Project, Item), sep = "_", remove = FALSE)
sddem2$ItemProj = as.factor(sddem2$ItemProj)

# create year by country indicators
year0 = 1990
sddem2 = sddem2[sddem2$Year > year0,]
sddem2 = unite(sddem2, YearCountry, c(Year, Country), sep = "_", remove = FALSE)
sddem2$YearCountry = as.factor(sddem2$YearCountry)

# create item by project by country indicators
sddem2 = unite(sddem2, ItemProjCnt, c(ItemProj, Country), sep = "_", remove = FALSE)
sddem2$ItemProjCnt = as.factor(sddem2$ItemProjCnt)

# create item by country indicators
sddem2 = unite(sddem2, ItemCnt, c(Item, Country), sep = "_", remove = FALSE)
sddem2$ItemCnt = as.factor(sddem2$ItemCnt)

# create year by project indicators
sddem2 = unite(sddem2, YrProj, c(Year, Project), sep = "_", remove = FALSE)
sddem2$YrProj = as.factor(sddem2$YrProj)

# factorise
sddem2$Country = as.factor(as.character(sddem2$Country))
sddem2$Item = as.factor(as.character(sddem2$Item))
sddem2$ItemProj = as.factor(as.character(sddem2$ItemProj))
sddem2$ItemCnt = as.factor(as.character(sddem2$ItemCnt))
sddem2$ItemProjCnt = as.factor(as.character(sddem2$ItemProjCnt))
sddem2$Project = as.factor(as.character(sddem2$Project))
sddem2$YrProj = as.factor(as.character(sddem2$YrProj))
sddem2$Year = as.factor(sddem2$Year-year0)

# create data vectors 
n.items = length(unique(sddem2$Item))
n.cntrys = length(unique(sddem2$Country))
n.yrs = 2015-year0
n.proj = length(unique(sddem2$Project))
n.resp = dim(sddem2)[1]
n.itm.prj = length(unique(sddem2$ItemProj))
n.cntry.yrs = n.cntrys * n.yrs
n.itm.prj.cnt = length(unique(sddem2$ItemProjCnt))
cntrys = as.numeric(factor(sddem2$Country))
cnt.names = sort(unique(sddem2$Country))
cnt.ccode = sddem2[match(cnt.names, sddem2$Country), "COWCode"]
items = as.numeric(factor(sddem2$Item))
yrs = as.numeric(sddem2$Year)
itm.prjs = as.numeric(factor(sddem2$ItemProj))
itm.prj.cnts = as.numeric(factor(sddem2$ItemProjCnt))
cntry.yrs = as.numeric(sddem2$YearCountry)

# create training set with 75-25 split
set.seed(1941)
test.inds = sample(x=dim(sddem2)[1], size=0.25*dim(sddem2)[1])
sd.train = sddem2[-test.inds,]
n.cntrys.trn = length(unique(sd.train$Country))
n.yrs.trn = 2015-year0
n.resp.trn = dim(sd.train)[1]
n.itm.prj.trn = length(unique(sd.train$ItemProj))
n.itm.prj.cnt.trn = length(unique(sd.train$ItemProjCnt))
n.cntry.yrs.trn = n.cntrys.trn * n.yrs.trn
cntrys.trn = cntrys[-test.inds]
cnt.names.trn = cnt.names[-test.inds]
cnt.ccode.trn = cnt.ccode[-test.inds]
itm.prjs.trn = itm.prjs[-test.inds]
itm.prj.cnts.trn = itm.prj.cnts[-test.inds]
yrs.trn = yrs[-test.inds]
cntry.yrs.trn = cntry.yrs[-test.inds]

# create test set
sd.test = sddem2[test.inds,]
n.resp.test = dim(sd.test)[1]
cntrys.test = cntrys[test.inds]
cnt.names.test = cnt.names[test.inds]
cnt.ccode.test = cnt.ccode[test.inds]
itm.prjs.test = itm.prjs[test.inds]
itm.prj.cnts.test = itm.prj.cnts[test.inds]
yrs.test = yrs[test.inds]
cntry.yrs.test = cntry.yrs[test.inds]

# examine full and training data
cbind(c(n.cntrys, n.cntrys.trn), c(n.yrs, n.yrs.trn), c(n.itm.prj, n.itm.prj.trn), c(n.itm.prj.cnt, n.itm.prj.cnt.trn))

# specify data for stan
dat.1 = list(N=n.resp.trn, K=n.itm.prj, T=n.yrs, J=n.cntrys, jj=cntrys.trn, tt=yrs.trn, kk=itm.prjs.trn, 
             x=sd.train$Response, samp=sd.train$Sample)
dat.2 = list(N=n.resp.trn, K=n.itm.prj, T=n.yrs, J=n.cntrys, P=n.itm.prj.cnt, jj=cntrys.trn, tt=yrs.trn, 
             pp=itm.prj.cnts.trn, kk=itm.prjs.trn, x=sd.train$Response, samp=sd.train$Sample)
sapply(list(dat.1, dat.2), summary)


# pars
pars.1 = c("mu_lambda","sigma_lambda","sigma_theta","lambda","theta")
pars.2 = c("mu_lambda","sigma_lambda","sigma_delta","sigma_theta","lambda","delta","theta")
pars.3 = c("Sigma","Omega","sigma_delta","sigma_theta","lambda","gamm","delta","theta")
pars.4 = c("mu_lambda","sigma_lambda","sigma_theta","phi","lambda","theta")
pars.5 = c("mu_lambda","sigma_lambda","sigma_delta","sigma_theta","phi","lambda","delta","theta")
pars.6 = c("Sigma","Omega","sigma_delta","sigma_theta","phi","lambda","gamm","delta","theta")


## fit models

# set pars for final run (time-consuming; reduce iterations for trial run)
n.iter = 1000
n.warm = 500
n.chn = 4
n.thin = 2

# binomial model with item intercepts
stan.mod.v1 = stan(file='supdem.stan.mod1.val.stan', data=dat.1, pars=pars.1, 
                  iter=n.iter, warmup=n.warm, chains=n.chn, thin=n.thin, 
                  control=list(adapt_delta=0.99, stepsize=0.005, max_treedepth=14))

# model v1 with item-country intercepts
stan.mod.v2 = stan(file='supdem.stan.mod2.val.stan', data=dat.2, pars=pars.2, 
                  iter=n.iter, warmup=n.warm, chains=n.chn, thin=n.thin, 
                  control=list(adapt_delta=0.99, stepsize=0.005, max_treedepth=14))

# model v2 with item slopes
stan.mod.v3 = stan(file='supdem.stan.mod3.val.stan', data=dat.2, pars=pars.3, 
                  iter=n.iter, warmup=n.warm, chains=n.chn, thin=n.thin, 
                  control=list(adapt_delta=0.99, stepsize=0.005, max_treedepth=15))

# beta-binomial model with item intercepts
stan.mod.v4 = stan(file='supdem.stan.mod4.val.stan', data=dat.1, pars=pars.4, 
                  iter=n.iter, warmup=n.warm, chains=n.chn, thin=n.thin, 
                  control=list(adapt_delta=0.99, stepsize=0.03, max_treedepth=11))

# model v4 with item-country intercepts
stan.mod.v5 = stan(file='supdem.stan.mod5.val.stan', data=dat.2, pars=pars.5, 
                  iter=n.iter, warmup=n.warm, chains=n.chn, thin=n.thin, 
                  control=list(adapt_delta=0.99, stepsize=0.02, max_treedepth=11))

# model v5 with item slopes
stan.mod.v6 = stan(file='supdem.stan.mod6.val.stan', data=dat.2, pars=pars.6, 
                  iter=n.iter, warmup=n.warm, chains=n.chn, thin=n.thin, 
                  control=list(adapt_delta=0.99, stepsize=0.01, max_treedepth=13))

# save workspace
save.image("supdem_stan_valid_workspace.RData")

# list of models
model.list = list(stan.mod.v1, stan.mod.v2, stan.mod.v3, stan.mod.v4, stan.mod.v5, stan.mod.v6)

# convergence
sapply(lapply(model.list, function(x) unlist(rstan::extract(x, pars="lp__"))), length)
lapply(lapply(model.list, function(x) summary(x)$summary[,"Rhat"]), function(x) sum(x >= 1.05, na.rm=TRUE))
lapply(model.list, function(x) rowSums(print(get_elapsed_time(x))))

summary(stan.mod.v6)$summary[,"Rhat"][order(summary(stan.mod.v6)$summary[,"Rhat"], decreasing=TRUE)][1:20]



## Calculate error in test dataset (external validation part of Table 3)

# read in table 3 results 
tab3 = read.csv("Table3.csv", row.names=1)

# extract actual response proportions from test dataset
test.vals = sd.test$Response / sd.test$Sample

# MAE fit of baseline country-only
cntry.mean = as.vector(by(sd.train$Response/sd.train$Sample, sd.train$Country, mean))
cnt.mae = sum(abs(test.vals - cntry.mean[cntrys.test])) / length(test.inds)
tab3[8,4] = round(cnt.mae, 3)

# MAE fit of baseline item-project only
item.mean = as.vector(by(sd.train$Response/sd.train$Sample, sd.train$ItemProj, mean, na.rm=TRUE))
itm.mae = sum(abs(test.vals - item.mean[itm.prjs.test])) / length(test.inds)
tab3[9,4] = round(itm.mae, 3)
tab3[9,5] = round((cnt.mae-itm.mae)/cnt.mae*100, 1)

# MAE fit of baseline grand mean
grd.mae = sum(abs(test.vals - mean(sd.train$Response/sd.train$Sample, na.rm=TRUE)) / length(test.inds))
tab3[10,4] = round(grd.mae, 3)
tab3[10,5] = round((cnt.mae-grd.mae)/cnt.mae*100, 1)

# fit for models 1 and 4

for(i in c(1,4)) {

  # current model
  stan.mod = model.list[[i]]

  # extract parameters
  lambdas = rstan::extract(stan.mod, pars="lambda")[[1]]
  thetas = rstan::extract(stan.mod, pars="theta")[[1]]

  # create predicted proportions matrix -- MCMC samples in rows and test observations in columns
  pred.prop = matrix(NA, dim(thetas)[1], length(test.inds))
  for (j in 1:length(test.inds)) {
  pred.prop[,j] = arm::invlogit(lambdas[, itm.prjs.test[j]] + thetas[, yrs.test[j], cntrys.test[j]])
  }

  # get predicted proportions and CIs
  pred.prop.mean = apply(pred.prop, 2, mean)
  pred.ci = data.frame(u80 = apply(pred.prop, 2, quantile, 0.9), l80 = apply(pred.prop, 2, quantile, 0.1))

  # MAE
  tab3[i,4] = round(sum(abs(test.vals - pred.prop.mean)) / length(test.inds), 3) 

  # CI coverage
  tab3[i,6] = round(sum(test.vals >= pred.ci$l80 & test.vals <= pred.ci$u80) / length(pred.ci$u80) * 100, 1)

  # % improvement
  tab3[i,5] = round((tab3[8,4] - tab3[i,4]) / tab3[8,4] * 100, 1)
  
}

# fit for models 2 and 5

for(i in c(2,5)) {
  
  # current model
  stan.mod = model.list[[i]]

  # extract parameters
  lambdas = rstan::extract(stan.mod, pars="lambda")[[1]]
  thetas = rstan::extract(stan.mod, pars="theta")[[1]]
  deltas = rstan::extract(stan.mod, pars="delta")[[1]]

  # create predicted proportions matrix -- MCMC samples in rows and test observations in columns
  pred.prop = matrix(NA, dim(thetas)[1], length(test.inds))
  for (j in 1:length(test.inds)) {
  pred.prop[,j] = arm::invlogit(lambdas[, itm.prjs.test[j]] + thetas[, yrs.test[j], cntrys.test[j]]
                                + deltas[, itm.prj.cnts.test[j]])
  }

  # get predicted proportions and CIs
  pred.prop.mean = apply(pred.prop, 2, mean)
  pred.ci = data.frame(u80 = apply(pred.prop, 2, quantile, 0.9), l80 = apply(pred.prop, 2, quantile, 0.1))

  # MAE
  tab3[i,4] = round(sum(abs(test.vals - pred.prop.mean)) / length(test.inds), 3)

  # CI coverage
  tab3[i,6] = round(sum(test.vals >= pred.ci$l80 & test.vals <= pred.ci$u80) / length(pred.ci$u80) * 100, 1)

  # % improvement
  tab3[i,5] = round((tab3[8,4] - tab3[i,4]) / tab3[8,4] * 100, 1)
  
}

# fit for models 3 and 6

for(i in c(3,6)) {
  
  # current model
  stan.mod = model.list[[i]]

  # extract parameters
  lambdas = rstan::extract(stan.mod, pars="lambda")[[1]]
  gammas = rstan::extract(stan.mod, pars="gamm")[[1]]
  thetas = rstan::extract(stan.mod, pars="theta")[[1]]
  deltas = rstan::extract(stan.mod, pars="delta")[[1]]

  # create predicted proportions matrix -- MCMC samples in rows and test observations in columns
  pred.prop = matrix(NA, dim(thetas)[1], length(test.inds))
  for (j in 1:length(test.inds)) {
  pred.prop[,j] = arm::invlogit(lambdas[, itm.prjs.test[j]] + thetas[, yrs.test[j], cntrys.test[j]]
                                * gammas[, itm.prjs.test[j]] + deltas[, itm.prj.cnts.test[j]])
  }

  # get predicted proportions and CIs
  pred.prop.mean = apply(pred.prop, 2, mean)
  pred.ci = data.frame(u80 = apply(pred.prop, 2, quantile, 0.9), l80 = apply(pred.prop, 2, quantile, 0.1))

  # MAE
  tab3[i,4] = round(sum(abs(test.vals - pred.prop.mean)) / length(test.inds), 3)

  # CI coverage
  tab3[i,6] = round(sum(test.vals >= pred.ci$l80 & test.vals <= pred.ci$u80) / length(pred.ci$u80) * 100, 1)

  # % improvement
  tab3[i,5] = round((tab3[8,4] - tab3[i,4]) / tab3[8,4] * 100, 1)
  
}

# print table 3 (this output includes only the external validation part of table 3)
write.csv(tab3, file="Table3.csv")



