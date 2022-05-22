###### Estimating Estimating Smooth Country-Year Panels of Public Opinion
###### (c) Christopher Claassen 2018
###### Fitting Caughey and Warshaw DGO model to support for democracy training data

library(dplyr)
library(tidyr)
library(ggplot2)
library(rstan)
library(devtools)
library(dgo)

# options
rstan_options(auto_write = TRUE)
options(mc.cores=parallel::detectCores())


## Load and edit data

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

# factorise
sddem2$Country = as.factor(as.character(sddem2$Country))
sddem2$Item = as.factor(as.character(sddem2$Item))
sddem2$ItemProj = as.factor(as.character(sddem2$ItemProj))
sddem2$ItemProjCnt = as.factor(as.character(sddem2$ItemProjCnt))
sddem2$Project = as.factor(as.character(sddem2$Project))
sddem2$Year = as.factor(sddem2$Year-year0)

# Create training and test datasets with 75-25 split
set.seed(1941)
test.inds = sample(x=dim(sddem2)[1], size=0.25*dim(sddem2)[1])
sd.train = sddem2[-test.inds,]
sd.test = sddem2[test.inds,]
n.cntrys.trn = length(unique(sd.train$Country))
n.yrs.trn = 2015-year0
n.resp.trn = dim(sd.train)[1]
n.itm.prj.trn = length(unique(sd.train$ItemProj))
n.itm.prj.cnt.trn = length(unique(sd.train$ItemProjCnt))
n.cntry.yrs.trn = n.cntrys.trn * n.yrs.trn

# examine full and training data
cbind(c(length(unique(sd.train$Country)), length(unique(sddem2$Country))), 
      c(length(unique(sd.train$ItemProj)), length(unique(sddem2$ItemProj))), 
      c(length(unique(sd.train$ItemProjCnt)), length(unique(sddem2$ItemProjCnt))))

# create indices
cntrys = as.numeric(factor(sddem2$Country))
cnt.names = sort(unique(sddem2$Country))
cnt.ccode = sddem2[match(cnt.names, sddem2$Country), "COWCode"]
cnt.abb = as.numeric(factor(sddem2$CAbb))
items = as.numeric(factor(sddem2$Item))
yrs = as.numeric(sddem2$Year)
itm.prjs = as.numeric(factor(sddem2$ItemProj))
cntry.yrs = as.numeric(sddem2$YearCountry)

cntrys.test = cntrys[test.inds]
cnt.names.test = cnt.names[test.inds]
cnt.abb.test = cnt.abb[test.inds]
cnt.ccode.test = cnt.ccode[test.inds]
itm.prjs.test = itm.prjs[test.inds]
yrs.test = yrs[test.inds]
cntry.yrs.test = cntry.yrs[test.inds]


## Run DGO model

# data setup

sddem.dgo = sd.train[, c("ItemProj", "CAbb", "Year", "Response", "Sample")]
row.names(sddem.dgo) = 1:dim(sddem.dgo)[1]
names(sddem.dgo)[c(1,4,5)] = c("item", "s_grp", "n_grp")
sddem.dgo$Year = as.numeric(sddem.dgo$Year)
item.names = levels(sddem.dgo$item)
sddem.dgo$item = as.character(sddem.dgo$item)
sddem.dgo$CAbb = as.character(sddem.dgo$CAbb)

supdem.shape = dgo::shape(aggregate_data=sddem.dgo, item_names="item", time_name="Year", geo_name="CAbb", 
                          group_names=NULL, aggregate_item_names=NULL)
summary(supdem.shape)

# run DGO (very time-consuming -- reduce iter for trial run)
dgo.mod = dgirt(supdem.shape, version="2017_01_04", hierarchical_model=TRUE, 
               iter=2000, chains=4, thin=4, control=list(adapt_delta=0.99, stepsize=0.1, max_treedepth=14))

save.image("supdem_stan_dgo_workspace.RData")

# diagnostics
dgo.mod
tail(rhats(dgo.mod)[order(rhats(dgo.mod)$Rhat),])


## Validate DGO

# extract parameters
kappas = rstan::extract(dgo.mod, pars="kappa")[[1]]
thetas = rstan::extract(dgo.mod, pars="theta_bar")[[1]]
sigma_thetas = rstan::extract(dgo.mod, pars="sd_theta")[[1]]
sigma_items = rstan::extract(dgo.mod, pars="sd_item")[[1]]

# create predicted proportions matrix -- MCMC samples in rows and test observations in columns
pred.prop = matrix(NA, dim(thetas)[1], length(test.inds))
for (i in 1:length(test.inds)) {
  pred.prop[,i] = pnorm( 
    ( thetas[, yrs.test[i], cnt.abb.test[i]] - kappas[, 1, itm.prjs.test[i]] ) /
    sqrt( sigma_thetas[, yrs.test[i]]^2 + sigma_items[, itm.prjs.test[i]]^2 ) 
    )
}

# get predicted proportions and CIs
pred.prop.mean = apply(pred.prop, 2, mean)
pred.ci = data.frame(u80 = apply(pred.prop, 2, quantile, 0.9), l80 = apply(pred.prop, 2, quantile, 0.1))

# compare predicted and actual data
test.vals = sd.test$Response / sd.test$Sample


## DGIRT results in table 3

# read in table 3 results 
#tab3 = read.csv("Table3.csv", row.names=1)

# MAE 
#dgo.mae = sum(abs(test.vals - pred.prop.mean)) / length(test.inds)
#tab3[7,4] = round(dgo.mae, 3)

# CI coverage
#tab3[7,6] = round(sum(test.vals >= pred.ci$l80 & test.vals <= pred.ci$u80) / length(pred.ci$u80) * 100, 1)

# % improvement over country-only
#cntry.mean = as.vector(by(sd.train$Response/sd.train$Sample, sd.train$Country, mean))
#cntry.mean.mae = sum(abs(test.vals - cntry.mean[cntrys.test])) / length(test.inds) 
#tab3[7,5] = round((cntry.mean.mae - dgo.mae) / cntry.mean.mae * 100, 1)

# add grand mean MAE and % improvement
#grand.mean = mean(sd.train$Response/sd.train$Sample, na.rm=TRUE)
#tab3[10,4] = round(sum(abs(test.vals - grand.mean)) / length(test.inds), 3)
#tab3[10,5] = round((cntry.mean.mae - tab3[10,4]) / cntry.mean.mae * 100, 1)

# save final version of table 3
#write.csv(tab3, file="Table3.csv")
