###### Estimating Estimating Smooth Country-Year Panels of Public Opinion
###### (c) Christopher Claassen 2018
###### Stan Estimation and Internal Validation

library(MASS)
library(psych)
library(arm)
library(dplyr)
library(tidyr)
library(rstan)
library(loo)
library(rstanarm)
library(ggplot2)
library(RColorBrewer)

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

# create item by project indicators
sddem1 = unite(sddem1, ItemProj, c(Project, Item), sep = "_", remove = FALSE)
sddem1$ItemProj = as.factor(sddem1$ItemProj)

# create year by country indicators
year0 = 1990
sddem1 = sddem1[sddem1$Year > year0,]
sddem1 = unite(sddem1, YearCountry, c(Year, Country), sep = "_", remove = FALSE)
sddem1$YearCountry = as.factor(sddem1$YearCountry)

# create item by project by country indicators
sddem1 = unite(sddem1, ItemProjCnt, c(ItemProj, Country), sep = "_", remove = FALSE)
sddem1$ItemProjCnt = as.factor(sddem1$ItemProjCnt)

# create item by country indicators
sddem1 = unite(sddem1, ItemCnt, c(Item, Country), sep = "_", remove = FALSE)
sddem1$ItemCnt = as.factor(sddem1$ItemCnt)

# create year by project indicators
sddem1 = unite(sddem1, YrProj, c(Year, Project), sep = "_", remove = FALSE)
sddem1$YrProj = as.factor(sddem1$YrProj)

# create year by project by country indicators
sddem1 = unite(sddem1, YrProjCnt, c(YrProj, Country), sep = "_", remove = FALSE)
sddem1$YrProjCnt = as.factor(sddem1$YrProjCnt)

# factorise
sddem1$Country = as.factor(as.character(sddem1$Country))
sddem1$Item = as.factor(as.character(sddem1$Item))
sddem1$ItemProj = as.factor(as.character(sddem1$ItemProj))
sddem1$ItemCnt = as.factor(as.character(sddem1$ItemCnt))
sddem1$ItemProjCnt = as.factor(as.character(sddem1$ItemProjCnt))
sddem1$Project = as.factor(as.character(sddem1$Project))
sddem1$YrProj = as.factor(as.character(sddem1$YrProj))
sddem1$YrProjCnt = as.factor(as.character(sddem1$YrProjCnt))
sddem1$Year = as.factor(sddem1$Year-year0)

# count data
length(unique(sddem1$Country))
length(unique(sddem1$Project))
length(unique(sddem1$ItemCnt))
length(unique(sddem1$YrProjCnt))

# drop countries with less than 2 years of data
cnt.obs.years = rowSums(table(sddem1$Country, sddem1$Year) > 0)
sddem2 = sddem1[sddem1$Country %in% levels(sddem1$Country)[cnt.obs.years > 1], ]



## Stan estimation

# prepare data for stan

n.items = length(unique(sddem2$Item))
n.cntrys = length(unique(sddem2$Country))
n.yrs = 2015-year0
n.proj = length(unique(sddem2$Project))
n.resp = dim(sddem2)[1]
n.itm.prj = length(unique(sddem2$ItemProj))
n.itm.cnt = length(unique(sddem2$ItemCnt))
n.cntry.yrs = n.cntrys * n.yrs
n.itm.prj.cnt = length(unique(sddem2$ItemProjCnt))
n.yr.proj.cnt = length(unique(sddem2$YrProjCnt))
cntrys = as.numeric(factor(sddem2$Country))
cnt.names = sort(unique(sddem2$Country))
cnt.ccode = sddem2[match(cnt.names, sddem2$Country), "COWCode"]
items = as.numeric(factor(sddem2$Item))
yrs = as.numeric(sddem2$Year)
projs = as.numeric(factor(sddem2$Project))
itm.prjs = as.numeric(factor(sddem2$ItemProj))
itm.prj.cnts = as.numeric(factor(sddem2$ItemProjCnt))
itm.cnts = as.numeric(factor(sddem2$ItemCnt))
cntry.yrs = as.numeric(sddem2$YearCountry)

# specify data for stan

dat.1 = list(N=n.resp, K=n.itm.prj, T=n.yrs, J=n.cntrys, jj=cntrys, tt=yrs, kk=itm.prjs, 
             x=sddem2$Response, samp=sddem2$Sample)
dat.2 = list(N=n.resp, K=n.itm.prj, T=n.yrs, J=n.cntrys, P=n.itm.prj.cnt, jj=cntrys, tt=yrs, 
             pp=itm.prj.cnts, kk=itm.prjs, x=sddem2$Response, samp=sddem2$Sample)
sapply(list(dat.1, dat.2), summary)

# pars

pars.1 = c("mu_lambda","sigma_lambda","sigma_theta","lambda","theta","x_pred","log_lik")
pars.2 = c("mu_lambda","sigma_lambda","sigma_delta","sigma_theta","lambda","delta","theta",
            "x_pred","log_lik")
pars.3 = c("Sigma","Omega","sigma_delta","sigma_theta","lambda","gamm","delta","theta",
           "x_pred","log_lik")
pars.4 = c("mu_lambda","sigma_lambda","sigma_theta","phi","lambda","theta","x_pred","log_lik")
pars.5 = c("mu_lambda","sigma_lambda","sigma_delta","sigma_theta","phi","lambda","delta","theta",
            "x_pred","log_lik")
pars.6 = c("Sigma","Omega","sigma_delta","sigma_theta","phi","lambda","gamm","delta","theta",
           "x_pred","log_lik")

# set pars for final run (time-consuming; reduce iterations for trial run)
n.iter = 1000
n.warm = 500
n.chn = 4
n.thin = 2

# binomial model with item intercepts
stan.mod.1 = stan(file='supdem.stan.mod1.stan', data=dat.1, pars=pars.1, 
                   iter=n.iter, warmup=n.warm, chains=n.chn, thin=n.thin, 
                   control=list(adapt_delta=0.99, stepsize=0.005, max_treedepth=14))

# model 1 with item-country intercepts
stan.mod.2 = stan(file='supdem.stan.mod2.stan', data=dat.2, pars=pars.2, 
                   iter=n.iter, warmup=n.warm, chains=n.chn, thin=n.thin, 
                   control=list(adapt_delta=0.99, stepsize=0.005, max_treedepth=14))

# model 2 with item slopes
stan.mod.3 = stan(file='supdem.stan.mod3.stan', data=dat.2, pars=pars.3, 
                   iter=n.iter, warmup=n.warm, chains=n.chn, thin=n.thin, 
                   control=list(adapt_delta=0.99, stepsize=0.005, max_treedepth=15))

# beta-binomial model with item intercepts
stan.mod.4 = stan(file='supdem.stan.mod4.stan', data=dat.1, pars=pars.4, 
                   iter=n.iter, warmup=n.warm, chains=n.chn, thin=n.thin, 
                   control=list(adapt_delta=0.99, stepsize=0.03, max_treedepth=11))

# model 4 with item-country intercepts
stan.mod.5 = stan(file='supdem.stan.mod5.stan', data=dat.2, pars=pars.5, 
                   iter=n.iter, warmup=n.warm, chains=n.chn, thin=n.thin, 
                   control=list(adapt_delta=0.99, stepsize=0.02, max_treedepth=11))

# model 5 with item slopes
stan.mod.6 = stan(file='supdem.stan.mod6.stan', data=dat.2, pars=pars.6, 
                   iter=n.iter, warmup=n.warm, chains=n.chn, thin=n.thin, 
                   control=list(adapt_delta=0.99, stepsize=0.01, max_treedepth=13))


# save workspace
save.image("supdem_stan_workspace.RData")
write.csv(as.data.frame(summary(stan.mod.1)), "stan_mod1_summary.csv")
write.csv(as.data.frame(summary(stan.mod.2)), "stan_mod2_summary.csv")
write.csv(as.data.frame(summary(stan.mod.3)), "stan_mod3_summary.csv")
write.csv(as.data.frame(summary(stan.mod.4)), "stan_mod4_summary.csv")
write.csv(as.data.frame(summary(stan.mod.5)), "stan_mod5_summary.csv")
write.csv(as.data.frame(summary(stan.mod.6)), "stan_mod6_summary.csv")



## Evaluate model fit

# list of models
model.list = list(stan.mod.1, stan.mod.2, stan.mod.3, stan.mod.4, stan.mod.5, stan.mod.6)

# convergence
sapply(lapply(model.list, function(x) unlist(rstan::extract(x, pars="lp__"))), length)
sapply(lapply(model.list, function(x) summary(x)$summary[,"Rhat"]), function(x) sum(x >= 1.05, na.rm=TRUE))

# Traceplot (Figure S2)
pdf("figureS2.pdf", width=6, height=7.5)
rstan::traceplot(stan.mod.5, ncol=3, nrow=4, alpha=0.8, size=0.3,
          pars=c("mu_lambda","sigma_lambda","sigma_theta","sigma_delta","phi","lambda[15]",
                 "delta[107]","delta[344]","theta[21,66]","theta[24,12]","theta[16,49]","theta[16,101]"))
dev.off()


## Extract theta estimates

summary(stan.mod.5, pars=c("theta"))$summary[,"mean"][
  c(1, n.cntrys+1, 2*n.cntrys+1, 3*n.cntrys+1, 4*n.cntrys+1, 5*n.cntrys+1)]

theta.out = rstan::extract(stan.mod.5, pars = c("theta"))[[1]]
theta.std = (theta.out - mean(as.vector(theta.out))) / sd(as.vector(theta.out))
theta.out.t = apply( theta.std, 1, function(x) t(x) )
theta.out.df = data.frame(Country=rep(cnt.names, length.out=n.cntrys*25), 
                          COWCode=rep(cnt.ccode, length.out=n.cntrys*25),
                          Year=rep(1991:2015, each=n.cntrys), theta.out.t)
theta.pe = theta.out.df[,1:3]
theta.pe$theta = apply(theta.out.df[,4:n.iter+3], 1, mean)

first.yr = data.frame(Country=levels(sddem2$Country), First_yr = 
                        as.vector(by(sddem2, sddem2$Country, function(x) min(as.numeric(x$Year))+1991)))
theta.pe = merge(theta.pe, first.yr, by="Country", all.x=TRUE)



## Internal fit / validation (part of Table 3)

# set up table 3
tab3 = matrix(NA, nrow=10, ncol=6)
colnames(tab3) = c("MAE_int", "pc_improv_int", "LOO-IC", "MAE_ext", "pc_improv_ext", "CIC_ext")
row.names(tab3) = c("Mod1", "Mod2", "Mod3", "Mod4", "Mod5", "Mod6", "DGIRT", "cnt_means", "item_means", "grnd_means")

# loo-ic
llik = lapply(model.list, extract_log_lik)
(loo.list = sapply(llik, loo))
tab3[1:6,3] = round(unlist(loo.list[3,]))

# mae for each model
x.sim = sapply(model.list, rstan::extract, pars=c("x_pred"))
x.sim.pe = sapply(x.sim, function(x) apply(x, 2, mean, na.rm=TRUE)) 
x.sim.err = x.sim.pe
x.sim.err = apply(x.sim.pe, 2, function(x) sddem2$Response/sddem2$Sample - x/sddem2$Sample)
(mae = apply(x.sim.err, 2, function(x) sum(abs(x), na.rm=TRUE) / length(sddem2$Response)))
tab3[1:6,1] = round(mae, 3)

# compare to fit of baseline country means
sddem2$CountryMean = ave(sddem2$Response/sddem2$Sample, sddem2$Country)
cnt.mae = sum(abs(sddem2$CountryMean - sddem2$Response/sddem2$Sample), na.rm=TRUE) / length(sddem2$Response)
tab3[8,1] = round(cnt.mae, 3)
tab3[1:6,2] = round((cnt.mae - mae) / cnt.mae * 100, 1)

# compare to fit of baseline item means
sddem2$ItemMean = ave(sddem2$Response/sddem2$Sample, sddem2$ItemProj)
itm.mae = sum(abs(sddem2$ItemMean - sddem2$Response/sddem2$Sample), na.rm=TRUE) / length(sddem2$Response)
tab3[9,1] = round(itm.mae, 3)
tab3[9,2] = round((cnt.mae - itm.mae) / cnt.mae * 100, 1)
  
# compare to fit of baseline grand mean
grd.mae = sum(abs(mean(sddem2$Response/sddem2$Sample) - sddem2$Response/sddem2$Sample), na.rm=TRUE) / 
          length(sddem2$Response)
tab3[10,1] = round(grd.mae, 3)
tab3[10,2] = round((cnt.mae - grd.mae) / cnt.mae * 100, 1)

# save table 3 (this output includes only the internal validation part of table 3)
write.csv(tab3, file="Table3.csv")


## Item analysis (figure 4)

# examine item pars
item.names = levels(sddem2$ItemProj)
item.pars = data.frame(
  item_int=round(summary(stan.mod.6, pars="lambda")$summary[,"mean"], 3), 
  item_slope=round(summary(stan.mod.6, pars="gamm")$summary[,"mean"], 3))
rownames(item.pars) = item.names
item.pars[order(item.pars[,2]),]

# adjust intercepts and slopes to account for standardization of theta 
theta.out = rstan::extract(stan.mod.6, pars = c("theta"))[[1]]
theta.raw.mean = mean(as.vector(theta.out))
theta.raw.sd = sd(as.vector(theta.out))
item.pars.std = item.pars
# intercept (a): a^std = a + b*mu
# slope (b): b^std = b*sigma
item.pars.std[,1] = item.pars[,1] + (item.pars[,2] * theta.raw.mean)
item.pars.std[,2] = item.pars[,2] * theta.raw.sd
item.pars.std
arm::invlogit(item.pars.std)

# set up plot
plot.pal = c("#000000", "#000000", "#000000", "#838383", "#838383", "#838383", "#838383", "#838383", "#838383" )
plot.lty = c(1, 5, 4, 1, 5, 4, 4, 4, 4)
names.col = rep(9, length(item.names))
names.col[grep("Army", item.names)] = 1
names.col[grep("Church", item.names)] = 2
names.col[grep("EvDemoc", item.names)] = 3
names.col[grep("Strong", item.names)] = 4
names.col[grep("Three", item.names)] = 5
leg.names = c("Army rule", "Churchill", "Evaluate democracy", "Strong leader", "Three statements", "Other")
proj.names = c("AfroBarometer", "ArabBarometer", "AsiaBarometer", "AsianBarometer", "CSES", "WVS", "LAPOP", 
               "LatinoBarometer", "ND Barometer", "Pew", "ESS")
proj.inds = c(rep(1,4), rep(2,5), rep(3,3), rep(4,6), rep(5,1), rep(11,1), rep(7,4), rep(8,2), rep(9,3), 
              rep(10,3), rep(6,5))

# plot
pdf("figure4.pdf", width=6.5, height=5.5)
layout(matrix(1:12, nrow=3, ncol=4, byrow=TRUE), widths=c(1.2,1,1,1), heights=c(1,1,1.2))

# plot 1
par(mar=c(0.1, 3, 2, 0.5), cex=0.7)
curve(arm::invlogit(item.pars.std[proj.inds==1,1][1] + x*item.pars.std[1,2]), -4, 4, type="n", 
      xlab="", ylab="", ylim=c(0,1), yaxs="i", xaxs="i", xaxt="n", yaxt="n", bty="n")
abline(h=c(.25,.5,.75), lty=3, lwd=0.75, col=grey(0.5))
abline(v=c(-2,0,2), lty=3, lwd=0.75, col=grey(0.5))
for(j in 1:dim(item.pars.std[proj.inds==1,])[1]) {
  curve(arm::invlogit(item.pars.std[proj.inds==1,1][j] + x*item.pars.std[proj.inds==1,2][j]), 
        -4, 4, type="l", add=TRUE, lwd=1.75, col=plot.pal[names.col[proj.inds==1]][j], 
        lty=plot.lty[names.col[proj.inds==1]][j])
  }
mtext(proj.names[1], side=3, line=0.4, cex=0.75)
axis(2, at=c(.25, .5, .75), labels=c(".25", ".50", ".75"), mgp=c(1.5, .35, 0), las=1, 
     tcl=-0.2, cex.axis=0.78)
box(lty=1, lwd=0.75)

# plots 2-4  
for(i in 2:4) {
  par(mar=c(0.1, 0.8, 2, 0.5), cex=0.7)
  curve(arm::invlogit(item.pars.std[proj.inds==i,1][1] + x*item.pars.std[1,2]), -4, 4, type="n", 
        xlab="", ylab="", ylim=c(0,1), yaxs="i", xaxs="i", xaxt="n", yaxt="n", bty="n")
  abline(h=c(.25,.5,.75), lty=3, lwd=0.75, col=grey(0.5))
  abline(v=c(-2,0,2), lty=3, lwd=0.75, col=grey(0.5))
  for(j in 1:dim(item.pars.std[proj.inds==i,])[1]) {
    curve(arm::invlogit(item.pars.std[proj.inds==i,1][j] + x*item.pars.std[proj.inds==i,2][j]), 
          -4, 4, type="l", add=TRUE, lwd=1.75, col=plot.pal[names.col[proj.inds==i]][j], 
          lty=plot.lty[names.col[proj.inds==i]][j])
  }
  mtext(proj.names[i], side=3, line=0.3, cex=0.75)
  box(lty=1, lwd=0.75)
}

# plot 5
par(mar=c(0.1, 3, 2, 0.5), cex=0.7)
curve(arm::invlogit(item.pars.std[proj.inds==5,1][1] + x*item.pars.std[1,2]), -4, 4, type="n", 
      xlab="", ylab="", ylim=c(0,1), yaxs="i", xaxs="i", xaxt="n", yaxt="n", bty="n")
abline(h=c(.25,.5,.75), lty=3, lwd=0.75, col=grey(0.5))
abline(v=c(-2,0,2), lty=3, lwd=0.75, col=grey(0.5))
for(j in 1:dim(item.pars.std[proj.inds==5,])[1]) {
  curve(arm::invlogit(item.pars.std[proj.inds==5,1][j] + x*item.pars.std[proj.inds==5,2][j]), 
        -4, 4, type="l", add=TRUE, lwd=1.75, col=plot.pal[names.col[proj.inds==5]][j], 
        lty=plot.lty[names.col[proj.inds==5]][j])
  }
mtext(proj.names[5], side=3, line=0.3, cex=0.75)
axis(2, at=c(.25, .5, .75), labels=c(".25", ".50", ".75"), mgp=c(1.5, .35, 0), las=1, 
     tcl=-0.2, cex.axis=0.78)
mtext("Proportion supporting democracy", side=2, line=1.5, las=0, cex=0.75)
box(lty=1, lwd=0.75)

# plots 6-8  
for(i in 6:8) {
  par(mar=c(0.1, 0.8, 2, 0.5), cex=0.7)
  curve(arm::invlogit(item.pars.std[proj.inds==i,1][1] + x*item.pars.std[1,2]), -4, 4, type="n", 
        xlab="", ylab="", ylim=c(0,1), yaxs="i", xaxs="i", xaxt="n", yaxt="n", bty="n")
  abline(h=c(.25,.5,.75), lty=3, lwd=0.75, col=grey(0.5))
  abline(v=c(-2,0,2), lty=3, lwd=0.75, col=grey(0.5))
  for(j in 1:dim(item.pars.std[proj.inds==i,])[1]) {
    curve(arm::invlogit(item.pars.std[proj.inds==i,1][j] + x*item.pars.std[proj.inds==i,2][j]), 
          -4, 4, type="l", add=TRUE, lwd=1.75, col=plot.pal[names.col[proj.inds==i]][j], 
          lty=plot.lty[names.col[proj.inds==i]][j])
  }
  mtext(proj.names[i], side=3, line=0.3, cex=0.75)
  box(lty=1, lwd=0.75)
}

# plot 9
par(mar=c(2.5, 3, 2, 0.5), cex=0.7)
curve(arm::invlogit(item.pars.std[proj.inds==9,1][1] + x*item.pars.std[1,2]), -4, 4, type="n", 
      xlab="", ylab="", ylim=c(0,1), yaxs="i", xaxs="i", xaxt="n", yaxt="n", bty="n")
abline(h=c(.25,.5,.75), lty=3, lwd=0.75, col=grey(0.5))
abline(v=c(-2,0,2), lty=3, lwd=0.75, col=grey(0.5))
for(j in 1:dim(item.pars.std[proj.inds==9,])[1]) {
  curve(arm::invlogit(item.pars.std[proj.inds==9,1][j] + x*item.pars.std[proj.inds==9,2][j]), 
        -4, 4, type="l", add=TRUE, lwd=1.75, col=plot.pal[names.col[proj.inds==9]][j], 
        lty=plot.lty[names.col[proj.inds==9]][j])
}
axis(1, at=c(-2, 0, 2),  mgp=c(1, .05, 0), las=1, tcl=-0.2, cex.axis=0.78)
mtext(expression(theta), side=1, line=1.2, cex=0.75)
mtext(proj.names[9], side=3, line=0.3, cex=0.75)
axis(2, at=c(.25, .5, .75), labels=c(".25", ".50", ".75"), mgp=c(1.5, .35, 0), las=1, 
     tcl=-0.2, cex.axis=0.78)
box(lty=1, lwd=0.75)

# plots 10-11  
for(i in 10:11) {
  par(mar=c(2.5, 0.8, 2, 0.5), cex=0.7)
  curve(arm::invlogit(item.pars.std[proj.inds==i,1][1] + x*item.pars.std[1,2]), -4, 4, type="n", 
        xlab="", ylab="", ylim=c(0,1), yaxs="i", xaxs="i", xaxt="n", yaxt="n", bty="n")
  abline(h=c(.25,.5,.75), lty=3, lwd=0.75, col=grey(0.5))
  abline(v=c(-2,0,2), lty=3, lwd=0.75, col=grey(0.5))
  for(j in 1:dim(item.pars.std[proj.inds==i,])[1]) {
    curve(arm::invlogit(item.pars.std[proj.inds==i,1][j] + x*item.pars.std[proj.inds==i,2][j]), 
          -4, 4, type="l", add=TRUE, lwd=1.75, col=plot.pal[names.col[proj.inds==i]][j], 
          lty=plot.lty[names.col[proj.inds==i]][j])
  }
  axis(1, at=c(-2, 0, 2),  mgp=c(1, .05, 0), las=1, tcl=-0.2, cex.axis=0.78)
  mtext(expression(theta), side=1, line=1.2, cex=0.75)
  mtext(proj.names[i], side=3, line=0.3, cex=0.75)
  box(lty=1, lwd=0.75)
}

# plot 12
par(mar=c(2.5, 0.8, 2, 0.5), cex=0.7)
curve(arm::invlogit(item.pars.std[proj.inds==9,1][1] + x*item.pars.std[1,2]), -4, 4, type="n", 
      xlab="", ylab="", ylim=c(0,1), yaxs="i", xaxs="i", xaxt="n", yaxt="n", bty="n")
legend(x=-3.6, y=.8, legend=leg.names, lwd=1.75, col=plot.pal[c(1:5,9)], lty=c(1,5,4,1,5,4), bty="n", 
       cex=0.8, seg.len=3)
box(lty=1, lwd=0.75)

dev.off()


# identify least discriminatory items
item.names[order(invlogit(item.pars.std[,1] + (item.pars.std[,2]*-3)), decreasing=TRUE)]
item.names[order(invlogit(item.pars.std[,1] + (item.pars.std[,2]*3)), decreasing=TRUE)]

# identify most discriminatory items
item.names[order(invlogit(item.pars.std[,1] + (item.pars.std[,2]*2)) - 
                 invlogit(item.pars.std[,1] + (item.pars.std[,2]*-2)), decreasing=TRUE)]


## Plots posterior predicted fit (figure S1)

x.sim = rstan::extract(stan.mod.5, pars=c("x_pred"))[[1]]
x.sim.pe = apply(x.sim, 2, mean, na.rm=TRUE) 

pdf('figureS1.pdf', width=6, height=3)
par(mfrow=c(1,2), mar=c(2.5,3,.5,.5), mgp=c(1.4,.25,0), tcl=-0.2, cex=0.8)

plot(y=sddem2$Response, x=x.sim.pe, pch=1, col=rgb(0,0,0,.25), 
     xlab=expression(tilde(y)[ikt]), xaxs="i", yaxs="i",
     ylab=expression(y[ikt]), main="", cex.axis=0.85, cex.lab=1)
grid()
abline(a=0, b=1, lty=2, col="darkgrey")
points(y=sddem2$Response, x=x.sim.pe, pch=16, col=rgb(0,0,.55,.1))
lines(lowess(y=sddem2$Response, x=x.sim.pe), col=rgb(0,0,.55,1))

plot(y=sddem2$Response/sddem2$Sample, x=x.sim.pe/sddem2$Sample, pch=1, 
     col=rgb(0,0,0,.25), xlim=c(0,1), ylim=c(0,1), xaxs="i", yaxs="i",
     xlab=expression(tilde(y)[ikt]/s[ikt]), 
     ylab=expression(y[ikt]/s[ikt]), main="", cex.axis=0.85, cex.lab=1)
grid()
abline(a=0, b=1, lty=2, col="darkgrey")
points(y=sddem2$Response/sddem2$Sample, x=x.sim.pe/sddem2$Sample, pch=16, 
       col=rgb(0,0,.55,.1))
lines(lowess(y=sddem2$Response/sddem2$Sample, x=x.sim.pe/sddem2$Sample), 
      col=rgb(0,0,.55,1))

dev.off()



## Plot selected countries' theta estimates with uncertainty (Figure 3)

theta.out = rstan::extract(stan.mod.5, pars = c("theta"))[[1]]
dim(theta.out)
theta.std = (theta.out - mean(as.vector(theta.out))) / sd(theta.out)
theta.pe = apply(theta.std, c(2,3), mean)
colnames(theta.pe) = cnt.names
theta.samp.200 = theta.std[sample(1:dim(theta.std)[1], n.iter/5), , ]

# standardize data
sddem2$PropResp = sddem2$Response / sddem2$Sample
itm.means = as.vector(by(sddem2$PropResp, sddem2$ItemProj, mean))
sddem2$PropRespStd = NA
for(i in 1:length(itm.means)) {
  sddem2$PropRespStd[itm.prjs==i] = sddem2$PropResp[itm.prjs==i] - itm.means[i]
}
sddem2$PropRespStd = sddem2$PropRespStd / sd(sddem2$PropRespStd)

# select countries
plot.cnt = c('Sweden', 'Ukraine', 'Botswana', 'Venezuela',
             'United States of America', 'China', 'South Africa', 'Egypt')

# plot
pdf('figure3.pdf', width=6.5, height=8)
layout(matrix(1:8, nrow=4, ncol=2), widths=c(1.12, 1), heights=c(1, 1, 1, 1.12))
# plots 1-3
par(mar=c(0.5, 2.5, 1.8, 0.5), tcl=-0.2, las=1, cex=0.72)
for(i in 1:3) {
  cnt.raw = sddem2[sddem2$Country == plot.cnt[i],]
  cnt.raw$Year = as.numeric(cnt.raw$Year) + 1990
  plot(y=theta.pe[,plot.cnt[i]], x=1991:2015, type="n", xlab="", ylab="", main="", 
       ylim=c(-4,4), xaxt="n", yaxt="n", xaxs="i", bty="n")
  axis(side=2, at=c(-2,0,2), las=1, mgp=c(1,.4,0), cex.axis=0.85)
  abline(h=c(-2,2), col=grey(0.5), lwd=0.75, lty=3)
  abline(v=1992:2014, col=grey(0.5), lwd=0.75, lty=3)
  abline(h=0, lty=1, lwd=0.75, col=rgb(0,0,0,.4))
  for(j in 1:dim(theta.samp.200)[1]) {
    lines(y=theta.samp.200[j,,cnt.names==plot.cnt[i]], x=1991:2015, col=rgb(0,0,0,.1), lwd=0.75, lty=1)
  }
  mtext(plot.cnt[i], side=3, line=0.4, cex=0.8)
  mtext(expression(theta), side=2, line=1.3, cex=0.92)
  lines(y=theta.pe[,plot.cnt[i]], x=1991:2015, col=rgb(0,0,0,1), lwd=1.5, lty=1)
  points(x=cnt.raw$Year, y=cnt.raw$PropRespStd, col=rgb(0,0,0,.4), pch=19, cex=0.8)
  box(lty=1, lwd=0.75)
}

# plot 4 
par(mar=c(1.5, 2.5, 1.8, 0.5), tcl=-0.2, las=1, cex=0.72)
cnt.raw = sddem2[sddem2$Country == plot.cnt[4],]
cnt.raw$Year = as.numeric(cnt.raw$Year) + 1990
plot(y=theta.pe[,plot.cnt[4]], x=1991:2015, type="n", xlab="", ylab="", main="", 
     ylim=c(-4,4), xaxt="n", yaxt="n", xaxs="i", bty="n")
axis(side=1, at=c(1995,2000,2005,2010), mgp=c(1,.22,0), cex.axis=0.85)
axis(side=2, at=c(-2,0,2), las=1, mgp=c(1,.4,0), cex.axis=0.85)
abline(h=c(-2,2), col=grey(0.5), lwd=0.75, lty=3)
abline(v=1992:2014, col=grey(0.5), lwd=0.75, lty=3)
abline(h=0, lty=1, lwd=0.75, col=rgb(0,0,0,.4))
for(j in 1:dim(theta.samp.200)[1]) {
  lines(y=theta.samp.200[j,,cnt.names==plot.cnt[4]], x=1991:2015, col=rgb(0,0,0,.1), lwd=0.75, lty=1)
}
mtext(plot.cnt[4], side=3, line=0.4, cex=0.8)
mtext(expression(theta), side=2, line=1.3, cex=0.92)
lines(y=theta.pe[,plot.cnt[4]], x=1991:2015, col=rgb(0,0,0,1), lwd=1.5, lty=1)
points(x=cnt.raw$Year, y=cnt.raw$PropRespStd, col=rgb(0,0,0,.4), pch=19, cex=0.8)
box(lty=1, lwd=0.75)

# plots 5-7
par(mar=c(0.5, 0.5, 1.8, 0.5), tcl=-0.2, las=1, cex=0.72)
for(i in 5:7) {
  cnt.raw = sddem2[sddem2$Country == plot.cnt[i],]
  cnt.raw$Year = as.numeric(cnt.raw$Year) + 1990
  plot(y=theta.pe[,plot.cnt[i]], x=1991:2015, type="n", xlab="", ylab="", main="", 
       ylim=c(-4,4), xaxt="n", yaxt="n", xaxs="i", bty="n")
  abline(h=c(-2,2), col=grey(0.5), lwd=0.75, lty=3)
  abline(v=1992:2014, col=grey(0.5), lwd=0.75, lty=3)
  abline(h=0, lty=1, lwd=0.75, col=rgb(0,0,0,.4))
  for(j in 1:dim(theta.samp.200)[1]) {
    lines(y=theta.samp.200[j,,cnt.names==plot.cnt[i]], x=1991:2015, col=rgb(0,0,0,.1), lwd=0.75, lty=1)
  }
  mtext(plot.cnt[i], side=3, line=0.4, cex=0.8)
  lines(y=theta.pe[,plot.cnt[i]], x=1991:2015, col=rgb(0,0,0,1), lwd=1.5, lty=1)
  points(x=cnt.raw$Year, y=cnt.raw$PropRespStd, col=rgb(0,0,0,.4), pch=19, cex=0.8)
  box(lty=1, lwd=0.75)
}
# plot 8
par(mar=c(1.5, 0.5, 1.8, 0.5), tcl=-0.2, las=1, cex=0.72)
cnt.raw = sddem2[sddem2$Country == plot.cnt[8],]
cnt.raw$Year = as.numeric(cnt.raw$Year) + 1990
plot(y=theta.pe[,plot.cnt[8]], x=1991:2015, type="n", xlab="", ylab="", main="", 
     ylim=c(-4,4), xaxt="n", yaxt="n", xaxs="i", bty="n")
axis(side=1, at=c(1995,2000,2005,2010), mgp=c(1,.22,0), cex.axis=0.855)
abline(h=c(-2,2), col=grey(0.5), lwd=0.75, lty=3)
abline(v=1992:2014, col=grey(0.5), lwd=0.75, lty=3)
abline(h=0, lty=1, lwd=0.75, col=rgb(0,0,0,.4))
for(j in 1:dim(theta.samp.200)[1]) {
  lines(y=theta.samp.200[j,,cnt.names==plot.cnt[8]], x=1991:2015, col=rgb(0,0,0,.1), lwd=0.75, lty=1)
}
mtext(plot.cnt[8], side=3, line=0.4, cex=0.8)
lines(y=theta.pe[,plot.cnt[8]], x=1991:2015, col=rgb(0,0,0,1), lwd=1.5, lty=1)
points(x=cnt.raw$Year, y=cnt.raw$PropRespStd, col=rgb(0,0,0,.4), pch=19, cex=0.8)
box(lty=1, lwd=0.75)

dev.off()


## Plot all countries' theta estimates with uncertainty (Figures S3-S7)

theta.out = rstan::extract(stan.mod.5, pars = c("theta"))[[1]]
dim(theta.out)
theta.std = (theta.out - mean(as.vector(theta.out))) / sd(theta.out)
theta.pe = apply(theta.std, c(2,3), mean)
colnames(theta.pe) = cnt.names
theta.samp.200 = theta.std[sample(1:dim(theta.std)[1], n.iter/5), , ]

# first 4 plots
plot.max = 28
rnd.cnt = sample(1:dim(theta.samp.200)[3], plot.max)
for(k in 1:4) {
  plot.cntrys = ((k-1)*plot.max+1):(k*plot.max)
  pdf(paste0("figureS",2+k,".pdf"), width=17.5, height=10)
  par(mfrow=c(4,7), mar=c(2, 2, 2, 1), tcl=-0.2, cex=0.75)
  for (i in 1:plot.max) {
    plot(y=theta.pe[,plot.cntrys[i]], x=1991:2015, type="l", col=rgb(0,0,.55,1), lwd=2, lty=1,
         xlab="", ylab="", main=cnt.names[plot.cntrys[i]], ylim=c(-3.9,3.9),
         xaxt="n", yaxt="n", xaxs="i", bty="n")    
    axis(side=1, at=c(1995, 2000, 2005, 2010), cex.axis=.85, mgp=c(1.4,.2,0))
    axis(side=2, at=c(-2,0,2), las=1, cex.axis=.85, mgp=c(1.4,.4,0))    
    grid(col="darkgray", ny=4)
    abline(h=0, lty=1, col="darkgrey")
    lines(x=1991:2015, y=theta.pe[,plot.cntrys[i]], col=rgb(0,0,.55,1), lwd=2, lty=1)
    for(j in 1:dim(theta.samp.200)[1]){
      lines(y=theta.samp.200[j,,plot.cntrys[i]], x=1991:2015, col=rgb(0,0,.55,.1), lwd=1, lty=1)
    }
    box(lwd=0.75)
  }
  dev.off()
}

# final plot
plot.cntrys = ((5-1)*plot.max+1):dim(theta.pe)[2]
pdf(paste0("figureS7.pdf"), width=17.5, height=10)
par(mfrow=c(4,7), mar=c(2, 2, 2, 1), tcl=-0.2, cex=0.75)
for (i in 1:length(plot.cntrys)) {
  plot(y=theta.pe[,plot.cntrys[i]], x=1991:2015, type="l", col=rgb(0,0,.55,1), lwd=2, lty=1,
       xlab="", ylab="", main=cnt.names[plot.cntrys[i]], ylim=c(-3.9,3.9),
       xaxt="n", yaxt="n", xaxs="i", bty="n")
  axis(side=1, at=c(1995, 2000, 2005, 2010), cex.axis=.85, mgp=c(1.4,.2,0))
  axis(side=2, at=c(-2,0,2), las=1, cex.axis=.85, mgp=c(1.4,.4,0))
  grid(col="darkgray", ny=4)
  abline(h=0, lty=1, col="darkgrey")
  lines(x=1991:2015, y=theta.pe[,plot.cntrys[i]], col=rgb(0,0,.55,1), lwd=2, lty=1)
  for(j in 1:dim(theta.samp.200)[1]){
    lines(y=theta.samp.200[j,,plot.cntrys[i]], x=1991:2015, col=rgb(0,0,.55,.1), lwd=1, lty=1)
  }
  box(lwd=0.75)
}
dev.off()



## Plot indicative grid showing data availability (figure 1)

# plot 1: all items

pdf("figure1.pdf", width=6.5, height=3.5)
layout(matrix(1:4, nrow=2, ncol=2, byrow=TRUE), widths=c(1.4, 1), heights=c(1, 1.15))

plot.cnt.2 = c('Egypt', 'Venezuela', 'South Africa', 'Botswana', 'China', 'Ukraine', 
               'United States of America', 'Sweden')

plot.dat.2 = sddem2[sddem2$Country %in% plot.cnt.2,]
cnt.yr.tab = table(sddem2$Year, sddem2$Country)
cnt.yr.tab = ifelse(cnt.yr.tab > 0, 1, NA)
plot.dat.2 = rbind(cnt.yr.tab[,plot.cnt.2[1]], cnt.yr.tab[,plot.cnt.2[2]], cnt.yr.tab[,plot.cnt.2[3]],
                   cnt.yr.tab[,plot.cnt.2[4]], cnt.yr.tab[,plot.cnt.2[5]], cnt.yr.tab[,plot.cnt.2[6]],
                   cnt.yr.tab[,plot.cnt.2[7]], cnt.yr.tab[,plot.cnt.2[8]])
plot.dat.2 = plot.dat.2 * 1:8

par(mar=c(0.3, 7.5, 2, 0.5), tcl=-0.2, cex=0.8)
plot(plot.dat.2[1,], type="n", ylim=c(0.5, 8.5), xaxt="n", yaxt="n", xlab="", ylab="", 
     main='', xlim=c(1, 24), bty="n")
abline(h=1:8, col=grey(0.5), lwd=0.75, lty=3)
abline(v=1:25, col=grey(0.5), lwd=0.75, lty=3)
for(i in 1:8){
  points(plot.dat.2[i,], pch=16, cex=1.1, col='black')
}
axis(2, at=1:8, labels=plot.cnt.2, cex.axis=0.78, mgp=c(1, .5, 0), las=1)
mtext(text='All items', side=3, line=0.4, cex=0.85)
box(lwd=0.75)

# plot 2: only strongman item
cnt.yr.tab = table(sddem2[grep('Strong', sddem2$Item), "Year"], 
                   sddem2[grep('Strong', sddem2$Item), "Country"])
cnt.yr.tab = ifelse(cnt.yr.tab > 0, 1, NA)
plot.dat.2 = rbind(cnt.yr.tab[,plot.cnt.2[1]], cnt.yr.tab[,plot.cnt.2[2]], cnt.yr.tab[,plot.cnt.2[3]],
                   cnt.yr.tab[,plot.cnt.2[4]], cnt.yr.tab[,plot.cnt.2[5]], cnt.yr.tab[,plot.cnt.2[6]],
                   cnt.yr.tab[,plot.cnt.2[7]], cnt.yr.tab[,plot.cnt.2[8]])
plot.dat.2 = plot.dat.2 * 1:8

par(mar=c(0.3, 0.5, 2, 0.5), tcl=-0.2, cex=0.8)
plot(plot.dat.2[1,], type="p", ylim=c(0.5, 8.5), xaxt="n", yaxt="n", xlab="", ylab="", 
     main='', xlim=c(1, 24), bty="n")
abline(h=1:8, col=grey(0.5), lwd=0.75, lty=3)
abline(v=1:25, col=grey(0.5), lwd=0.75, lty=3)
for(i in 1:8){
  points(plot.dat.2[i,], pch=16, cex=1.1)
}
mtext(text='Strong leader items', side=3, line=0.4, cex=0.85)
box(lwd=0.75)

# plot 3: only three-statements item
cnt.yr.tab = table(sddem2[grep('ThreeState', sddem2$Item), "Year"], 
                   sddem2[grep('ThreeState', sddem2$Item), "Country"])
cnt.yr.tab = ifelse(cnt.yr.tab > 0, 1, NA)
plot.dat.2 = rbind(cnt.yr.tab[,plot.cnt.2[1]], cnt.yr.tab[,plot.cnt.2[2]], cnt.yr.tab[,plot.cnt.2[3]],
                   cnt.yr.tab[,plot.cnt.2[4]], cnt.yr.tab[,plot.cnt.2[5]], cnt.yr.tab[,plot.cnt.2[6]],
                   cnt.yr.tab[,plot.cnt.2[7]], cnt.yr.tab[,plot.cnt.2[8]])
plot.dat.2 = plot.dat.2 * 1:8

par(mar=c(2, 7.5, 1.8, 0.5), tcl=-0.2, cex=0.8)
plot(plot.dat.2[1,], type="p", ylim=c(0.5, 8.5), xaxt="n", yaxt="n", xlab="", ylab="", 
     main='', xlim=c(1, 24), bty="n")
abline(h=1:8, col=grey(0.5), lwd=0.75, lty=3)
abline(v=1:25, col=grey(0.5), lwd=0.75, lty=3)
for(i in 1:8){
  points(plot.dat.2[i,], pch=16, cex=1.1)
}
axis(1, at=c(4, 9, 14, 19, 24), labels=c(1995, 2000, 2005, 2010, 2015), cex.axis=0.75, mgp=c(1, 0.1, 0))
axis(2, at=1:8, labels=plot.cnt.2, cex.axis=0.78, mgp=c(1, .5, 0), las=1)
mtext(text='Three statements items', side=3, line=0.4, cex=0.85)
mtext(text='Year', side=1, line=1.1, cex=0.7)
box(lwd=0.75)

# plot 4: only army rule item
cnt.yr.tab = table(sddem2[grep('Army', sddem2$Item), "Year"], 
                   sddem2[grep('Army', sddem2$Item), "Country"])
cnt.yr.tab = ifelse(cnt.yr.tab > 0, 1, NA)
plot.dat.2 = rbind(cnt.yr.tab[,plot.cnt.2[1]], cnt.yr.tab[,plot.cnt.2[2]], cnt.yr.tab[,plot.cnt.2[3]],
                   cnt.yr.tab[,plot.cnt.2[4]], cnt.yr.tab[,plot.cnt.2[5]], cnt.yr.tab[,plot.cnt.2[6]],
                   cnt.yr.tab[,plot.cnt.2[7]], cnt.yr.tab[,plot.cnt.2[8]])
plot.dat.2 = plot.dat.2 * 1:8

par(mar=c(2, 0.5, 1.8, 0.5), tcl=-0.2, cex=0.8)
plot(plot.dat.2[1,], type="p", ylim=c(0.5, 8.5), xaxt="n", yaxt="n", xlab="", ylab="", 
     main='', xlim=c(1, 24), bty="n")
abline(h=1:8, col=grey(0.5), lwd=0.75, lty=3)
abline(v=1:25, col=grey(0.5), lwd=0.75, lty=3)
for(i in 1:8){
  points(plot.dat.2[i,], pch=16, cex=1.1)
}
axis(1, at=c(4, 9, 14, 19, 24), labels=c(1995, 2000, 2005, 2010, 2015), cex.axis=0.75, mgp=c(1, 0.1, 0))
mtext(text='Military rule items', side=3, line=0.4, cex=0.85)
mtext(text='Year', side=1, line=1.1, cex=0.7)
box(lwd=0.75)

dev.off()


## Table showing observed and estimated data for 3 countries, 5 years, all items (Table 2)

# set up table 2
tab2 = matrix(NA, ncol=8, nrow=39)
colnames(tab2) = c("ItemProj", "Country", "Year", "Cnt_lat_est", "Item_bias", "Itm_Cnt_bias", "Obs_prop", "Est_prop")
tab2 = data.frame(tab2)

length(unique(sddem2$ItemProjCnt))
length(levels(sddem2$ItemProjCnt))
sddem2$ItemProjCnt = factor(sddem2$ItemProjCnt)
length(levels(sddem2$ItemProjCnt))

sddem2$PropResp = sddem2$Response / sddem2$Sample
sddem2$SimResp5 = x.sim.pe / sddem2$Sample

tab2.dat = sddem2[sddem2$Country %in% c('United States of America', 'South Africa', 'China') & sddem2$Year %in% 15:19,
        c('ItemProj', 'ItemProjCnt', 'Country', 'Year', 'Response', 'Sample', 'PropResp', "PropRespStd", 'SimResp5')]
tab2.dat = tab2.dat[order(tab2.dat$Year, decreasing=FALSE),]
tab2.dat = tab2.dat[order(tab2.dat$Country, decreasing=TRUE),]

# create indices to extract lambdas and deltas
tab2.item.proj.inds = as.numeric(tab2.dat$ItemProj) 
tab2.item.proj.cnt.inds = as.numeric(tab2.dat$ItemProjCnt) 

tab2[c(2:7, 9:19, 21:24, 27:38), 1] = as.character(tab2.dat$ItemProj)
tab2[c(2:7, 9:19, 21:24, 27:38), 2] = as.character(tab2.dat$Country)
tab2[c(2:7, 9:19, 21:24, 27:38), 3] = as.numeric(tab2.dat$Year)
tab2[c(2:7, 9:19, 21:24, 27:38), 7] = round(tab2.dat$PropResp, 2)
tab2[c(2:7, 9:19, 21:24, 27:38), 8] = round(tab2.dat$SimResp5, 2)
tab2[c(1,8,20,25,26,39), 3] = c(14,16,16,18,14,18)
tab2[c(1,8,20,25,26,39), 2] = c("United States of America", "United States of America", "South Africa", "South Africa",
                                "China", "China")

# extract thetas
theta.out.5 = rstan::extract(stan.mod.5, pars = c("theta"))[[1]]
theta.std.5 = (theta.out.5 - mean(as.vector(theta.out.5))) / sd(theta.out.5)
theta.pe.5 = apply(theta.out.5, c(2,3), mean)
colnames(theta.pe.5) = cnt.names

for(i in 1:39) { tab2[i, "Cnt_lat_est"] = round(theta.pe.5[tab2$Year[i]+1, tab2$Country[i]], 2) }

# extract lambdas
lambda.out.5 = rstan::extract(stan.mod.5, pars = c("lambda"))[[1]]
lambda.pe.5 = apply(lambda.out.5, 2, mean)
tab2[c(2:7, 9:19, 21:24, 27:38), 5] = round(lambda.pe.5[tab2.item.proj.inds], 2)

# extract deltas
delta.out.5 = rstan::extract(stan.mod.5, pars = c("delta"))[[1]]
delta.pe.5 = apply(delta.out.5, 2, mean)
tab2[c(2:7, 9:19, 21:24, 27:38), 6] = round(delta.pe.5[tab2.item.proj.cnt.inds], 2)

# adjust years
tab2[, "Year"] = tab2[, "Year"] + 1991

# write table 2
write.csv(tab2, file="Table2.csv", row.names=FALSE)
