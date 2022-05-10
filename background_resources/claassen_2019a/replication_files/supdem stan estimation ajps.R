#### Does Public Support Help Democracy Survive?
#### Christopher Claassen
#### AJPS Replication file: Stan Models for estimating support for democracy

# install.packages(c('MASS','arm','dplyr','tidyr','ggplot2','RColorBrewer','rstan','loo'))

library(MASS)
library(arm)
library(dplyr)
library(tidyr)
library(rstan)
library(loo)
library(ggplot2)
library(RColorBrewer)

# options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# working directory
WD = getwd()
setwd(WD)

# read support for dem data
sddem1 = read.csv("background_resources/claassen_2019a/replication_files/supdem-raw-survey-marginals.csv")

# remove NAs
sddem1 = sddem1[!sddem1$Response==0, ]

# create year by country indicators
year0 = 1987
sddem1 = sddem1[sddem1$Year > year0,]
sddem1 = unite(sddem1, YearCountry, c(Year, Country), sep = "_", remove = FALSE)

# create item by country indicators
sddem1 = unite(sddem1, ItemCnt, c(Item, Country), sep = "_", remove = FALSE)

# create year by project indicators
sddem1 = unite(sddem1, YrProj, c(Year, Project), sep = "_", remove = FALSE)

# create year by project by country indicators
sddem1 = unite(sddem1, YrProjCnt, c(YrProj, Country), sep = "_", remove = FALSE)

# factorise
sddem1$Country = as.factor(as.character(sddem1$Country))
sddem1$Item = as.factor(as.character(sddem1$Item))
sddem1$ItemCnt = as.factor(as.character(sddem1$ItemCnt))
sddem1$Project = as.factor(as.character(sddem1$Project))
sddem1$YrProj = as.factor(as.character(sddem1$YrProj))
sddem1$YrProjCnt = as.factor(as.character(sddem1$YrProjCnt))
sddem1$Year = sddem1$Year-year0

# count data
dim(sddem1)[1]                   # 3765 national responses
length(unique(sddem1$Country))   # 150 countries
length(unique(sddem1$Project))   # 14 projects
length(unique(sddem1$Item))      # 52 items
length(unique(sddem1$ItemCnt))   # 1453 item-country
length(unique(sddem1$YrProjCnt)) # 1390 national surveys
length(unique(sddem1$Year))      # 27 unique years (out of 30)
sum(sddem1$Sample) / dim(sddem1)[1] * length(unique(sddem1$YrProjCnt))   # 1,804,450 respondents              

# drop countries with less than 2 years of data
cnt.obs.years = rowSums(table(sddem1$Country, sddem1$Year) > 0)
sort(cnt.obs.years)
sddem2 = sddem1[sddem1$Country %in% levels(sddem1$Country)[cnt.obs.years > 1], ]
length(unique(sddem2$Country))   # 137 countries with 2+ years of data



#### Stan estimation


# prepare data for stan
n.items = length(unique(sddem2$Item))
n.cntrys = length(unique(sddem2$Country))
n.yrs = 2017-year0
n.proj = length(unique(sddem2$Project))
n.resp = dim(sddem2)[1]
n.itm.cnt = length(unique(sddem2$ItemCnt))
n.cntry.yrs = n.cntrys * n.yrs
n.yr.proj.cnt = length(unique(sddem2$YrProjCnt))
cntrys = as.numeric(factor(sddem2$Country))
cnt.names = as.character(sort(unique(sddem2$Country)))
cnt.code = as.character(sddem2[match(cnt.names, sddem2$Country), "CAbb"])
cnt.code[83] = "MNE"
cnt.code[106] = "SRB"
items = as.numeric(factor(sddem2$Item))
yrs = sddem2$Year
projs = as.numeric(factor(sddem2$Project))
itm.cnts = as.numeric(factor(sddem2$ItemCnt))

# specify data for stan
dat.1 = list(N=n.resp, K=n.items, T=n.yrs, J=n.cntrys, jj=cntrys, tt=yrs, kk=items, 
             x=sddem2$Response, samp=sddem2$Sample)
dat.2 = list(N=n.resp, K=n.items, T=n.yrs, J=n.cntrys, P=n.itm.cnt, jj=cntrys, tt=yrs, 
             pp=itm.cnts, kk=items, x=sddem2$Response, samp=sddem2$Sample)
sapply(dat.2, summary)

# pars
pars.4 = c("mu_lambda","sigma_lambda","sigma_theta","phi","lambda","theta","x_pred","log_lik")
pars.5 = c("mu_lambda","sigma_lambda","sigma_delta","sigma_theta","phi","lambda","delta","theta",
            "x_pred","log_lik")
pars.6 = c("Sigma","Omega","sigma_delta","sigma_theta","phi","lambda","gamm","delta","theta",
           "x_pred","log_lik")

# iterations
n.iter = 500
n.warm = 250
n.chn = 4
n.thin = 1

# best model from PA article (model 5)
stan.mod.5 = stan(file='supdem.stan.mod5.stan', data=dat.2, pars=pars.5, 
                   iter=n.iter, warmup=n.warm, chains=n.chn, thin=n.thin, 
                   control=list(adapt_delta=0.99, stepsize=0.02, max_treedepth=11))

# model 5 with different priors
stan.mod.5.2 = stan(file='supdem.stan.mod5.2.stan', data=dat.2, pars=pars.5, 
                  iter=n.iter, warmup=n.warm, chains=n.chn, thin=n.thin, 
                  control=list(adapt_delta=0.99, stepsize=0.02, max_treedepth=11))

# model 5 with item slopes
stan.mod.6 = stan(file='supdem.stan.mod6.2.stan', data=dat.2, pars=pars.6, 
                   iter=n.iter, warmup=n.warm, chains=n.chn, thin=n.thin, 
                   control=list(adapt_delta=0.99, stepsize=0.01, max_treedepth=13))


#### Model fit plots


# traceplot

pdf("figure_S2B.pdf", width=10, height=4)
rstan::traceplot(stan.mod.5, ncol=5, nrow=2, alpha=0.8, size=0.3, 
          pars=c("mu_lambda","sigma_lambda","sigma_theta","sigma_delta","phi","lambda[15]",
                 "delta[107]","theta[21,66]","theta[16,49]","theta[16,101]"))
dev.off()

# R-hat plot

pdf("figure_S2A.pdf", height=4, width=4)
stan_rhat(stan.mod.5)
dev.off()

# Parameter dotpots, mod 5 vs. mod 5.2 (prior perturbation)

summ5 = as.data.frame(summary(stan.mod.5))
summ52 = as.data.frame(summary(stan.mod.5.2))

par.list = c("mu_lambda", "sigma_lambda", "sigma_delta", "sigma_theta", "lambda[15]", "delta[107]",
             "delta[344]", "theta[21,66]", "theta[24,12]", "theta[16,49]", "theta[16,101]")
par.inds = row.names(summ5) %in% par.list


pdf("figure_S4.pdf", width=6, height=6)

layout(matrix(1:2, nrow=2, ncol=1, byrow=TRUE), widths=1, heights=c(6,1))
par(mar=c(1.5, 6, 0.5, 0.5), tcl=-0.2, las=1, cex=1, xaxs="r", yaxs="r", mgp=c(1,0.3,0))
plot(y=length(par.list):1, x=summ5[par.inds, "summary.mean"], xlim=c(-1, 1.5), type="n", ylab="", xlab="", main="",
     yaxt="n", cex.axis=0.9)
axis(side=2, at=length(par.list):1, lab=par.list, cex.axis=0.9, mgp=c(1,0.5,0))
abline(h=1:length(par.list), lty=1, lwd=30, col=grey(0.8))
segments(y0=length(par.list):1+0.15, x0=summ5[par.inds, "summary.2.5."], x1=summ5[par.inds, "summary.97.5."],
         col="black", lwd=1.5)
segments(y0=length(par.list):1-0.15, x0=summ52[par.inds, "summary.2.5."], x1=summ52[par.inds, "summary.97.5."],
         col=grey(0.3), lwd=1.5)
points(y=length(par.list):1+0.15, x=summ5[par.inds, "summary.mean"], pch=21, col="black", bg="black", cex=1)
points(y=length(par.list):1-0.15, x=summ52[par.inds, "summary.mean"], pch=21, col=grey(0.3), bg="white", cex=1)
legend(x=-1, y=11.1, legend=c("Model 5", "Model 5, alternative\npriors"), bg=rgb(1,1,1,0.5), box.col=rgb(1,1,1,0.5),
       col=c("black", grey(0.3)), pt.bg=c("black", "white"), pch=21, cex=0.9, pt.cex=1)
box()

par(mar=c(2.5, 6, 0.5, 0.5), tcl=-0.2, las=1, cex=1, xaxs="r", yaxs="r", mgp=c(1,0.3,0))
plot(y=1, x=summ5[5, "summary.mean"], xlim=c(55, 85), type="n", ylab="", xlab="", main="", yaxt="n", cex.axis=0.9)
axis(side=2, at=1, lab="phi", cex.axis=0.9, mgp=c(1,0.5,0))
mtext(side=1, line=1.2, at=70, "Parameter estimates", cex=1)
abline(h=1, lty=1, lwd=30, col=grey(0.8))
segments(y0=1+0.15, x0=summ5[5, "summary.2.5."], x1=summ5[5, "summary.97.5."], col="black", lwd=1.5)
segments(y0=1-0.15, x0=summ52[5, "summary.2.5."], x1=summ52[5, "summary.97.5."], col=grey(0.3), lwd=1.5)
points(y=1+0.15, x=summ5[5, "summary.mean"], pch=21, col="black", bg="black", cex=1)
points(y=1-0.15, x=summ52[5, "summary.mean"], pch=21, col=grey(0.3), bg="white", cex=1)
box()

dev.off()



#### Extract theta estimates

theta.out = rstan::extract(stan.mod.5, pars = c("theta"))[[1]]
theta.std = (theta.out - mean(as.vector(theta.out))) / sd(as.vector(theta.out)) # standardize
theta.out.t = apply( theta.std, 1, function(x) t(x) )
theta.out.df = data.frame(Country=rep(cnt.names, length.out=n.cntrys*30), 
                          ISO_code=rep(cnt.code, length.out=n.cntrys*30),
                          Year=rep(1988:2017, each=n.cntrys), theta.out.t)
theta.pe = theta.out.df[,1:3]
theta.dim = dim(theta.out.df)[2]
theta.pe$SupDem_trim = apply(theta.out.df[,4:theta.dim], 1, mean)

first.yr = data.frame(Country=levels(sddem2$Country),
                      First_yr = as.vector(by(sddem2, sddem2$Country, function(x) min(as.numeric(x$Year))+1987)))

theta.pe = merge(theta.pe, first.yr, by="Country", all.x=TRUE)
cnts = theta.pe[theta.pe$Year==2008, "Country"]
frst.yr = theta.pe[theta.pe$Year==2008, "First_yr"]
for(i in 1:length(cnts)) {
  theta.pe[theta.pe$Country==cnts[i] & theta.pe$Year<frst.yr[i], "SupDem_trim"] = NA
}

write.csv(theta.pe, "stan_est_sup_dem_m5.csv", row.names=FALSE)



#### Item analysis

# examine item pars
item.names = levels(sddem2$Item)
item.pars = data.frame(
  item_int=round(summary(stan.mod.6, pars="lambda")$summary[,"mean"], 3), 
  item_slope=round(summary(stan.mod.6, pars="gamm")$summary[,"mean"], 3))
rownames(item.pars) = item.names
item.pars$Proj = as.character(sddem2$Project[match(item.names, sddem2$Item)])

# adjust intercepts and slopes to account for standardization of theta 
theta.out = rstan::extract(stan.mod.6, pars = c("theta"))[[1]]
theta.raw.mean = mean(as.vector(theta.out))
theta.raw.sd = sd(as.vector(theta.out))
item.pars.std = item.pars

# intercept (a): a^std = a + b*mu
item.pars.std[,1] = item.pars[,1] + (item.pars[,2] * theta.raw.mean)

# slope (b): b^std = b*sigma
item.pars.std[,2] = item.pars[,2] * theta.raw.sd
item.pars.std
arm::invlogit(item.pars.std[,1:2])

# set up plot
plot.pal = RColorBrewer::brewer.pal(9, "Set1")
names.col = rep(9, length(item.names))
names.col[grep("Army", item.names)] = 1
names.col[grep("Church", item.names)] = 2
names.col[grep("EvDemoc", item.names)] = 3
names.col[grep("Strong", item.names)] = 4
names.col[grep("Three", item.names)] = 5
leg.names = c("Army rule", "Churchill question", "Evaluate democracy", "Strong leader", "Three statements", "Other")
proj.names = c("AfroBarom", "ArabBarom", "AsiaBarom", "AsianBarom", "CSES", "EuroBarom", "ESS", "EVS", "LAPOP", 
               "LatinoBarom", "NewDemocBarom", "Pew", "SouthAsiaBarom", "WVS")
item.pars.std$Col = c(rep(plot.pal[1],8), rep(plot.pal[2],7), rep(plot.pal[9],5), rep(plot.pal[3],5), rep(plot.pal[9],5), 
                      rep(plot.pal[4],13), rep(plot.pal[5],9))
proj.inds = as.numeric(as.factor(item.pars.std$Proj))


## plot

pdf("figure_S5.pdf", width=6.5, height=4)
layout(matrix(1:15, nrow=3, ncol=5, byrow=TRUE), widths=c(1.22,1,1,1), heights=c(1,1,1.3))

# plot 1
par(mar=c(0.1, 3, 2, 0.5), cex=0.7)
curve(arm::invlogit(item.pars.std[proj.inds==1,1][1] + x*item.pars.std[proj.inds==1,2][1]), -4, 4, 
      type="n", xlab="", ylab="", ylim=c(0,1), yaxs="i", xaxs="i", xaxt="n", yaxt="n", bty="n")
abline(h=c(.25,.5,.75), lty=3, lwd=0.75, col=grey(0.5))
abline(v=c(-2,0,2), lty=3, lwd=0.75, col=grey(0.5))
for(j in 1:dim(item.pars.std[proj.inds==1,])[1]) {
  curve(arm::invlogit(item.pars.std[proj.inds==1,1][j] + x*item.pars.std[proj.inds==1,2][j]), 
        -4, 4, type="l", add=TRUE, lwd=1.75, col=item.pars.std[proj.inds==1, "Col"][j])
  }
mtext(proj.names[1], side=3, line=0.4, cex=0.75)
axis(2, at=c(.25, .5, .75), labels=c(".25", ".50", ".75"), mgp=c(1.5, .35, 0), las=1, 
     tcl=-0.2, cex.axis=0.78)
box(lty=1, lwd=0.75)

# plots 2-5  
for(i in 2:5) {
  par(mar=c(0.1, 0.8, 2, 0.5), cex=0.7)
  curve(arm::invlogit(item.pars.std[proj.inds==i,1][1] + x*item.pars.std[1,2]), -4, 4, type="n", 
        xlab="", ylab="", ylim=c(0,1), yaxs="i", xaxs="i", xaxt="n", yaxt="n", bty="n")
  abline(h=c(.25,.5,.75), lty=3, lwd=0.75, col=grey(0.5))
  abline(v=c(-2,0,2), lty=3, lwd=0.75, col=grey(0.5))
  for(j in 1:dim(item.pars.std[proj.inds==i,])[1]) {
    curve(arm::invlogit(item.pars.std[proj.inds==i,1][j] + x*item.pars.std[proj.inds==i,2][j]), 
          -4, 4, type="l", add=TRUE, lwd=1.75, col=plot.pal[names.col[proj.inds==i]][j])
  }
  mtext(proj.names[i], side=3, line=0.3, cex=0.75)
  box(lty=1, lwd=0.75)
}

# plot 6
par(mar=c(0.1, 3, 2, 0.5), cex=0.7)
curve(arm::invlogit(item.pars.std[proj.inds==6,1][1] + x*item.pars.std[1,2]), -4, 4, type="n", 
      xlab="", ylab="", ylim=c(0,1), yaxs="i", xaxs="i", xaxt="n", yaxt="n", bty="n")
abline(h=c(.25,.5,.75), lty=3, lwd=0.75, col=grey(0.5))
abline(v=c(-2,0,2), lty=3, lwd=0.75, col=grey(0.5))
for(j in 1:dim(item.pars.std[proj.inds==5,])[1]) {
  curve(arm::invlogit(item.pars.std[proj.inds==5,1][j] + x*item.pars.std[proj.inds==6,2][j]), 
        -4, 4, type="l", add=TRUE, lwd=1.75, col=item.pars.std[proj.inds==6, "Col"][j])
  }
mtext(proj.names[6], side=3, line=0.3, cex=0.75)
axis(2, at=c(.25, .5, .75), labels=c(".25", ".50", ".75"), mgp=c(1.5, .35, 0), las=1, 
     tcl=-0.2, cex.axis=0.78)
mtext("Proportion supporting democracy", side=2, line=1.5, las=0, cex=0.75)
box(lty=1, lwd=0.75)

# plots 7-10  
for(i in 7:10) {
  par(mar=c(0.1, 0.8, 2, 0.5), cex=0.7)
  curve(arm::invlogit(item.pars.std[proj.inds==i,1][1] + x*item.pars.std[1,2]), -4, 4, type="n", 
        xlab="", ylab="", ylim=c(0,1), yaxs="i", xaxs="i", xaxt="n", yaxt="n", bty="n")
  abline(h=c(.25,.5,.75), lty=3, lwd=0.75, col=grey(0.5))
  abline(v=c(-2,0,2), lty=3, lwd=0.75, col=grey(0.5))
  for(j in 1:dim(item.pars.std[proj.inds==i,])[1]) {
    curve(arm::invlogit(item.pars.std[proj.inds==i,1][j] + x*item.pars.std[proj.inds==i,2][j]), 
          -4, 4, type="l", add=TRUE, lwd=1.75, col=plot.pal[names.col[proj.inds==i]][j])
  }
  mtext(proj.names[i], side=3, line=0.3, cex=0.75)
  box(lty=1, lwd=0.75)
}

# plot 11
par(mar=c(2.5, 3, 2, 0.5), cex=0.7)
curve(arm::invlogit(item.pars.std[proj.inds==11,1][1] + x*item.pars.std[1,2]), -4, 4, type="n", 
      xlab="", ylab="", ylim=c(0,1), yaxs="i", xaxs="i", xaxt="n", yaxt="n", bty="n")
abline(h=c(.25,.5,.75), lty=3, lwd=0.75, col=grey(0.5))
abline(v=c(-2,0,2), lty=3, lwd=0.75, col=grey(0.5))
for(j in 1:dim(item.pars.std[proj.inds==9,])[1]) {
  curve(arm::invlogit(item.pars.std[proj.inds==9,1][j] + x*item.pars.std[proj.inds==11,2][j]), 
        -4, 4, type="l", add=TRUE, lwd=1.75, col=item.pars.std[proj.inds==11, "Col"][j])
}
axis(1, at=c(-2, 0, 2),  mgp=c(1, .05, 0), las=1, tcl=-0.2, cex.axis=0.78)
mtext(expression(theta), side=1, line=1.2, cex=0.75)
mtext(proj.names[11], side=3, line=0.3, cex=0.75)
axis(2, at=c(.25, .5, .75), labels=c(".25", ".50", ".75"), mgp=c(1.5, .35, 0), las=1, 
     tcl=-0.2, cex.axis=0.78)
box(lty=1, lwd=0.75)

# plots 12-14  
for(i in 12:14) {
  par(mar=c(2.5, 0.8, 2, 0.5), cex=0.7)
  curve(arm::invlogit(item.pars.std[proj.inds==i,1][1] + x*item.pars.std[1,2]), -4, 4, type="n", 
        xlab="", ylab="", ylim=c(0,1), yaxs="i", xaxs="i", xaxt="n", yaxt="n", bty="n")
  abline(h=c(.25,.5,.75), lty=3, lwd=0.75, col=grey(0.5))
  abline(v=c(-2,0,2), lty=3, lwd=0.75, col=grey(0.5))
  for(j in 1:dim(item.pars.std[proj.inds==i,])[1]) {
    curve(arm::invlogit(item.pars.std[proj.inds==i,1][j] + x*item.pars.std[proj.inds==i,2][j]), 
          -4, 4, type="l", add=TRUE, lwd=1.75, col=plot.pal[names.col[proj.inds==i]][j])
  }
  axis(1, at=c(-2, 0, 2),  mgp=c(1, .05, 0), las=1, tcl=-0.2, cex.axis=0.78)
  mtext(expression(theta), side=1, line=1.2, cex=0.75)
  mtext(proj.names[i], side=3, line=0.3, cex=0.75)
  box(lty=1, lwd=0.75)
}

# plot 15
par(mar=c(2.5, 0.8, 2, 0.5), cex=0.7)
curve(arm::invlogit(item.pars.std[proj.inds==9,1][1] + x*item.pars.std[1,2]), -4, 4, type="n", 
      xlab="", ylab="", ylim=c(0,1), yaxs="i", xaxs="i", xaxt="n", yaxt="n", bty="n")
legend(x=-4.2, y=0.9, legend=leg.names, lwd=1.75, col=plot.pal[c(1:5,9)], bty="n", cex=0.75)
box(lty=1, lwd=0.75)

dev.off()




#### Plots of model fit


x.sim = rstan::extract(stan.mod.5, pars=c("x_pred"))[[1]]
x.sim.pe = apply(x.sim, 2, mean, na.rm=TRUE) / sddem2$Sample
x.sim.err = sddem2$Response/sddem2$Sample - x.sim.pe

## plot posterior predicted fit 

pdf('figure_S3A.pdf', width=5, height=5)
par(mfrow=c(1,1), mar=c(2.5,3,.5,.5), tcl=-0.2, las=1)
plot(y=sddem2$Response, x=x.sim.pe*sddem2$Sample, type="n", xlab="", ylab="", axes=FALSE, xaxs="i", yaxs="i", main="", 
     xlim=c(0,3600), ylim=c(0,3600))
abline(a=0, b=1, lty=2, col="black")
axis(side=1, tick=TRUE, mgp=c(1,0.15,0), cex.axis=0.8)
axis(side=2, tick=TRUE, mgp=c(1,0.4,0), cex.axis=0.8)
grid()
points(y=sddem2$Response, x=x.sim.pe*sddem2$Sample, pch=16, cex=0.6, col=rgb(0,0,0,.3))
lines(lowess(y=sddem2$Response, x=x.sim.pe*sddem2$Sample), col=rgb(0,0,0.5,1), lwd=2)
mtext(side=1, line=1.2, at=1800, "Observed response counts", cex=1)
mtext(side=2, line=2, at=1800, "Simulated response counts", cex=1, las=0)
box()
dev.off()

pdf('figure_S3B.pdf', width=5, height=5)
par(mfrow=c(1,1), mar=c(2.5,3,.5,.5), tcl=-0.2, las=1)
plot(y=sddem2$Response/sddem2$Sample, x=x.sim.pe, type="n", xlab="", ylab="", axes=FALSE, xaxs="i", yaxs="i", main="", 
     xlim=c(0,1), ylim=c(0,1))
abline(a=0, b=1, lty=2, col="black")
axis(side=1, tick=TRUE, mgp=c(1,0.15,0), cex.axis=0.8)
axis(side=2, tick=TRUE, mgp=c(1,0.4,0), cex.axis=0.8)
grid()
points(y=sddem2$Response/sddem2$Sample, x=x.sim.pe, pch=16, cex=0.6, col=rgb(0,0,0,.3))
lines(lowess(y=sddem2$Response/sddem2$Sample, x=x.sim.pe), col=rgb(0,0,0.5,1), lwd=2)
mtext(side=1, line=1.2, at=0.5, "Observed response proportions", cex=1)
mtext(side=2, line=2, at=0.5, "Simulated response proportions", cex=1, las=0)
box()
dev.off()

pdf("figure_S3C.pdf", width=5, height=5)
par(mfrow=c(1,1), mar=c(2.5, 2, 1, .5), tcl=-0.2, las=1)
plot(density(sddem2$Response/sddem2$Sample), type="n", axes=FALSE, xlim=c(0,1), ylim=c(0,3), xaxs="i", yaxs="i", main="")
axis(side=1, tick=TRUE, mgp=c(1,0.15,0), cex.axis=0.8)
axis(side=2, tick=TRUE, mgp=c(1,0.4,0), cex.axis=0.8)
grid()
lines(density(sddem2$Response/sddem2$Sample), lwd=3, col="black")
polygon(density(sddem2$Response/sddem2$Sample), col=rgb(0,0,0,0.7))
lines(density(x.sim.pe), lwd=3, col=rgb(0,0,.5,1))
polygon(density(x.sim.pe), col=rgb(0,0,.5,.4))
mtext(side=1, line=1.2, at=0.5, "Response proportions", cex=1)
legend("topleft", lwd=3, col=c(rgb(0,0,0,1), rgb(0,0,.5,1)), c("Observed", "Simulated"), cex=0.9, bty="n")
box()
dev.off()



