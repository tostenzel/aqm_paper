#### Does Public Support Help Democracy Survive?
#### Christopher Claassen
#### AJPS Replication file: Stan models for estimating satisfaction with democracy

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

# read satisfaction with dem data
satis = read.csv("satisdem raw survey marginals.csv")

# create year by country indicators
min(satis$Year) # 1973
year0 = min(satis$Year) - 1
satis1 = satis[satis$Year > year0,]
satis1 = unite(satis1, YearCountry, c(Year, Country), sep = "_", remove = FALSE)
satis1$YearCountry = as.factor(satis1$YearCountry)

# create project by country indicators
satis1 = unite(satis1, ProjCnt, c(Project, Country), sep = "_", remove = FALSE)
satis1$ProjCnt = as.factor(satis1$ProjCnt)

# create year by project indicators
satis1 = unite(satis1, YrProjCnt, c(Year, Project, Country), sep = "_", remove = FALSE)
satis1$YrProjCnt = as.factor(satis1$YrProjCnt)

# factorise
satis1$Country = as.factor(as.character(satis1$Country))
satis1$Year = as.factor(satis1$Year-year0)

# count data in original data
length(unique(satis1$Country)) # 143 countries
length(unique(satis1$Year)) # 43 years
length(unique(satis1$YearCountry)) # 1621 country-years
length(unique(satis1$Project)) # 14 project-items
length(unique(satis1$YrProjCnt)) # 2013 national surveys

# drop countries with less than 2 years of data
cnt.obs.years = rowSums(table(satis1$Country, satis1$Year) > 0)
satis2 = satis1[satis1$Country %in% levels(satis1$Country)[cnt.obs.years > 1], ]

# count data in trimmed
length(unique(satis2$Country)) # 132 countries
length(unique(satis2$Year)) # 43 years
length(unique(satis2$YearCountry)) # 1610 country-years
length(unique(satis2$Project)) # 14 projects



#### Stan estimation


# prepare data for stan

n.cntrys = length(unique(satis2$Country))
n.yrs = 2017-year0
n.proj = length(unique(satis2$Project))
n.resp = dim(satis2)[1]
n.cntry.yrs = n.cntrys * n.yrs
n.cntry.proj = length(unique(satis2$ProjCnt))
cntrys = as.numeric(factor(satis2$Country))
cnt.names = sort(unique(satis2$Country))
cnt.ccode = satis2[match(cnt.names, satis2$Country), "COWCode"]
yrs = as.numeric(satis2$Year)
projs = as.numeric(factor(satis2$Project))
cntry.yrs = as.numeric(satis2$YearCountry)
satis2$ProjCnt = as.factor(as.character(satis2$ProjCnt))
cntry.prjs = as.numeric(satis2$ProjCnt)

# specify data for stan

dat.1 = list(N=n.resp, K=n.proj, T=n.yrs, J=n.cntrys, P=n.cntry.proj, jj=cntrys, tt=yrs, kk=projs, 
             pp=cntry.prjs, x=satis2$Response, samp=satis2$Sample)
lapply(dat.1, summary)

# pars

pars.1 = c("sigma_lambda","sigma_theta","sigma_delta","phi","lambda","delta","theta",
           "x_pred","log_lik")

# run model

n.iter = 500
n.warm = 250
n.chn = 4
n.thin = 1

stan.mod.2 = stan(file='stan.satisdem.mod.2.stan', data=dat.1, pars=pars.1, 
                  iter=n.iter, warmup=n.warm, chains=n.chn, thin=n.thin, 
                  control=list(adapt_delta=0.99, stepsize=0.01, max_treedepth=13))


## Evaluate model fit

pdf("figure_S7B.pdf", width=8, height=5)
rstan::traceplot(stan.mod.2, ncol=4, nrow=2, alpha=0.8, size=0.3,
                 pars=c("sigma_theta","sigma_delta","sigma_lambda","phi",
                        "lambda[2]","delta[44]","theta[14,57]","theta[22,87]"))
dev.off()

pdf("figure_S7A.pdf", width=6, height=4)
stan_rhat(stan.mod.2)
dev.off()


## Extract satisfaction with democracy estimates

theta.out = rstan::extract(stan.mod.2, pars = c("theta"))[[1]]

# standardize
theta.std = (theta.out - mean(as.vector(theta.out))) / sd(as.vector(theta.out)) 
theta.out.t = apply( theta.std, 1, function(x) t(x) )
theta.out.df = data.frame(Country=rep(cnt.names, length.out=n.cntrys*n.yrs), 
                          Year=rep(1973:2017, each=n.cntrys), theta.out.t)
theta.pe = theta.out.df[,1:2]
theta.pe$Satis_trim = apply(theta.out.df[,3:dim(theta.out.df)[2]], 1, mean)

# add variable to indicate first year survey data were available for that country
first.yr = data.frame(Country=levels(satis2$Country), 
                      First_yr = as.vector(by(satis2, satis2$Country, function(x) min(as.numeric(x$Year))+year0)))
theta.pe = merge(theta.pe, first.yr, by="Country", all.x=TRUE)
cnts = theta.pe[theta.pe$Year==2008, "Country"]
frst.yr = theta.pe[theta.pe$Year==2008, "First_yr"]
for(i in 1:length(cnts)) {
  theta.pe[theta.pe$Country==cnts[i] & theta.pe$Year<frst.yr[i], "Satis_trim"] = NA
}

write.csv(theta.pe, "stan_est_satis_dem.csv", row.names=FALSE)





