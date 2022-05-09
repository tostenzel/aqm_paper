#### Does Public Support Help Democracy Survive?
#### Christopher Claassen
#### AJPS Replication file: constructing main dataset using own esimates of support and satisfaction

#### Note: this file merges estimates of democratic support and satisfaction with the other 
#### country-year covariates. It is an optional step. Estimates of support and satisfaction are already 
#### included in the dataset Support_democracy_ajps.csv


# Working directory
WD = getwd()
setwd(WD)

# Get data
sd.panel = read.csv("Support_democracy_ajps.csv")
sup.est = read.csv("stan_est_sup_dem_m5.csv")
sat.est = read.csv("stan_est_satis_dem.csv")

# Trim data
sd.panel = subset(sd.panel, select = -c(SupDem_trim, SupDem_Democ, SupDem_Autoc, 
                                        Satis_trim, Satis_Democ, Satis_Autoc))
sup.est = sup.est[, c("Country", "Year", "SupDem_trim")]
sat.est = sat.est[, c("Country", "Year", "Satis_trim")]

# Merge data
sd1 = merge(sd.panel, sup.est, by=c("Country", "Year"))
sd2 = merge(sd1, sat.est, by=c("Country", "Year"))

# Create democ vs autoc indicators for support and satisfaction
regime = ifelse(sd2$Regime_VD > 1, 1, 0)
sd2$SupDem_Democ = regime * sd2$SupDem_trim
sd2$SupDem_Autoc = (1-regime) * sd2$SupDem_trim
sd2$Satis_Democ = regime * sd2$Satis_trim
sd2$Satis_Autoc = (1-regime) * sd2$Satis_trim

write.csv(sd2, "Support_democracy_ajps.csv", row.names=FALSE)
