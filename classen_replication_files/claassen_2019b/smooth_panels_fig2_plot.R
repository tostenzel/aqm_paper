###### Estimating Estimating Smooth Country-Year Panels of Public Opinion
###### (c) Christopher Claassen 2018
###### Figure 2 plots

library(texreg)
library(ggplot2)
library(dplyr)

# data
dem = read.csv("Support_DemStock_05_15.csv")

# correlations between supdem and democracy
cor(dem[dem$Year==2015, c("SupDem", "DemStock")], use="pair")
cor(dem[dem$Year==2005, c("SupDem", "DemStock")], use="pair")

## Plots

pdf("figure2a.pdf", width=4.5, height=4.5)
p = ggplot(dem[dem$Year==2015,], aes(y=SupDem, x=DemStock)) +
  geom_text(aes(label=ISO_code), size=2.8, alpha=0.7) +
  geom_smooth(level=FALSE, size=0.75, colour="black") +
  geom_text(aes(x=4, y=2.1, label="r = .50"), size=4.5) +
  ylab("Support for democracy point estimates (2015)") + ggtitle("2015") +
  xlab("Cumulative democratic experience (V-Dem, 2015)") + theme_bw()
p
dev.off()

pdf("figure2b.pdf", width=4.5, height=4.5)
p = ggplot(dem[dem$Year==2005,], aes(y=SupDem, x=DemStock)) +
  geom_text(aes(label=ISO_code), size=2.8, alpha=0.7) +
  geom_smooth(level=FALSE, size=0.75, colour="black") +
  geom_text(aes(x=5, y=2, label="r = .57"), size=4.5) +
  ylab("Support for democracy point estimates (2005)") + ggtitle("2005") +
  xlab("Cumulative democratic experience (V-Dem, 2005)") + theme_bw()
p
dev.off()

