This folder contains replication materials for:

Christopher Claassen. 2018. "Estimating Smooth Panels of Public Opinion". Political Analysis



Data Files:

Support for democracy.csv		

CSV data file with numbers of respondents offering support to democracy by country-year (rows) and project-items (columns). Total sample sizes for each survey are in the final 11 columns


Support_DemStock_05_15.csv

CSV data file with latent estimates of support for democracy and cumulative experience with democracy (sum of V-Dem liberal democracy scores from 1950 until present day, discounted each year by 2%) for the years 2005 and 2015, and for all countries. Used to create figure 2.


R Code Files:

smooth panels stan estimation.R

R code which calls Stan, runs the 6 models using Support for democracy data, conducts tests of internal validation, and produces table 2, part of table 3, and figures 1, 3, 4 and S1 - S7. This file took approx. 12 hours to run using a desktop PC with a quad-core Intel i7 processor and 16 GB of RAM


smooth panels external validation.R

R code which splits the support for democracy data into training and test sets, calls Stan, runs the 6 models, conducts tests of external validation, and produces the external validation part of table 3. This file took approx. 10 hours to run using a desktop PC with a quad-core Intel i7 processor and 16 GB of RAM.


smooth panels dgo estimation.R

R code which splits the support for democracy data into training and test sets, uses functions from the dgo package to apply Caughey and Warshaws' dgirt model to the training set, and conducts tests of external validation.
This file took approx. 56 hours to run using a desktop PC with a quad-core Intel i7 processor and 16 GB of RAM. 


smooth panels fig2 plot.R

R code for producing figure 2


Note: to replicate the results of table 3, one must run the 1st, 2nd and 3rd R code files (above) in that order. 



Stan code files:

supdem.stan.mod1.stan
supdem.stan.mod2.stan
supdem.stan.mod3.stan
supdem.stan.mod4.stan
supdem.stan.mod5.stan
supdem.stan.mod6.stan

Stan code for the 6 models described in the paper. For use when applied to the full dataset.


supdem.stan.mod1.val.stan
supdem.stan.mod2.val.stan
supdem.stan.mod3.val.stan
supdem.stan.mod4.val.stan
supdem.stan.mod5.val.stan
supdem.stan.mod6.val.stan

Stan code for the 6 models, when applied to the training dataset 


System requirements:

The R code files were run on a desktop PC with a quad-core Intel i7 processor, 16 GB of RAM, and running Windows 7. 

The following R packages were loaded: countrycode, MASS, psych, arm, dplyr, tidyr, rstan, loo, rstanarm, ggplot2, RColorBrewer, devtools, dgo, texreg.

rstan requires that Stan be installed. See here: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
