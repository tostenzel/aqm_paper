Files included in replication materials for "Does Public Support Help Democracy Survive", AJPS
----------------------------------------------------------------------------------------------------------------------------

Software


Analyses were carried out using R version 3.5.3 for Windows (x86_64-w64-mingw32/x64 (64-bit))


Data /  CSV files


Support_democracy_ajps.csv  -  Panel (country-year) data necessary to reproduce results in paper. Does not require re-estimating support and satisfaction.

satisdem raw survey marginals.csv  -  Raw survey marginals, for estimating smooth panel of satisfaction with democracy (necessary only for results in supplementary materials)

supdem raw survey marginals.csv  -  Raw survey marginals, for estimating smooth panel of support for democracy (necessary only for results in supplementary materials)

vdem15.csv  -  Three democracy measures (V-Dem liberal democracy; combined Freedom House; Polity IV democracy-autocracy) for all countries in 2015 (necessary only for results in supplementary materials).


Codebooks


codebook support_democracy_ajps.pdf

codebook satisdem raw survey marginals.pdf 

codebook supdem raw survey marginals.pdf

codebook vdem15.pdf


R code files


supdem democracy ajps replication.R  -  R code necessary to  reproduce results in paper and most results in supplementary materials

satisdem stan estimation ajps.R  -  R code to estimate satisfaction with democracy smooth panel and certain results in supplementary materials

supdem stan estimation ajps.R  -  R code to estimate support for democracy smooth panel and certain results in supplementary materials

data merge ajps.R  -  R code to merge estimated support for and satisfaction with democracy with other country-year panel data (optional)


Stan files


stan.satisdem.mod.2.stan  -  stan code, latent variable model estimating for estimating satisfaction with democracy (called when satisdem stan estimation ajps.R file is run)
 
supdem.stan.mod5.2.stan  -  stan code, latent variable model estimating for estimating support for democracy (called when supdem stan estimation ajps.R file is run)

supdem.stan.mod5.stan  -  stan code, latent variable model estimating for estimating support for democracy (called when supdem stan estimation ajps.R file is run)

supdem.stan.mod6.2.stan  -  stan code, latent variable model estimating for estimating support for democracy (called when supdem stan estimation ajps.R file is run)


