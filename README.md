# Final Paper with Reproduction Files for AQM 2022

The pdf is ``/StenzelFinalPaper/00-main.pdf``.

## Paper

The paper is written in R Markdown. All required files are in ``/StenzelFinalPaper``.
There are two possibilities to look at the code. In both cases run the .Rmd-files that contain R code in increasing order starting from ``00-main.Rmd`` to load all necessary variables. If you don't want to run the STAN code that may take days to complete skip the three cells in ``04-analysis-and-results.Rmd`` that contain the ``eval=FALSE`` setting and are titled starting by "generate-data-...". Make sure to also comment out the ``"rstan"`` package in ``00-main.Rmd`` if you don't want to install it. In case you want to install it follow [these instructions](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started). Note that you need to setup a C++ toolchain for R.

## Replication Files for Claassen's Papers

The papers and the replication files from the dataverse are contained in ``/claasen_replication_files``.


