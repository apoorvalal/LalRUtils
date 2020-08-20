####################################################
rm(list=ls())
# library(LalRUtils)
library(devtools); library(roxygen2)
####################################################
# %% Run this to document new functions and populate namespace
home = "~/Desktop/code/LalRUtils/"
setwd(home)
# create('LalRUtils') # only run once
document()
# %%
library(usethis)
library(pkgdown)
pkgdown::build_site()
# commit and push changes here
# %%
# reinstall after pushing updates
# Run this to install new version after pushing to github
library(devtools)
devtools::install_github("apoorvalal/LalRUtils")
library(LalRUtils)
