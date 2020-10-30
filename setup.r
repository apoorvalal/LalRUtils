####################################################
rm(list=ls())
# library(LalRUtils)
library(devtools); library(roxygen2)

root = "/home/alal/Desktop/code/LalRUtils/"
####################################################
# %% Run this to document new functions and populate namespace
setwd(root)
# create('LalRUtils') # only run once
document()

# %% Run this for local testing
install.packages(root, repos = NULL, type = 'source')

# %% website documentation

library(usethis); library(pkgdown)
pkgdown::build_site()
# commit and push changes here
# %%
# reinstall after pushing updates
# Run this to install new version after pushing to github
library(devtools)
devtools::install_github("apoorvalal/LalRUtils")
library(LalRUtils)
