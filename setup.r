####################################################
# library(LalRUtils)
library(devtools); library(roxygen2)
####################################################
# %% Run this to document new functions and populate namespace
# setwd(root)
# root = "/home/alal/Desktop/code/LalRUtils/"
# create('LalRUtils') # only run once
document()
# %% Run this for local testing
install.packages(getwd(), repos = NULL, type = 'source')

# %%
# reinstall from remote after pushing updates
# Run this to install new version after pushing to github
# library(devtools)
# devtools::install_github("apoorvalal/LalRUtils")
# library(LalRUtils)
# %%
