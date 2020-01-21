####################################################
rm(list=ls())
# Run this to install new version after pushing to github
load_or_install(c('devtools','roxygen2'))

####################################################
#%% Run this to document new functions and populate namespace
home = "~/Desktop/code/LalRUtils/"
setwd(home)
# create('LalRUtils') # only run once
document()

#%%
# reinstall after pushing updates
library(devtools)
devtools::install_github("apoorvalal/LalRUtils")
