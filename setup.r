####################################################
rm(list=ls())
# Run this to install new version after pushing to github
library(devtools)
devtools::install_github("apoorvalal/LalRUtils")

load_or_install(c('devtools','roxygen2'))
####################################################
#%% Run this to document new functions and populate namespace
# home = "C:/Users/alal/Desktop/code/LalRUtils/"
home = "~/Desktop/code/LalRUtils/"
setwd(paste0(home))
# create('LalRUtils')
document()
