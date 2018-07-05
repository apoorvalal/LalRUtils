####################################################
rm(list=ls())
require(LalRUtils)
# library(devtools)
devtools::install_github("apoorvalal/LalRUtils")
load_or_install(c('devtools','roxygen2'))
####################################################
#%%
home = "/home/alal/Desktop/code/"
setwd(paste0(home,'LalRUtils'))
# create('LalRUtils')
document()
