####################################################
rm(list=ls())
require(LalRUtils)
library(devtools)
# devtools::install_github("apoorvalal/LalRUtils")
# load_or_install(c('devtools','roxygen2'))
####################################################
#%%
# home = "C:/Users/alal/Desktop/code/LalRUtils/"
home = "~/Desktop/code/LalRUtils/"
setwd(paste0(home))
# create('LalRUtils')
document()
