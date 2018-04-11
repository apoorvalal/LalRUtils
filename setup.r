####################################################
rm(list=ls())
if (require('LalRUtils')==F) {
    library(devtools)
    devtools::install_github("apoorvalal/LalRUtils")
}
load_or_install(c('devtools','roxygen2'))
sessionInfo()
####################################################
#%%
home = "/home/alal/Desktop/code/"
setwd(home)
# create('LalRUtils')
#%%
setwd(paste0(home,'LalRUtils'))
document()
