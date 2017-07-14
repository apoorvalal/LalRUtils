####################################################
rm(list=ls())
user_name = Sys.info()['user']
temp_path = Sys.getenv()['TMP']
sessionInfo()
####################################################
# install.packages('devtools')
library(devtools)
# devtools::install_github("klutometis/roxygen")
library(roxygen2)
home = 'C:/Users/alal/Desktop/projects/scripts/'
setwd(home)
# create('LalRUtils')
setwd(paste0(home,'/LalRUtils'))
document()

setwd(home)
install('LalRUtils')
