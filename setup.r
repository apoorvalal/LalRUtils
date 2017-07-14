# install.packages('devtools')
library(devtools)
# devtools::install_github("klutometis/roxygen")
library(roxygen2)
home = 'C:/Users/alal/Desktop/Research/scripts/'
setwd(home)
# create('LalRUtils')
setwd(paste0(home,'/LalRUtils'))
document()

setwd(home)
install('LalRUtils')

LalRUtils::load_or_install(c('tidyverse','Hmisc'))
