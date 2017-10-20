####################################################
rm(list=ls())
user_name = Sys.info()['user']
temp_path = Sys.getenv()['TMP']
if (require('LalRUtils')==F) {
    library(devtools)
    devtools::install_github("apoorvalal/LalRUtils")
}
load_or_install(c('devtools','roxygen2'))
sessionInfo()
####################################################
home = "C:/Users/alal/Google Drive/3_misc_code/"
setwd(home)
# create('LalRUtils')
setwd(paste0(home,'LalRUtils'))
document()
setwd(home)
# install('LalRUtils')
