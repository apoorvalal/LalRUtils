####################################################
rm(list=ls())
user_name = Sys.info()['user']
temp_path = Sys.getenv()['TMP']
if (require('LalRUtils')==F) {
    library(devtools)
    devtools::install_github("apoorvalal/LalRUtils")
}
load_or_install(c('tidyverse','Hmisc','AER')) #, "lib2")
sessionInfo()
####################################################
data("CPS1985")
fml1 = formula_stitcher('wage',c('age','experience','married'),c('ethnicity','sector'))
lm1 <- lm(fml1,data=CPS1985)
summary(lm1)
