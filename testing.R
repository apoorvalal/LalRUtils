####################################################
rm(list=ls())
library(LalRUtils)
load_or_install(c('tidyverse','Hmisc','AER', 'lfe', 'stargazer')) #, "lib2")
####################################################
#%%
data("CPS1985")
# formula stitcher
fml1 = formula_stitcher('wage',c('age','experience','married'),
                        c('ethnicity','sector'))
lm1 <- lm(fml1,data=CPS1985)

# lfe stitcher
fml2 = formula_lfe('wage', c('age', 'experience', 'married'),
                   D = c('ethnicity', 'sector'))

lm2 <- robustify(felm(fml2,data=CPS1985))
stargazer(lm1, lm2, type = 'text') # should be the same point estimates
#%%
nmiss_nun(CPS1985)
