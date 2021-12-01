#%%
rm(list=ls())
library(LalRUtils)
load_or_install(c('data.table','AER','lfe', 'modelsummary')) #, "lib2")
theme_set(lal_plot_theme())
#%%
data("CPS1985")
# formula stitcher
fml1 = formula_stitcher('wage',c('age','experience','married'),
                        c('ethnicity','sector'))
lm1 <- lm(fml1,data=CPS1985)
#%%
# lfe stitcher
fml2 = formula_lfe('wage', c('age', 'experience', 'married'),
                   D = c('ethnicity', 'sector'))
lm2 <- robustify(felm(fml2,data=CPS1985))
msummary(list(lm1, lm2), type = 'text') |> chr_nb() # should be the same point estimates
#%%
regplot_dens(CPS1985, xvar = education, yvar = wage, zvar = ethnicity)
# %%
