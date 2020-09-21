#' Simple Difference in Means estimator for binary treatment (for HW etc)
#' @param df A dataframe
#' @param outcome The outcome variable (in df)
#' @param treatment The treatment indicator (in df)
#' @keywords Treatment Effect ATE
#' @export
#' @examples
#' nsw_te = ate_diffmeans(mtcars, outcome = mpg, treatment = vs)

ate_diffmeans <- function(df, outcome, treatment){
  # general function to calculate treatment effect
  # and standard error for binary treatments
  suppressPackageStartupMessages(library(dplyr))
  outcome = enquo(outcome)
  treatment = enquo(treatment)
  df %>% group_by(!! treatment) %>%
  summarise(means    = mean(!! outcome),
            sigma_2  = var(!! outcome),
            nobs     = n()
    )  -> sumcalc
  mu_diff = sumcalc$means[2] - sumcalc$means[1]
  se_hat = sqrt(
    sumcalc$sigma_2[1] / sumcalc$nobs[1] +
    sumcalc$sigma_2[2] / sumcalc$nobs[2])
  # report estimates and ingredients (in case order is flipped etc)
  return(list(te = mu_diff, std_err = se_hat, sumstats = sumcalc))
}
