# %%
#' Summary table
#' @param df A dataframe , var A column in said dataframe
#' @return table with summary statistics
#' @export
#' @examples
#' summar(mtcars)
summar = function(df){
  col1 = apply(df, 2, function(x) sum(is.na(x)))
  col2 = apply(df, 2, function(x) length(unique(x)))
  col3 = apply(df, 2, min) |> round(2)
  col4 = apply(df, 2, max) |> round(2)
  col5 = apply(df, 2, mean) |> round(2)
  col6 = apply(df, 2, var) |> round(2)
  col7plus = apply(df, 2, function(x) quantile(x, c(0.25, 0.5, 0.75)))  |> t() |> round(2)
  data.frame(n_missing = col1, n_unique = col2, minimum = col3,
    col7plus, avg = col5, variance = col6, maximum = col4)
}

# %%
#' Summary table with percentages for categorical variables
#' @param df A dataframe , var A column in said dataframe
#' @keywords dataframe variable name categorical
#' @export
#' @examples
#' freq_table(mtcars, 'as.factor(cyl)')
freq_table = function(df, var) {
    suppressMessages(library(dplyr))
    ft = df %>% count_(var) %>%
      mutate(prop=prop.table(n)) %>%
      arrange(desc(prop))
    return(ft)
}


#' Simple Difference in Means estimator for binary treatment (for HW etc)
#' @param df A dataframe
#' @param outcome The outcome variable (in df)
#' @param treatment The treatment indicator (in df)
#' @keywords Treatment Effect ATE
#' @export
#' @examples
#' \dontrun{
#' ate_diffmeans(mtcars, outcome = mpg, treatment = vs)
#' }

ate_diffmeans <- function(df, outcome, treatment){
  # general function to calculate treatment effect
  # and standard error for binary treatments
  suppressPackageStartupMessages(library(dplyr))
  outcome = enquo(outcome); treatment = enquo(treatment)
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


#' Normalised Difference (Imbens, Rubin 2015) for observational studies
#' @param df dataframe to check balance
#' @param treat dummy for treatment
#' @param value covariate to calculate normalised difference statistic
#' @keywords regression linear model balance check
#' @export
#' @examples
#' norm_diff(mtcars, vs, gear)
norm_diff <- function(df, treat, value){
  suppressPackageStartupMessages(library(tidyverse))
  value = enquo(value); treat = enquo(treat)
  df %>% group_by(!! treat) %>%
    summarise(mean=mean(!! value), sd=sd(!! value)) -> summ
  m_t = summ %>% filter(!! treat == 1) %>% .$mean
  s_t = summ %>% filter(!! treat == 1) %>% .$sd
  m_c = summ %>% filter(!! treat == 0) %>% .$mean
  s_c = summ %>% filter(!! treat == 0) %>% .$sd
  (m_t - m_c)/sqrt((s_t^2 + s_c^2) / 2)
}

#' Balance Check for observational studies
#' @param df dataframe to check balance
#' @param treatvar dummy for treatment
#' @param bal_vars covariates to check balance in
#' @keywords regression linear model balance check
#' @export
#' @examples
#' \dontrun{
#' balance_table(mtcars, treatvar = 'vs', bal_vars = c('mpg', 'cyl', 'disp'))
#' }

balance_table <- function (df, treatvar, bal_vars)  {
    treat_all   = df[which(df[, treatvar] == 1), bal_vars]
    control_all = df[which(df[, treatvar] == 0), bal_vars]
    bal_mat = matrix(NA, ncol = 7, nrow = length(bal_vars))
    i = 1
    for (bv in bal_vars) {
        treat   = na.omit(treat_all[, bv])
        control = na.omit(control_all[, bv])
        # t test output
        t = t.test(df[, bv] ~ df[, treatvar], na.rm = TRUE)
        # treatment effects
        bal_mat[i, 1] = t$estimate[1]
        bal_mat[i, 3] = t$estimate[2]
        bal_mat[i, 5] = t$estimate[2] - t$estimate[1]
        bal_mat[i, 6] = t$p.value
        # sd and normalised difference
        bal_mat[i, 2] = sd(control)/ sqrt(length(control))
        bal_mat[i, 4] = sd(treat)  / sqrt(length(treat))
        # normalised difference
        bal_mat[i, 7] = (bal_mat[i, 5]/ sqrt(sd(control)^2 + sd(treat)^2)/2)
        i = i+1
      }
    rownames(bal_mat) = bal_vars
    colnames(bal_mat) = c("Control Mean", "Control SE", "Treatment Mean",
                          "Treatment SE", "Difference in Means", "p-value",
                          "Normalised Difference")
    bal_mat = round(bal_mat, digits = 4)
    bal_mat
}
