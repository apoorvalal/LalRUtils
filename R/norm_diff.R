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
