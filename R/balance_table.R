#' Balance Check
#' @param df dataframe to check balance
#' @param treatvar dummy for treatment
#' @param bal_vars covariates to check balance in
#' @keywords regression linear model balance check
#' @export
#' @examples
#' formula_lfe(y='mpg', X = c('hp', 'drat'), D = c('wt', 'vs'))
#' balance_table(nsw_psid, 'nsw', c('age', 'black', 'u75'))
balance_table = function(df,
    treatvar, bal_vars){
  index_bal = which(names(df) %in% bal_vars)
  bal_mat = matrix(NA, ncol = 4, nrow = length(index_bal))
  for (i in 1:length(index_bal)){
    t = t.test(df[,index_bal[i]] ~ df[, treatvar], na.rm = TRUE)
    bal_mat[i, 1] = round(t$estimate[1], digits = 3)
    bal_mat[i, 2] = round(t$estimate[2], digits = 3)
    bal_mat[i, 3] = round(t$estimate[2] - t$estimate[1], digits = 3)
    bal_mat[i, 4] = round(t$p.value, digits = 3)
  }
  rownames(bal_mat) = names(df)[index_bal]
  colnames(bal_mat) = c("Control Mean", "Treatment Mean", "Difference in Means", "p-value")
  format(bal_mat, digits = 4)
}
