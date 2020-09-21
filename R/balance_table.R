#' Balance Check for observational studies; includes SD and Normalised Difference
#' @param df dataframe to check balance
#' @param treatvar dummy for treatment
#' @param bal_vars covariates to check balance in
#' @keywords regression linear model balance check
#' @export
#' @examples
#' balance_table(mtcars, treatvar = 'vs', bal_vars = c('mpg', 'cyl', 'disp'))
balance_table <- function (df, treatvar, bal_vars)  {
    treat_all   = df[which(df[, treatvar] == 1), bal_vars]
    control_all = df[which(df[, treatvar] == 1), bal_vars]
    bal_mat = matrix(NA, ncol = 7, nrow = length(bal_vars))
    i = 1
    for (bv in bal_vars) {
        treat   = na.omit(treat_all[, bv])
        control = na.omit(control_all[, bv])
        # t test output
        t = t.test(df[, bv] ~ df[, treatvar], na.rm = TRUE)
        bal_mat[i, 1] = t$estimate[1]
        bal_mat[i, 3] = t$estimate[2]
        bal_mat[i, 5] = t$estimate[2] - t$estimate[1]
        bal_mat[i, 6] = t$p.value
        # sd and normalised difference
        bal_mat[i, 2] = sd(control)/sqrt(length(control))
        bal_mat[i, 4] = sd(treat)/sqrt(length(treat))
        # normalised difference
        bal_mat[i, 7] = (bal_mat[i, 5]/ sqrt(sd(control)^2 + sd(treat)^2)/2)
        i = i+1
      }
    rownames(bal_mat) = bal_vars
    colnames(bal_mat) = c("Control Mean", "Control SE", "Treatment
      Mean", "Treatment SE", "Difference in Means", "p-value", "Normalised Difference")
    bal_mat = round(bal_mat, digits = 4)
    bal_mat
}
