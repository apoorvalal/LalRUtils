#' Compute subclassification ATT and ATE
#' @param df data.table
#' @param x character vector of discrete covariates
#' @param y outcome
#' @param w treatment
#' @param debug boolean to return strata-specific estimates
#' @export
#' @examples
#' \dontrun{
#' subclassify(mtcars, x = "cyl", y = "mpg", w = "foreign")
#' }

subclassify = function(df, x, y = 're78', w = 'treat', debug = F){
  require(data.table)
  if (!data.table::is.data.table(df)) {
    df <- data.table::as.data.table(df)
  }
  N = nrow(df); N1 = sum(df[[w]])
  grpmeans = df[, list(grpmean = mean(get(y)), N = .N), by = c(x, w)]
  fml = as.formula(paste0(paste(x, collapse = "+"), "~", w))
  strata_level = dcast(grpmeans, fml, value.var = c("grpmean", "N"))
  strata_level[, `:=`(DiM = grpmean_1 - grpmean_0, N_k = N_1 + N_0)]
  ATE = strata_level[, sum(DiM * (N_k/N))]; ATT = strata_level[, sum(DiM * (N_1/N1))]
  return(list(est = data.frame(ATE = ATE, ATT = ATT), table = strata_level))
}

# %%
