#' Compute subclassification ATT and ATE
#' @param df data.table
#' @param x character vector of discrete covariates
#' @param y outcome
#' @param w treatment
#' @param debug boolean to return strata-specific estimates
subclass_est = function(df, x, y = 're78', w = 'treat', debug = F){
  df = as.data.table(df)
  # denominators
  N = nrow(df); N1=sum(df[[w]])
  # aggregate by group
  grpmeans = df[, .(grpmean = mean(get(y)), N = .N), by= c(x, w)]
  # reshape needs formula - more general to accommodate multiple Xs
  fml = as.formula(paste0(paste(x, collapse = "+"), "~", w))
  # reshape to wide
  strata_level = dcast(grpmeans, fml, value.var = c("grpmean", "N"))
  strata_level[,  `:=`(DiM = grpmean_1 - grpmean_0, N_k = N_1 + N_0)]
  ATE = strata_level[, sum(DiM * (N_k/N))]
  ATT = strata_level[, sum(DiM * (N_1/N1))]
  if(debug){ return(list(est = data.frame(ATE = ATE, ATT = ATT), table = strata_level)) }
  else{ return(data.frame(ATE = ATE, ATT = ATT))}
}
