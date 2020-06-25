#' Summary table for Rdrobust estimates to export using kableExtra
#' @param rdr_out Rdrobust object
#' @param prec decimal places
#' @keywords regression-discontinuity export
#' @export
#' @examples
#' rdrobust(df$vote, df$margin, all = T) %>% rdr_export -> rdre
#' rdre %>% kable(, format = "latex", booktabs = T) %>% collapse_rows(columns = 1, latex_hline = "major", valign = "middle") %>% cat(., file = "table.tex")

rdr_export <- function(rdr_out, prec = 4){
  suppressPackageStartupMessages(require(glue))
  outrows = c('coef', 'se', 'z', 'ci', 'bws', 'N_h', 'N_b')
  out = rdr_out[outrows]
  # convential local-linear results
  ids = 1
  CI = glue('(', as.numeric(round(out$ci[ids, ][1], prec)), ',',
                 as.numeric(round(out$ci[ids, ][2], prec)), ')')
  conventional = rbind('Coef'   = round(out$coef[ids], prec),
                       'SE'     = round(out$se[ids], prec),
                       't-stat' = round(out$z[ids], prec), CI)
  # robust rows
  ids = 3
  CI = glue('(', as.numeric(round(out$ci[ids, ][1], prec)), ',',
                 as.numeric(round(out$ci[ids, ][2], prec)), ')')
  robust = rbind('Coef'   = round(out$coef[ids], prec),
                 'SE'     = round(out$se[ids], prec),
                 't-stat' = round(out$z[ids], prec), CI)
  # common rows
  common_rows = rbind(
    'bw'          = paste0('(',  round(out$bws[1, 1], prec), ',',
                                 round(out$bws[1, 2], prec), ')'),
    'Nobs'       = paste0('(', out$N_h[1], ',', out$N_h[2], ')'),
    'poly_order' = rdr_out$p)
  res = rbind(conventional, robust, common_rows)
  res2 = data.table(res, keep.rownames = TRUE)
  res2[1:4, est := "Local-Linear"]
  res2[5:8, est := "Robust"]
  res2[9:11,est := "--"]
  res2[rn == "poly_order", rn := "Polynomial Order"]
  res2[rn == "bw",         rn := "Bandwidth"]
  res2[rn == "Nobs",       rn := "Number of Observations"]
  setcolorder(res2, c("est", "rn", "V1"))
  return(res2)
}
