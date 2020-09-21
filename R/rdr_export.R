#' Summary table for Rdrobust estimates to export using kableExtra
#' @param rdr_out Rdrobust object
#' @param prec decimal places
#' @keywords regression-discontinuity export
#' @export
#' @examples
#' rdrobust(df$vote, df$margin, all = T) %>% rdr_export -> rdre
#' rdre %>% kable(, format = "latex", booktabs = T) %>% collapse_rows(columns = 1, latex_hline = "major", valign = "middle") %>% cat(., file = "table.tex")

rdr_export <- function(rdr_out, prec = 3){
    outrows = c("coef", "se", "z", "ci", "bws", "N_h", "N_b")
    out = rdr_out[outrows]
    # conventional
    ids = 1
    CI = paste0("(", as.numeric(round(out$ci[ids, ][1], prec)),
        ",", as.numeric(round(out$ci[ids, ][2], prec)), ")")
    conventional = rbind(Coef = round(out$coef[ids], prec), SE = round(out$se[ids],
        prec), `t-stat` = round(out$z[ids], prec), CI)
    # robust
    ids = 3
    CI = paste0("(", as.numeric(round(out$ci[ids, ][1], prec)),
        ",", as.numeric(round(out$ci[ids, ][2], prec)), ")")
    robust = rbind(Coef = round(out$coef[ids], prec), SE = round(out$se[ids],
        prec), `t-stat` = round(out$z[ids], prec), CI)
    common_rows = rbind(bw = paste0("(", round(out$bws[1, 1],
        prec), ",", round(out$bws[1, 2], prec), ")"), Nobs = paste0("(",
        out$N_h[1], ",", out$N_h[2], ")"), poly_order = rdr_out$p)
    # prep output table
    resout = cbind(c(rep("Local-Linear", 4), rep("Robust", 4), rep("--", 3)),
                  rbind(conventional, robust, common_rows)) %>%
                  data.frame(rowname = row.names(.), .)
    rownames(resout) <- NULL
    names(resout) = c('stat', 'type', 'val')
    resout$stat = gsub("poly_order", "Polynomial Order", resout$stat)
    resout$stat = gsub("bw", "Bandwidth", resout$stat)
    resout$stat = gsub("nobs", "Obs", resout$stat)
    return(resout[, c(2, 1, 3)])
}
