####################################################
#' R implementation of binned scatterplot and CEF plotter, with added options for cluster variance
#' @param fmla FELM formula *as a string*
#' @param key_var X axis variable for CEF
#' @param data dataframe
#' @param plotraw T if underlying scatterplot should be plotted
#' @param bins number of bins
#' @param rawdata_colour   Colour of Rawdata
#' @param rawdata_alpha    Alpha of Rawdata
#' @param rawdata_size     Size of Rawdata
#' @param linfit_width     width of linear fit
#' @param linfit_colour    colour of linear fit
#' @param cef_point_size   Size of binscatter points
#' @param cef_point_colour Colour of binscatter points
#' @param ci_colour        Colour of CI ribbon
#' @param ci_alpha         Alpha of CI ribbon
#' @export
#' @keywords cef bins scatterplot
#' @examples
#' binscatter('Sepal.Length ~ Petal.Length + Petal.Width|Species', key_var = 'Petal.Width', iris)

binscatter = function(fmla, key_var, data, plotraw = TRUE, bins = 20,
      rawdata_colour = 'black', rawdata_alpha = 0.2, rawdata_size = 0.5,
      linfit_width = 0.6, linfit_colour = 'blue',
      cef_point_size = 1, cef_point_colour = 'red',
      ci_colour = 'gray', ci_alpha = 0.3){
    # load libraries
    require(lfe); require(stringr); require(ggplot2)
    # FWL
    y <- unlist(strsplit(fmla, "~"))[1] ; x <- unlist(strsplit(fmla, "~"))[2]
    controls <- str_replace(x, key_var, '1') # replace main X with intercept
    # residualise regressions
    reg_y <- felm(formula(paste0(y, "~", controls)), data = data)
    reg_x <- felm(formula(paste0(key_var, "~", controls)), data = data)
    resid_y <- resid(reg_y); resid_x <- resid(reg_x)
    df <- data.frame(resid_y, resid_x)
    colnames(df) <- c(paste0("residual_", names(df)[1]), paste0("residual_", names(df)[2]))
    # are errors clustered
    cluster_grp <- trimws(unlist(strsplit(fmla, "\\|"))[4])
    # regression with partialed Xs and Ys
    if (is.na(cluster_grp)) { reg <- felm(resid_y ~ resid_x) }
    else{reg <- felm(formula(paste0("resid_y ~ resid_x | 0 | 0 |", cluster_grp)), data)}
    intercept <- coef(reg)[1] ; slope <- coef(reg)[2]
    # variance covariance matrix
    if (is.null(reg$clustervcv)) { vcov <- reg$robustvcv; se_type <- "robust" }
    else { vcov <- reg$clustervcv; se_type <- paste0("clustered by ", cluster_grp) }
    Terms <- terms(reg); m_mat <- model.matrix(Terms, data = df)
    fit <- as.vector(m_mat %*% coef(reg))
    se_fit <- sqrt(rowSums((m_mat %*% vcov) * m_mat))
    # confidence intervals
    df$upper_ci <- fit + 1.96 * se_fit; df$lower_ci <- fit - 1.96 * se_fit
    df_bin <- aggregate(df, by = list(cut(as.matrix(df[, 2]), bins)), mean)
    # construct plot
    plot <- ggplot(data = df, aes(x = df[, 2], y = df[, 1]))
    if (plotraw == TRUE) { # plot raw data first
        plot <- plot + geom_point(data = df, aes(x = df[, 2], y = df[, 1]),
              alpha = rawdata_alpha, size = rawdata_size, colour = rawdata_colour)
    }
    plot <- plot + # linear fit
        geom_abline(slope = slope, intercept = intercept,
          color = linfit_colour, size = linfit_width) +
        # confint
        geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
          alpha = ci_alpha, fill = ci_colour) +
        # binned scatterplot
        geom_point(data = df_bin, aes(x = df_bin[, 3], y = df_bin[, 2]),
            color = cef_point_colour, size = cef_point_size) +
        # label slope
        labs(caption = paste0(" slope = ", signif(slope, 2), '\n SE:', se_type),
          x = names(df)[2], y = names(df)[1])
    return(plot)
}
