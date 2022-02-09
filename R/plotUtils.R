#' Stitches together multiple ggplot objects for export-ready graphs
#' @param ... ggplot objects
#' @keywords graphs plots
#' @export
#' @examples
#' \dontrun{
#' plot1 = ggplot(data,aes(xvar1,yvar)) + geom_point()
#' plot2 = ggplot(data,aes(xvar2,yvar)) + geom_point()
#' multiplot(plot1,plot2,cols=2)
#' }

#######################################################################
# Multiple plot function - stitches together multiple ggplot objects
#######################################################################
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
   library(grid)
   # Make a list from the ... arguments and plotlist
   plots <- c(list(...), plotlist)
   numPlots = length(plots)
   # If layout is NULL, then use 'cols' to determine layout
   if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
   }
   if (numPlots==1) {
      print(plots[[1]])
   } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
         # Get the i,j matrix positions of the regions that contain this subplot
         matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

         print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                         layout.pos.col = matchidx$col))
      }
   }
}

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


#' plot binary treatment status for time series for panel dataset
#' @param data     dataframe
#' @param time     bare name of time (not in quotes)
#' @param id       bare name of unit
#' @param status   bare name of treatment
#' @keywords dataframe panel diff-in-diff
#' @export
#' @examples
#'\dontrun{
#' panel_treat_plot(california_prop99, Year, State, treated)
#'}

panel_treat_plot = function(data, time, id, status) {
  tiles_plot <- ggplot(data,
      aes(x = {{time}}, y = {{id}}, fill = as.factor({{status}}) ) ) +
    geom_tile(color = "white", size = 1) +
    scale_y_discrete(limits = rev) +
    scale_fill_manual(values = c("#4285F4", "#DB4437", "#F4B400")) +
    theme_classic(base_size = 14) +
    lal_plot_theme() + labs(fill = "Treatment   :")
  return(tiles_plot)
}


####################################################
#' Scatterplot with regression line and densites by grouping variable. Use this to evaluate overlap.
#' @param df dataframe
#' @param xvar x variable
#' @param yvar y variable
#' @param zvar z variable (coerced to factor)
#' @param title plot title
#' @export
#' @keywords seaborn Scatterplot
#' @examples
#' regplot_dens(mtcars, wt, mpg, am)
regplot_dens = function(df, xvar, yvar, zvar, title = ""){
  require(ggExtra); require(ggplot2)
  p0 = ggplot(df, aes(x = {{xvar}}, y = {{yvar}})) +
    geom_point(aes(colour = as.factor({{zvar}}))) +
    geom_smooth(method = 'lm', alpha = 0.2) +
    scale_colour_brewer(palette = "Set1") +
    ggtitle(title)
  p = ggMarginal(p0, type = "density", size = 7, groupColour = T, groupFill = T)
  return(p)
}


#' Canned subroutine to plot time series for several variables
#' @param df A dataframe
#' @param timevar a string with a colname for the time variable
#' @keywords dataframe variable name categorical
#' @export
#' @examples
#'\dontrun{
#' overall_ineq = tsplotter(overall) + labs(title='Overall Inequality')
#'}
tsplotter = function(df, timevar='year') {
    library(reshape2); library(ggplot2)
    meltdf <- reshape2::melt(df,id=timevar)
    variable='variable'
    value='value'
    p = ggplot(data = meltdf, aes_string(x = timevar,
                y = value, colour = variable, group = variable)) +
          geom_point(size=0.5) + geom_line() +
        lal_plot_theme()
    return(p)
}


#' Theme for maps
#' @param ggplot object
#' @keywords maps ggplot spatial
#' @export
#' @examples
#' \dontrun{
#' map_ggp_object + lal_map_theme()
#' }
lal_map_theme <- function(fontfam="Roboto Condensed", ...) {
  theme_bw() +
  theme(
    text = element_text(family = fontfam, color = "#22211d"),
    legend.position='bottom',
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank(),
    ...
  )
}


#' Wrapper for hrbrthemes::theme_ft_rc() with bigger font, altered legend options and dots
#' @param ggplot object
#' @keywords ggplot plots
#' @export
#' @examples
#' \dontrun{
#' plot_ggp_object + lal_plot_theme_d()
#' }

lal_plot_theme_d <- function(fontfam = "IBM Plex Sans Condensed", fsize = 15, axsize = 13, textangle = 0, ...) {
  suppressMessages(library(hrbrthemes))
  theme_modern_rc(base_family = fontfam, subtitle_family = fontfam,
    axis_title_just = 'c', base_size = fsize) +
  theme(
    text = element_text(family = fontfam, size = fsize),
    axis.text.x = element_text(angle = textangle, size = axsize),
    axis.text.y = element_text(face = "bold", size = axsize),
    strip.text = element_text(color = 'white'),
    legend.position='bottom',
    panel.border = element_blank(),
    ...
  )
}


#' theme for nice looking plots with sensible defaults (no legend title, legend at the bottom)
#' @param ggplot object
#' @keywords ggplot plots
#' @export
#' @examples
#' \dontrun{
#' plot_ggp_object + lal_plot_theme()
#' }

lal_plot_theme = function (fontfam = "IBM Plex Sans Condensed",
      fsize = 14, textangle = 0, ...) {
  theme_minimal() +
    theme(
      text                  = element_text(family = fontfam, size = fsize),
      axis.text.x           = element_text(angle = textangle),
      axis.ticks            = element_line(color = "grey92"),
      plot.title            = element_text(size = 18, face = "bold"),           # title fsize
      plot.title.position   = "plot",                                           # left align
      plot.caption          = element_text(size = 9, margin = margin(t = 15)),  # caption fsize
      plot.caption.position = "plot",                                           # right align
      plot.subtitle         = element_text(size = 12, color = "grey30"),        # subtitle
      legend.position       = "top",
      legend.text           = element_text(color = "grey30"),
      legend.title          = element_text(size = 12),
      panel.border          = element_blank(),
      panel.grid.minor      = element_blank(),
      ...)
}
