# %% ####################################################
#' test-train split
#' @param df [data.frame, matrix] data table (n x p)
#' @param k share of data obs in train
#' @return list with test and train
#' @export
ttsplit = function(df, k = 0.75) {
  # sample k% of obs
  trainid = sample(nrow(df), nrow(df) * k, replace = FALSE)
  list(train = df[trainid, ], test = df[-trainid, ])
}

# %% ####################################################
#' create folds for cross-fitting
#' @param df dataframe - used to compute nrows
#' @param nf Number of folds
#' @return list with indices for each fold in first element, and fold assignment vector in second element
#' @export
createFolds = function(df, nf = 10) {
  n = nrow(df)
  foldid = rep.int(1:nf, times = ceiling(n / nf))[sample.int(n)]
  list(
    fold_assignments = split(1:n, foldid),
    fold_indices = foldid
  )
}

# %% ####################################################
#' min-max scale (maps continuous variable to [0, 1])
#' @param X vector
#' @export
mMscale = function(X) {
  X = as.matrix(X)
  mins = apply(X, 2, min)
  maxs = apply(X, 2, max)
  return(scale(X, center = mins, scale = maxs - mins))
}

# %%
#' Classification Evaluation
#'
#' @param actual vector of real labels
#' @param predicted vector of predicted labels
#' @return list of classification accuracy metrics
#' @export
classifEval = function(actual, predicted) {
  cm = as.matrix(table(Actual = actual, Predicted = predicted))
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class

  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes

  list(
    accuracy  = sum(diag) / n,
    precision = diag / colsums,
    recall    = diag / rowsums,
    f1        = 2 * precision * recall / (precision + recall)
  )
}
# %%
#' plot calibration
#' @param y    true value (vector)
#' @param yhat predicted value (vector)
#' @param nbins number of bins (defaults to 10)
#' @return ggplot of calibration
#' @export
calibPlot = function(y, yhat, nbins = 10){
  df = data.table(y, yhat)
  df[, ppbin := cut(yhat, seq(0,1,,nbins), include.lowest = TRUE)]
  # agged
  pdf = df[, .(avg_real = mean(y), avg_pred = mean(yhat)), ppbin]
  # fig
  ggplot(pdf,aes(x=avg_pred,y=avg_real)) +
    geom_point(color="dodgerblue") + geom_line(color="dodgerblue") +
    geom_abline(intercept = 0,slope=1,linetype=2) +
    xlim(0,1) + ylim(0,1) +
    xlab("Mean Predicted Value") + ylab("Fraction of Positives")
}
# %%
