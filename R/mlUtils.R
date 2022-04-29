# %% ####################################################
#' test-train split
#' @param df [data.frame, matrix] data table (n x p)
#' @param k share of data obs in train
#' @return list with test and train
#' @export
ttsplit = function(df, k = 0.75){
  # sample k% of obs
  trainid <- sample(nrow(df), nrow(df) * k, replace=FALSE)
  list(train = df[trainid,], test = df[-trainid,])
}

# %% ####################################################
#' create folds for cross-fitting
#' @param df dataframe - used to compute nrows
#' @param nf Number of folds
#' @return list with indices for each fold in first element, and fold assignment vector in second element
#' @export
createFolds = function(df, nf = 10){
  n = nrow(df)
  foldid = rep.int(1:nf, times = ceiling(n/nf))[sample.int(n)]
  list(fold_assignments = split(1:n, foldid),
       fold_indices = foldid)
}

# %% ####################################################
#' min-max scale (maps continuous variable to [0, 1])
#' @param X vector
#' @export
mMscale = function(X){
  X = as.matrix(X)
  mins = apply(X,2,min)
  maxs = apply(X,2,max)
  return(scale(X, center=mins, scale=maxs-mins))
}
