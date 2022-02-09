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


#' min-max scale (maps continuous variable to [0, 1])
#' @param X vector
#' @export
mMscale = function(X){
  X = as.matrix(X)
  mins = apply(X,2,min)
  maxs = apply(X,2,max)
  return(scale(X, center=mins, scale=maxs-mins))
}
