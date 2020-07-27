#' Oneliners for piping
#' @export
nunique = function(x) length(unique(x))

#' @export
lv = function() .Last.value

#' @export
mMscale <- function(X){
  X = as.matrix(X)
  mins = apply(X,2,min)
  maxs = apply(X,2,max)
  return(scale(X, center=mins, scale=maxs-mins))
}
