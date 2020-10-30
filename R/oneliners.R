#' Oneliners for piping etc
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

#' @export
matr <-function(...) {
  # turn into string
  args<-deparse(substitute(rbind(cbind(...))))
  # create "rbind(cbind(.),cbind(.),.)" construct
  args<-gsub("\\|","),cbind(",args)
  # eval
  eval(parse(text=args))
}

#' @export
checkmark <- function(name, yesno, format = 'latex') {
  if (format %in% c("html", "text")){
    return(c(name, ifelse(yesno, "✓", "")))
  } else{
    return(c(name, ifelse(yesno, "$\\checkmark$", "")))
  }
}

#' @export
datapkg = function(pkg) data(package = pkg)$results[, 3:4]
