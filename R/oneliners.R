#' Oneliners for piping etc
#' @export
nunique = function(x) length(unique(x))

#' @export
lv = function() .Last.value

#' @export
mMscale = function(X){
  X = as.matrix(X)
  mins = apply(X,2,min)
  maxs = apply(X,2,max)
  return(scale(X, center=mins, scale=maxs-mins))
}

#' @export
matr = function(...) {
  # turn into string
  # args=deparse(substitute(rbind(cbind(...))))
  args = ... |> cbind() |> rbind() |> substitute() |> deparse()
  # create "rbind(cbind(.),cbind(.),.)" construct
  args = gsub("\\|","),cbind(",args)
  # eval
  eval(parse(text=args))
}

#' @export
checkmark = function(name, yesno, format = 'latex') {
  if (format %in% c("html", "text")){
    return(c(name, ifelse(yesno, "✓", "")))
  } else{
    return(c(name, ifelse(yesno, "$\\checkmark$", "")))
  }
}

#' @export
matrix2latex = function(matr) {
  printmrow = function(x) cat(cat(x, sep=" & "), "\\\\ \n")
  cat("$$ \n", "\\begin{bmatrix}","\n")
  body = apply(matr, 1, printmrow)
  cat("\\end{bmatrix}", "\n$$")
}


#' @export
dataPkg = function(pkg) data(package = pkg)$results[, 3:4]

# "not.in" function
#' @export
'%!in%' = function(x,y)!('%in%'(x,y))

# Omit NA entries in list
#' @export
naOmitList = function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }

# Omit element if it contains "contains"
#' @export
listOmitIf = function(lst, contains) {lst[lapply(lst, function(x) length(grep(contains,x,value=FALSE))) == 0]}

# Get n'th element in list of lists
#' @export
getNthElement = function(list.of.lists, nth.element){
  sapply(list.of.lists, `[`, nth.element)
}

# render html output in atom / jupyter notebooks
#' @export
chr_nb = function(...) as.character(...) |> IRdisplay::display_html()
