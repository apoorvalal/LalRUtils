# %% ####################################################
#' matrix constructor
#' @param ... Vector of numbers with "|" as row break
#' @return matrix
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


# %% ####################################################
#' Omit NA entries in list
#' @param y list of elements
#' @return y with NAs omitted
#' @export
naOmitList = function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }


# %% ####################################################
#' Omit element if it contains "contains"
#' @param y list of elements
#' @return y with "contains" omitted
#' @export
listOmitIf = function(lst, contains) {
  lst[lapply(lst,
    function(x) length(grep(contains,x,value=FALSE))) == 0]
}

# %% ####################################################
#' Get n'th element in list of lists
#' @param list.of.lists list of lists
#' @param nth.element index of list to return
#' @return list
#' @export
getNthElement = function(list.of.lists, nth.element){
  sapply(list.of.lists, `[`, nth.element)
}
