# %% ####################################################
#' number of unique values in vector
#' @param x vector
#' @return scalar with number of unique values
#' @export
nunique = function(x) length(unique(x))


# %% ####################################################
#' Timestamp with no spaces for filenames
#' @return character with time stamp in format ff
#' @export
timeStamp = \(ff = "%d_%m_%Y__%H%M") format(Sys.time(), ff)

# %% ####################################################
#' inverse logit fn
#' @param x vector
#' @return vector with e(x)/[1+e(x)]
#' @export
expit = function(x) exp(x)/(1+exp(x))


# %% ####################################################
#' last value
#' @return last value
#' @export
lv = function() .Last.value


# %% ####################################################
#' list datasets in package
#' @param pkg package name in string
#' @return table with names of data objects
#' @export
dataPkg = function(pkg) data(package = pkg)$results[, 3:4]

# %% ####################################################
#' not.in function
#' @export
'%!in%' = function(x,y)!('%in%'(x,y))

# %% ####################################################
#' coerce to string
#' @export
chr = function(...) as.character(...)

# %% ####################################################
#' list methods for object
#' @export
mls = function(y) Map(function(x) methods(class = x), class(y))
