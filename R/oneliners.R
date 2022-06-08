# %% ####################################################
#' number of unique values in vector
#' @param x vector
#' @return scalar with number of unique values
#' @export
nunique = \(x) length(unique(x))


# %% ####################################################
#' Timestamp with no spaces for filenames
#' @return character with time stamp in format ff
#' @examples
#' timeStamp()
#' @export
timeStamp = \(ff = "%d_%m_%Y__%H%M") format(Sys.time(), ff)

# %% ####################################################
#' inverse logit fn
#' @param x vector
#' @return vector with e(x)/[1+e(x)]
#' @export
expit = \(x) exp(x)/(1+exp(x))


# %% ####################################################
#' last value
#' @return last value
#' @export
lv = \() .Last.value


# %% ####################################################
#' list datasets in package
#' @param pkg package name in string
#' @return table with names of data objects
#' @export
dataPkg = \(pkg) data(package = pkg)$results[, 3:4]

# %% ####################################################
#' not.in function
#' @export
'%!in%' = \(x,y)!('%in%'(x,y))

# %% ####################################################
#' coerce to string
#' @export
chr = \(...) as.character(...)

# %% ####################################################
#' list methods for object
#' @export
mls = \(y) Map(function(x) methods(class = x), class(y))
