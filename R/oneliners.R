# %% ####################################################
#' number of unique values in vector
#' @param x vector
#' @return scalar with number of unique values
#' @export
nunique = \(x) length(unique(x))

# %%
#' Shorter length
#' @param x object
#' @return scalar with length
#' @export
len = \(x) length(x)

# %%
#' Source all R scripts in directory
#' @param p path string
#' @export
sourceDir = \(p) {
  invisible(lapply(
    list.files(p, full.names = TRUE, pattern = "*.R"),
    source
  ))
}

# %%
#' Hex codes for portal game colours (blue and orange)
#' @export
portal_hexcodes = \() c("#ff9a00", "#00a2ff", "#ff5d00", "#0065ff")

# %%
#' t-norm of a vector
#' @param x vector
#' @param t integer
#' @export
vnorm = \(x, t = 2) norm(matrix(x, ncol = 1), t)



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
expit = \(x) exp(x) / (1 + exp(x))

# %%
#' clip vector to specified range (clone of np.clip)
#' @param x vector
#' @param l lower bound
#' @param u upper bound
#' @return clipped vector
#' @export
clip = \(x, l, u) pmax(pmin(x, u), l)

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
'%!in%' = \(x, y)!('%in%'(x, y))

# %% ####################################################
#' coerce to string
#' @export
chr = \(...) as.character(...)

# %% ####################################################
#' list methods for object
#' @export
mls = \(y) Map(function(x) methods(class = x), class(y))
