# brackets an interval in which a root of the function f (x) must lie,
# then repeatedly bisects the interval until a root is found within
# the desired precision.

#' @export
bisectionRoot = function(f, xmin, xmax, tol = 1e-5) {
  a = xmin; b = xmax
  # Check inputs
  if (a >= b) {
    cat("error: xmin > xmax \n")
    return(NULL)
  }
  if (f(a) == 0) {
    return(a)
  } else if (f(b) == 0) {
    return(b)
  } else if (f(a) * f(b) > 0) {
    cat("error: f(xmin) and f(xmax) of same sign \n")
    return(NULL)
  }
  # If inputs OK, converge to root
  iter = 0
  while ((b - a) > tol) {
    c = (a + b) / 2
    if (f(c) == 0) {
      return(c)
    } else if (f(a) * f(c) < 0) {
      b = c
    } else {
      a = c
    }
    iter = iter + 1
  }
  c(
    root = (a + b) / 2,
    iter = iter,
    precision = (b - a)
  )
}


#' solve for roots using the NR update rule x1 = x0 − f(x0)/f'(x0)
#' @export
newtonRoot = function(f, df, x0, tol = 1e-5, maxit = 20) {
  root = x0
  for (jit in 1:maxit) {
    dx = f(root) / df(root)
    root = root - dx
    if (abs(dx) < tol) return(c(root = root, jit = jit, dx = dx))
  }
  print(" Maximum number of iterations exceeded.")
}
