#' Multi-core replicate.
#' Use multiple cores for repeated evaluation of an expression.
#' @param n integer; the number of replications.
#' @param expr the expression (a language object, usually a call) to evaluate n times.
#' @param mc.cores number of cores to use.
#' @param refresh status update refresh interval.
#' @examples
#' one_sim = \(n = 100, control_prob = 0.1, rel_effect = 0.01) {
#'   treat_prob = control_prob + (control_prob * rel_effect)
#'   cy = rbinom(n = n, size = 1, prob = control_prob)
#'   ty = rbinom(n = n, size = 1, prob = treat_prob)
#'   mean(ty) - mean(cy)
#' }
#' mcReplicate(10, one_sim(), mc.cores = 2)
#'
#' @returns  A vector, matrix, or list of length `n`.
#' @import parallel
#' @export

mcReplicate = function(n, expr, mc.cores = 4, refresh = 0.1) {
  show_progress = \(i_) {
    intervaln = floor(n * refresh)
    if (floor(i_ / intervaln) == i_ / intervaln) cat(paste("[", i_, "/", n, "]\r"))
  }
  result = simplify2array(parallel::mclapply(1:n,
    eval.parent(substitute(
      \(i_, ...) {
        if (refresh > 0) show_progress(i_)
        expr
      }
    )),
    mc.cores = mc.cores
  ))
  if (refresh > 0) cat("\n")
  result
}

#' Multi-core sapply
#' Use multiple cores for simplifying apply.
#' @param X input vector to iterate over
#' @param FUN function
#' @param mc.cores number of cores to use.
#' @returns  A vector, matrix, or list of length `n`.
#' @import parallel
#' @export

mcSapply = function(X, FUN, mc.cores = 6, ...,
    simplify = TRUE) {
  FUN = match.fun(FUN)
  answer = parallel::mclapply(X = X, FUN = FUN, ...)
  if (is.null(names(answer))) names(answer) = as.character(X)
  if (!isFALSE(simplify) && length(answer))
    simplify2array(answer, higher = (simplify == "array"))
  else answer
}
