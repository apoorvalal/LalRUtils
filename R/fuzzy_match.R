#' fuzzy matches 2 character vectors, and returns most similar pairs
#' @param character vectors a and b
#' @keywords fuzzy match string
#' @export
#' @examples
#' fuzzy_match(keys_A, keys_B)
fuzzy_match <- function(a,b) {
  require(stringdist)
  # calculate a jaccard dissimilarity matrix
  distance <- stringdistmatrix(a,b,method = 'jaccard')
  # find the closest match for each
  match <- apply(distance, 1, which.min)
  key = a
  values = b[match] # slice b using match index
  return(as.data.frame(cbind(key, values)))
}
