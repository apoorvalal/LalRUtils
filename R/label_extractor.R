#' extracts variable labels and returns a dataframe with varname-varlabel for DTA files
#' @param dataframe (read from stata)
#' @keywords stata data labels
#' @export
#' @examples
#' label_extractor(df)

label_extractor = function(df, colnames = c('names', 'var.labels')){
  info = data.frame(attributes(df)[colnames])
  return(info)
}
