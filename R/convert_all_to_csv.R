####################################
#' Writes all dataframes to csv
#' @param dataframes = list of dataframes,
#' @param path = path of file/s
#' @export
#' @keywords write save bulk
#' @examples
#' convert_all_to_csv(dataframes, path)

####################################
# writes all dataframes to csv
####################################

convert_all_to_csv <- function(dataframes,path) {
    for (file in dataframes){
        write.csv(mget(file, .GlobalEnv),
            paste0(path,'/',file,'.csv'))
    }
}
