####################################################
#' downloads and unzips file if it is not found in current wdir
#' @param url, filename
#' @export
#' @keywords download
#' @examples
#' \dontrun{
#' get_and_unzip('url','qob.txt')
#' }
get_and_unzip <- function(url, filename){
    if (!file.exists(filename)) {
        download.file(url, "zipped.zip", mode="wb")
        unzip(zipfile="zipped.zip",files=filename)
    }
}
