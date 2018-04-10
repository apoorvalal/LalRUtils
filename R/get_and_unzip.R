####################################################
#' downloads and unzips file if it is not found in current wdir
#' @param url, filename
#' @export
#' @keywords download
#' @examples
#' get_and_unzip('link_to_angrist_site','qob.txt')
get_and_unzip <- function(url,filename){
    if (!file.exists(filename)) {
        download.file(url,"zipped.zip",mode="wb")
        unzip(zipfile="zipped.zip",files=filename)
    }
}
