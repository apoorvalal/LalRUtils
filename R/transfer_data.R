#' Populates a stat-transfer script to convert files to CSV
#' @param path of file/s
#' @export
#' @keywords stat_transfer convert reader
#' @examples
#' transfer_data(path='C:/data/data1.sas7bdat',outpath='C:/working/')

############################################
# runs stat-transfer from R - tested on Windows 10
############################################
# alternative to the package readr when files are very large + you have access to stat-transfer
transfer_data <- function(path,
              stat.transfer.path = '\"C:\\Program Files\\StatTransfer12-64\\st.exe\"',
              out.ext='csv',
              outpath)
                {
    library(tools)
    inp.ext = file_ext(path)
    if(missing(outpath)) {
        path.out = gsub(inp.ext,out.ext,path)
        print(c('Writing to ', path.out))
    }else{
        fn = basename(path)
        fn.out = gsub(inp.ext,out.ext,fn)
        path.out = paste0(outpath,'\\',fn.out)
        print(c('Writing to ', path.out))
    }
    target = 'conv_script.stcmd'
    target_full = paste0(temp.path,'conv_script.stcmd')
    sink(target_full)
    cat("SET VAR_CASE_CS        lower \n")
    cat('copy \"',path,'\"  ', '\"',path.out,'\"', '  -y \n', sep="")
    cat('quit \n')
    cat('\n')
    sink()

    command = readlines(target_full)
    print(command)
    transfer_command = paste0(stat.transfer.path,
                                ' \"',target_full,'\" \\e')
    system(transfer_command,
        wait=TRUE,
        invisible=FALSE,
        show.output.on.console=TRUE)
}
