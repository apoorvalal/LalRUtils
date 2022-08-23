#' Fork of inborutils::csv_to_sqlite to save a delimited text table into a single table
#' sqlite database that can then be munged w dplyr/ dbplyr / sqldf
#'
#' The table can be a comma separated (csv) or a tab separated (tsv) or any
#' other delimited text file. The file is read in chunks. Each chunk is copied
#' in the same sqlite table database before the next chunk is loaded into
#' memory. See the INBO tutorial \href{https://inbo.github.io/tutorials/tutorials/r_large_data_files_handling/#convertsqlite}{Handling large files in R}
#' to learn more.
#'
#' @section Remark:
#' The \code{callback} argument in the \code{read_delim_chunked} function call
#' refers to the custom written callback function `append_to_sqlite` applied
#' to each chunk.
#'
#' @param csv_file Name of the text file to convert.
#' @param sqlite_file Name of the newly created sqlite file.
#' @param table_name Name of the table to store the data table in the sqlite
#'   database.
#' @param delim Text file delimiter (default ",").
#' @param pre_process_size Number of lines to check the data types of the
#'   individual columns (default 1000).
#' @param chunk_size Number of lines to read for each chunk (default 50000).
#' @param show_progress_bar Show progress bar (default TRUE).
#' @param ... Further arguments to be passed to \code{read_delim}.
#' @return a SQLite database
#' @export
csv_to_sqlite = function(csv_file, sqlite_file, table_name,
                          delimiter = ",",
                          pre_process_size = 1000, chunk_size = 50000,
                          show_progress_bar = TRUE, ...) {
  require(dplyr); require(DBI); require(lubridate); require(readr)
  # init connection
  con = dbConnect(SQLite(), dbname = sqlite_file)
  # read a first chunk of data to extract the colnames and types
  # to figure out the date and the datetime columns
  df = read_delim(csv_file, delim = delimiter, n_max = pre_process_size, ...)
  names = colnames(df)
  date_cols = df %>%
    select_if(is.Date) %>%
    colnames()
  datetime_cols = df %>%
    select_if(is.POSIXt) %>%
    colnames()
  # write the first batch of lines to SQLITE table, converting dates to string
  # representation
  df = df %>%
    mutate_at(.vars = date_cols, .funs = as.character.Date) %>%
    mutate_at(.vars = datetime_cols, .funs = as.character.POSIXt)
  dbWriteTable(con, table_name, df, overwrite = TRUE)
  # readr chunk functionality
  read_delim_chunked(
    csv_file,
    callback = append_to_sqlite(
      con = con,
      table_name = table_name,
      date_cols = date_cols,
      datetime_cols = datetime_cols
    ),
    delim = delimiter,
    skip = pre_process_size, chunk_size = chunk_size,
    progress = show_progress_bar,
    col_names = names, ...
  )
  dbDisconnect(con)
}

#' Callback function that appends new sections to the SQLite table.
#' @param con A valid connection to SQLite database.
#' @param table_name Name of the table to store the data table in the sqlite
#'   database.
#' @param date_cols Name of columns containing Date objects
#' @param datetime_cols Name of columns containint POSIXt objects.
#'
#' @keywords internal
append_to_sqlite = function(con, table_name,
                             date_cols, datetime_cols) {
  #' @param x Data.frame we are reading from.
  function(x, pos) {
    x = as.data.frame(x)
    x = x %>%
      mutate_at(.vars = date_cols, .funs = as.character.Date) %>%
      mutate_at(.vars = datetime_cols, .funs = as.character.POSIXt)
    # append data frame to table
    dbWriteTable(con, table_name, x, append = TRUE)
  }
}

####################################
#' Writes all dataframes to csv
#' @param dataframes = list of dataframes,
#' @param path = path of file/s
#' @export
#' @keywords write save bulk
#' @examples
#' \dontrun{
#' convert_all_to_csv(dataframes, path)
#' }
#'
####################################
# writes all dataframes to csv
####################################
#'
convert_all_to_csv = function(dataframes, path) {
  for (file in dataframes) {
    write.csv(mget(file, .GlobalEnv), paste0(path, '/', file, '.csv'))
  }
}

# %%
#' Small summary table with stata variable labels from DTA file
#' @param df dataframe to check balance
#' @keywords stata summary labels dataframe
#' @export
#' @examples
#' \dontrun{
#' dta_vlabs(df)
#' }
#'
dta_vlabs = \(dta) sapply(dta, function(x) attr(x, "label"))

# %%

#' extracts variable labels and returns a dataframe with varname-varlabel for DTA files
#' @param dataframe (read from stata)
#' @keywords stata data labels
#' @export
#' @examples
#' \dontrun{
#' label_extractor(df)
#' }
#'
label_extractor = function(df, colnames = c('names', 'var.labels')) {
  info = data.frame(attributes(df)[colnames])
  return(info)
}


####################################################
#' Reads in all datasets in given location
#' @param path of file/s
#' @export
#' @keywords read stata sas spss
#' @examples
#' \dontrun{
#' read_all_files(extension = 'dta', path = '~/data/')
#' }
#'
read_all_files = function(extension, location) {
  suppressMessages(library(fread))
  suppressMessages(library(haven))
  setwd(location)
  file_pattern = paste0("\\.", extension, "$")
  obj = list.files(pattern = file_pattern)
  pos = regexpr(extension, obj)
  objs = substr(obj, 1, pos - 2)
  if (extension == 'Rdata') {
    df = lapply(obj, load, envir = .GlobalEnv)
  } else if (extension == 'csv') {
    df = lapply(obj, fread, envir = .GlobalEnv)
  } else if (extension == 'dta') {
    for (n in 1:length(objs)) {
      assign(paste0(objs[n]), haven::read_dta(obj[n]), envir = .GlobalEnv)
    }
  } else if (extension == 'sav') {
    for (n in 1:length(objs)) {
      assign(paste0(objs[n]), haven::read_sav(obj[n]), envir = .GlobalEnv)
    }
  }
  return(objs)
}


# %%

####################################################
#' downloads and unzips file if it is not found in current wdir
#' @param url, filename
#' @export
#' @keywords download
#' @examples
#' \dontrun{
#' get_and_unzip('url', 'qob.txt')
#' }
get_and_unzip = function(url, filename) {
  if (!file.exists(filename)) {
    download.file(url, "zipped.zip", mode = "wb")
    unzip(zipfile = "zipped.zip", files = filename)
  }
}


#' Populates a stat-transfer script to convert files to CSV
#' @param path of file/s
#' @export
#' @keywords stat_transfer convert reader
#' @examples
#' \dontrun{
#' stat_transfer_data(path = 'C:/data/data1.sas7bdat', outpath = 'C:/working/')
#' }
#'
############################################
# runs stat-transfer from R - tested on Windows 10
############################################
# alternative to the package readr when files are very large + you have access to stat-transfer
stat_transfer_data = function(path,
                               stat.transfer.path = '\"C:\\Program Files\\StatTransfer12-64\\st.exe\"',
                               out.ext = 'csv',
                               outpath) {
  library(tools)
  inp.ext = file_ext(path)
  if (missing(outpath)) {
    path.out = gsub(inp.ext, out.ext, path)
    print(c('Writing to ', path.out))
  } else {
    fn = basename(path)
    fn.out = gsub(inp.ext, out.ext, fn)
    path.out = paste0(outpath, '\\', fn.out)
    print(c('Writing to ', path.out))
  }
  target = 'conv_script.stcmd'
  target_full = paste0(temp.path, 'conv_script.stcmd')
  sink(target_full)
  cat("SET VAR_CASE_CS        lower \n")
  cat('copy \"', path, '\"  ', '\"', path.out, '\"', '  -y \n', sep = "")
  cat('quit \n')
  cat('\n')
  sink()
  command = readlines(target_full)
  print(command)
  transfer_command = paste0(
    stat.transfer.path,
    ' \"', target_full, '\" \\e'
  )
  system(transfer_command,
    wait = TRUE,
    invisible = FALSE,
    show.output.on.console = TRUE
  )
}
