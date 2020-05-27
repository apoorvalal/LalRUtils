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
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite dbWriteTable
#' @importFrom readr read_delim read_delim_chunked
#' @importFrom dplyr %>% select_if mutate_at
#' @importFrom lubridate is.Date is.POSIXt
csv_to_sqlite <- function(csv_file, sqlite_file, table_name,
                          delimiter = ",",
                          pre_process_size = 1000, chunk_size = 50000,
                          show_progress_bar = TRUE, ...) {
    # init connection
    con <- dbConnect(SQLite(), dbname = sqlite_file)
    # read a first chunk of data to extract the colnames and types
    # to figure out the date and the datetime columns
    df <- read_delim(csv_file, delim = delimiter, n_max = pre_process_size, ...)
    names = colnames(df)
    date_cols <- df %>% select_if(is.Date) %>% colnames()
    datetime_cols <- df %>% select_if(is.POSIXt) %>% colnames()
    # write the first batch of lines to SQLITE table, converting dates to string
    # representation
    df <- df %>%
      mutate_at(.vars = date_cols, .funs = as.character.Date) %>%
      mutate_at(.vars = datetime_cols, .funs = as.character.POSIXt)
    dbWriteTable(con, table_name, df, overwrite = TRUE)
    # readr chunk functionality
    read_delim_chunked(
      csv_file,
      callback = append_to_sqlite(con = con,
                            table_name = table_name,
                            date_cols = date_cols,
                            datetime_cols = datetime_cols),
      delim = delimiter,
      skip = pre_process_size, chunk_size = chunk_size,
      progress = show_progress_bar,
      col_names = names, ...)
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
append_to_sqlite <- function(con, table_name,
                             date_cols, datetime_cols) {
  #' @param x Data.frame we are reading from.
  function(x, pos) {
    x <- as.data.frame(x)
    x <- x %>%
      mutate_at(.vars = date_cols, .funs = as.character.Date) %>%
      mutate_at(.vars = datetime_cols, .funs = as.character.POSIXt)
    # append data frame to table
    dbWriteTable(con, table_name, x, append = TRUE)
  }
}
