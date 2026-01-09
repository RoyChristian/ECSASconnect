#'@export
#'@title Insert record into ECSAS table 
#'
#'@description This function inserts a single record into and ECSAS database table
#'
#'@param table \[charcter, required]\cr The name of the table to insert the
#'  record into.
#'@param dat \[dataframe, required]\cr A single row data frame providing the
#'  data to be inserted.
#'#'@param ecsas.path \[character, optional]\cr Full path name to the ECSAS
#'  database. 
#'@param channel \[optional]\cr An existing open RODBC channel to the ECSAS database.
#'
#'@details
#'  Column names in \code{dat} must match those in the ECSAS database table exactly.
#'  There must be a column for every required table field (unless the field has
#'  a default value in the database). Optional ECSAS table fields with no
#'  matching column in \code{dat} and no default value will be initialized to
#'  \code{NULL}.
#'  
#'  If both valid \code{ecsas.path} and \code{channel} are provided, \code{channel}
#'  takes precedence and is left open upon return. If only \code{ecsas.path}
#'  is provided, then a new connection is made to the database at that location
#'  which is closed upon return.
#'  
#'@return 
#'  The value of the autonumber field for the newly inserted row or \code{NA} if
#'  there isn't one.
#'
#'@section Author:Dave Fifield
#'
ECSAS.insert.table.row <- function(table, dat, ecsas.path = NULL, channel = NULL){
  
  # check args
  coll = checkmate::makeAssertCollection()
  checkmate::assert(
    checkmate::check_character(table, min.chars = 1),
    checkmate::check_data_frame(dat, nrow = 1),
    add = coll
  )
  checkmate::reportAssertions(coll)

  # Get channel
  ch <- ecsas.get.channel(ecsas.path, channel)
  
  # Create SQL and insert
  sql.frag <- get_sql_insert_fragment(dat)
  sqlstr <-  paste0("INSERT INTO ", table, " (", sql.frag$fields, 
                    ") VALUES (", sql.frag$values, ");")
  id <- access_insert_return_id(ch$channel, sqlstr)
  
  if(!ch$leave.open) RODBC::odbcClose(ch$channel)
  
  invisible(id)
}


# Function to produce SQL INSERT field/value strings from a named list
# or dataframe.
get_sql_insert_fragment <- function(cols) {
  if (is.data.frame(cols) & isTRUE(nrow(cols) > 1)) 
    stop("get_sql_insert_fragment: Input dataframe has more than one column")
  if (length(cols) == 0) 
    stop("get_sql_insert_fragment: Input list is empty")
  if (is.null(names(cols))) 
    stop("get_sql_insert_fragment: Input list must be named")
  
  # --- Field names in square brackets ---
  fields <- paste0("[", names(cols), "]", collapse = ", ")
  
  # --- Convert values to SQL-safe forms ---
  values <- paste0(
    sapply(cols, handle.col.value),
    collapse = ", "
  )
  
  list(fields = fields, values = values)
}



# handle one column (or named list field) converting to SQL 
handle.col.value <- function(x) {
  
  # --- Character values ---
  if (is.character(x)) {
    x <- gsub("'", "''", x)   # escape single quotes
    return(paste0("'", x, "'"))
  }
  
  # --- Numeric / integer values ---
  if (is.numeric(x) || is.integer(x)) {
    return(as.character(x))
  }
  
  # --- Logical values (Access expects -1 = TRUE, 0 = FALSE) ---
  if (is.logical(x)) {
    if (is.na(x)) return("NULL")
    return(if (x) "-1" else "0")
  }
  
  # --- POSIXct date/time values ---
  if (inherits(x, "POSIXct")) {
    if (is.na(x)) return("NULL")
    # Format as Access datetime literal
    dt <- format(x, "%Y-%m-%d %H:%M:%S")
    return(paste0("#", dt, "#"))
  }
  
  # --- POSIXct date/time values ---
  if (inherits(x, "Date")) {
    if (is.na(x)) return("NULL")
    # Format as Access datetime literal
    dt <- format(x, "%Y-%m-%d")
    return(paste0("#", dt, "#"))
  }
  
  # --- lubridate hms values ---
  if (inherits(x, "hms")) {
    if (is.na(x)) return("NULL")
    # Format as Access datetime literal
    dt <- format(x, "HH:MM:SS")
    return(paste0("#", dt, "#"))
  }
  
  # --- Unsupported types ---
  stop("handle.col.value: Unsupported column type: ", paste(class(x), collapse = ", "))
  
}
