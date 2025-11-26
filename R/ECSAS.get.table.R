#' @export
#'@title Get ECSAS table from database
#'
#'@description This function will connect to the Access database and return a 
#'table as a data frame.
#'
#'@param table character vector (of length 1) naming table to be returned.
#'@param ecsas.path (optional, default NULL) full path name to the ECSAS database.
#'#'@param channel (optional, default NULL) an existing open RODBC channel to the ECSAS database.
#'  Ignored if ecsas.path is provided. 
#' 
#'@section Author:Dave Fifield
#'
ECSAS.get.table <- function(table = NULL, ecsas.path = NULL, channel = NULL) {

  coll = checkmate::makeAssertCollection()
  checkmate::assert(checkmate::check_file_exists(ecsas.path),
                    checkmate::check_class(channel, "RODBC"),
                    add = coll)
  checkmate::assert_string(table, add = coll)
  checkmate::reportAssertions(coll)
  
  # close channel1 by default
  leave.open = FALSE
  
  if(!is.null(ecsas.path))
    channel1 <- RODBC::odbcConnectAccess2007(ecsas.path, uid="") 
  else if (!is.null(channel)){
    channel1 = channel
    leave.open = TRUE
  } else
    stop("ECSAS.get.table: could not open database.")
  
  ret <- RODBC::sqlFetch(channel1, table) %>% ensure_data_is_returned 
  if(!leave.open) RODBC::odbcClose(channel1)
  
  ret
}