#' @export
#'@title Get ECSAS table from database
#'
#'@description This function will connect to the Access database and return a 
#'table as a data frame.
#'
#'@param table character vector (of length 1) naming table to be returned.
#'@param ecsas.path (default NULL) full path name to the ECSAS database. 
#' 
#'@section Author:Dave Fifield
#'
ECSAS.get.table <- function(table = NULL, ecsas.path = NULL) {

  coll = checkmate::makeAssertCollection()
  checkmate::assert_file_exists(ecsas.path, add = coll)
  checkmate::assert_string(table, add = coll)
  checkmate::reportAssertions(coll)
  
  channel1 <- RODBC::odbcConnectAccess2007(ecsas.path, uid="") 
  
  ret <- RODBC::sqlFetch(channel1, table) %>% ensure_data_is_returned 
  RODBC::odbcClose(channel1)
  
  ret
}