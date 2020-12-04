#' @export
#'@title Get ECSAS table from database
#'
#'
#'@description This function will connect to the Access database and return a table as a data frame.
#'
#'@param table character vector (of length 1) naming table to be returned.
#'@param ecsas.path (default NULL) full path name to the ECSAS database. 
#' 
#'@section Author:Dave Fifield
#'
ECSAS.get.table <- function(table = NULL, ecsas.path = NULL, debug = FALSE) {

  coll = checkmate::makeAssertCollection()
  checkmate::assert_file_exists(ecsas.path, add = coll)
  checkmate::assert_string(table, add = coll)
  checkmate::assert (
    checkmate::check_null(debug),
    checkmate::check_logical(debug, len = 1),
    add = coll
  )
  checkmate::reportAssertions(coll)
  
  browser(expr = debug)
  # test for 32-bit architecture
  if (Sys.getenv("R_ARCH") != "/i386")
    stop("You are not running a 32-bit R session. You must run ECSAS.extract in a 32-bit R session due to limitations in the RODBC Access driver.")

  channel1 <- RODBC::odbcConnectAccess(ecsas.path, uid="") 
  
  ret <- RODBC::sqlFetch(channel1, table) %>% ensure_data_is_returned 
  RODBC::odbcClose(channel1)
  
  ret
}