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

  # Get channel
  ch <- ecsas.get.channel(ecsas.path, channel)
  
  ret <- RODBC::sqlFetch(ch$channel, table) %>% ensure_data_is_returned 
  
  if(!ch$leave.open) RODBC::odbcClose(ch$channel)
  
  ret
}