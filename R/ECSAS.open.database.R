#' @export
#'@title Return an open RODBC channel to an ECSAS database
#'
#'@description This function will connect to the Access database and return a 
#'channel object from \code{ RODBC::odbcConnectAccess2007} suitable for use in
#'other database operation.
#'
#'@param ecsas.path (required) full path name to the ECSAS database. 
#' 
#'@section Author:Dave Fifield
#'
ECSAS.open.database <- function(ecsas.path = NULL) {
  
  checkmate::assert_file_exists(ecsas.path)
  
  RODBC::odbcConnectAccess2007(ecsas.path, uid="") 
}

#' @export
#'@title Close an open RODBC channel to an ECSAS database
#'
#'@description This function close a channel returned by [ECSAS.open.database]
#'
#'@param channel (required) an open RODBC channel 
#' 
#'@section Author:Dave Fifield
#'
ECSAS.open.database <- function(channel) {
  
  RODBC::odbcClose(channel) 
}

