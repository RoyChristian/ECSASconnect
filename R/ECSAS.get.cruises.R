#' @export
#'@title Get ECSAS cruises  from database
#'
#'@description This function will connect to the Access database and return a 
#'data frame containing info about all cruises.
#'
#'@param ecsas.path (required) full path name to the ECSAS database. 
#' 
#'@section Author:Dave Fifield
#'
ECSAS.get.cruises <- function(ecsas.path = NULL) {

  checkmate::assert_file_exists(ecsas.path)
  
  channel1 <- RODBC::odbcConnectAccess2007(ecsas.path, uid="") 
  
  query.cruises <- paste("SELECT [q Cruise selector].CruiseID, ",
    "[q Cruise selector].[Start Date], [q Cruise selector].[End Date], ",
    "[q Cruise selector].Platform, [q Cruise selector].[Observer 1], ",
    "[q Cruise selector].[Observer 2], [q Cruise selector].[Start Port], ",
    "[q Cruise selector].[End Port], [q Cruise selector].Program, ",
    "[q Cruise selector].Company",
    " FROM [q Cruise selector]",
    " ORDER BY [q Cruise selector].[Start Date] DESC", sep = ""
  )
  cruises <- RODBC::sqlQuery(channel1, query.cruises) %>% ensure_data_is_returned 
  RODBC::odbcClose(channel1)
  
  cruises
}