#' @export
#'@title Get ECSAS cruises  from database
#'
#'@description This function will connect to the Access database and return a 
#'data frame containing info about all cruises.
#'
#'@param ecsas.path (optional) full path name to the ECSAS database. 
#'@param channel (optional) an existing open RODBC channel to the ECSAS database.
#'  Ignored if ecsas.path is provided.
#'@section Author:Dave Fifield
#'
ECSAS.get.cruises <- function(ecsas.path = NULL, channel = NULL) {

  checkmate::assert(checkmate::check_file_exists(ecsas.path),
                    checkmate::check_class(channel, "RODBC"),
                    add = coll)
  
  # close channel1 by default
  leave.open = FALSE
  
  if(!is.null(ecsas.path))
    channel1 <- RODBC::odbcConnectAccess2007(ecsas.path, uid="") 
  else if (!is.null(channel)){
    channel1 = channel
    leave.open = TRUE
  } else
    stop("ECSAS.get.cruises: could not open database.")
  
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
  if (!leave.open) RODBC::odbcClose(channel1)
  
  cruises
}