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

  # Get channel
  ch <- ecsas.get.channel(ecsas.path, channel)
  
  # query.cruises <- paste("SELECT [q Cruise selector].CruiseID, ",
  #   "[q Cruise selector].[Start Date], [q Cruise selector].[End Date], ",
  #   "[q Cruise selector].Platform, [q Cruise selector].[Observer 1], ",
  #   "[q Cruise selector].[Observer 2], [q Cruise selector].[Start Port], ",
  #   "[q Cruise selector].[End Port], [q Cruise selector].Program, ",
  #   "[q Cruise selector].Company",
  #   " FROM [q Cruise selector]",
  #   " ORDER BY [q Cruise selector].[Start Date] DESC", sep = ""
  # )
  # Get everything the cruise selector returns
  query.cruises <- "SELECT [q Cruise selector].* FROM [q Cruise selector];"
  cruises <- RODBC::sqlQuery(ch$channel, query.cruises) %>% ensure_data_is_returned 
  
  # All ECSAS times are UTC but tzone will not be set when data is read from
  # ECSAS since Access has no timezone concept.
  # Set timezone for all POSIXct fields to be UTC without converting the wall
  # clock time.
  cruises <- cruises %>%
    dplyr::mutate(dplyr::across(
      .cols = names(.)[sapply(., inherits, what = "POSIXct")],
      .fns = ~ lubridate::force_tz(.x, "UTC")
    ))
  
  if (!ch$leave.open) RODBC::odbcClose(ch$channel)
  
  cruises
}