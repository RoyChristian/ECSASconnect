#'@export
#'@title Execute an SQL query on an ECSAS database
#'
#'@description This function will connect to the Access database, execute a query and 
#'  return the result.
#'
#'@param ecsas.path \[character]\cr Full path name to the ECSAS
#'  database. 
#'@param channel \[optional]\cr An existing open RODBC channel to the ECSAS database.
#'  Ignored if ecsas.path is provided.
#'@param sqlstr SQL string to execute 
#'
#'@details
#'  Opens a connection to the database and executes the query given in \code{sqlstr}.
#'@return 
#'  Returns whatever the query returned.
#'
#'@section Author:Dave Fifield
#'

ECSAS.run.query <-  function(ecsas.path = NULL, sqlstr, channel = NULL) {
  

  # check args
  coll = checkmate::makeAssertCollection()
    checkmate::assert(
    checkmate::check_character(sqlstr, min.chars = 1),
    add = coll
  )
  checkmate::assert(checkmate::check_file_exists(ecsas.path),
                    checkmate::check_class(channel, "RODBC"),
                    add = coll)
  checkmate::reportAssertions(coll)

  # close channel1 by default
  leave.open = FALSE
  
  ### open connection. This should work on both 32-bit R and 64-bit R as long as
  # user has both 32-bit and 64-bit Access drivers installed. See 
  # https://stackoverflow.com/questions/45064057/reading-data-from-32-bit-access-db-using-64-bit-r
  # and https://www.microsoft.com/en-US/download/details.aspx?id=13255
  if(!is.null(ecsas.path))
    channel1 <- RODBC::odbcConnectAccess2007(ecsas.path, uid="") 
  else if (!is.null(channel)){
    channel1 = channel
    leave.open = TRUE
  } else
    stop("ECSAS.get.cruises: could not open database.")
  
  # Execute query   
  res <-  RODBC::sqlQuery(channel1, sqlstr) 
    
  #close connection
  if (!leave.open) RODBC::odbcClose(channel1)
  
  res
}