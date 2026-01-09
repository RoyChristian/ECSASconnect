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
    
  # Get channel
  ch <- ecsas.get.channel(ecsas.path, channel)
    
  # Execute query   
  res <-  RODBC::sqlQuery(ch$channel, sqlstr) 
    
  #close connection
  if (!ch$leave.open) RODBC::odbcClose(ch$channel)
  
  res
}