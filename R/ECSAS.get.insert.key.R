#'@export
#'@title Get the primary key of last record inserted.
#'
#'@description This function will connect to the Access database, execute a query and 
#'  return the result.
#'
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
ECSAS.get.insert.key <- function(channel){
  ret <- RODBC::sqlQuery(channel, "SELECT @@IDENTITY;") 
  checkmate::assert_data_frame(ret, nrows = 1)
  ret[1,1]
}
