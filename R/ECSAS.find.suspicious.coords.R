#'@export
#'@title Find ECSAS watches with suspicious coordinates and/or speed and time
#'
#'@description This function looks for watches whose length in km as calculated by dead-reckoning (speed * time) is more than \code{thresh} percent different 
#'than that calculated by using start and end positions.
#'@param thresh (as percentage; default 50) maximum that the two calculations can be different before being flagged as problematic. Note that, as of at least ECSAS 3.61,
#'watch duration is only recorded to the nearest minute
#'
#'@section Author:Dave Fifield
#'
#'@seealso \code{\link{ECSAS.extract}}
#'
ECSAS.find.suspicious.coords <- function(debug = FALSE){
  
  if(debug) browser()
}