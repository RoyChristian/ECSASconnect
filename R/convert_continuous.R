#'@title Convert a series of continuous aerial observations
#'
#'@description This function takes a data frame of continuous observations and
#'  converts them into the transect/watch/observation structure required by
#'  ECSAS. Each row of the \code{cont} data frame contains either an
#'  observation, a transect start/end record, or a weather update record. Each
#'  transect is cut into watches (of \code{watchlen} minutes each) beginning
#'  from the time of the transect start record. Thus the final watch in each
#'  transect may not be a full \code{watchlen} minutes long.
#'
#'@param cont Input data frame of continuous observations
#'@param watchlen The links in minutes of each watch to be created (default 1
#'  minute).
#'
#'@details I envision this being called after creating a \code{tblAerialSurvey}
#'  record and before transects, watches and observations are added to the
#'  appropriate tables. Or, perhaps instead, it should do the insertion
#'  on-the-fly as it goes. I wonder what's easier…
#'@return Returns a list with elements \code{transects, watches}, and
#'  \code{observations} suitable for insertion into the ECSAS database using
#'  \code{ECSAS.??????()}.
#'@section Author:Dave Fifield


convert_continuous_to_watches <- function(cont, watchlen = 1) {
  
}