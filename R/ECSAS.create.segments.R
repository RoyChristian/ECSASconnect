#' @export
#'@title Convert a series of continuous observations into segments
#'
#'@description This function takes a data frame of continuous observations and
#'  converts them into the segments Each row of the \code{dat} data frame
#'  contains either an observation, a PAUSE, or a RESUME record, Each
#'  transect is cut into segments (of \code{seglen} seconds each) beginning
#'  from the time of the transect start record. Thus the final watch in each
#'  transect may not be a full \code{seglen} seconds long.
#'
#'@param dat Input data frame of continuous observations. Can contain data for 
#'  multiple surveys. Must contain a transect column whose name is provided
#'  in \code{transcol}
#'@param watchlen The length in seconds of each segment to be created (default 30
#'  seconds).
#'@param posns The GPS track positions with one row per second.
#'
#'@param ecsas.path \[character, default: \sQuote{NULL}]\cr Full path name to 
#' the ECSAS database. Needed to extract the pause/resume info for aerial data.
#'
#'@details This is mostly used to convert aerial survey transects into segments
#'  for Density Surface Modeling and other analyses.
#'  
#'@return 
#'@section Author:Dave Fifield

ECSAS.create.aerial.watches <- function(dat, watchlen = 30, posns, ecsas.path) {

  # Get pause resume information
  pr <- ECSAS.get.table(ecsas.path = ecsas.path, "tblAerialPauseResume") %>% 
    dplyr::mutate(PauseDateTime = lubridate::force_tz(PauseDateTime, "UTC"),
           ResumeDateTime = lubridate::force_tz(ResumeDateTime, "UTC"))
  
  x <- dat %>%
    # XXXXXX REMEMBER TO REMOVE NEXT LINE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # dplyr::filter(AerialTransectID %in% pr$TransectID) %>%
    split(.$AerialTransectID) %>% 
    purrr::map_dfr(create.aerial.watch.by.transect, watchlen, posns, pr)
  x
}


# New idea: pass in pause/resume table and subset for current transect. 
# 
# Then
# insert "observation" records for each pause/resume with ALPHA == "PAUSE/RESUME" 
# StartTime == pause time and EndTime == resume tim
# 
# Then want to break the dataframe into contiguous chunks between each pause/resum.
# Do this by getting the row indices of all pause/resume records and use this as the
# breaks to cut the vector of dataframe row indices 1:nrow(dat). 
# 
# For each chunk 
# create n watches (new dataframe? with columns for start/end time and posn?)
# from start of chunk to end of chunk (think about how to get 
# these times - for the first chunk the starttime is transect start time, and end
# time is pause or end of transect if no pause/resume records. For next chunk,
# start time is resume time of first pause/resume record and end is either next 
# pause or end of transect, etc.
# Finally, assign the correct watch to each observation row by findind the watch 
# records whose start/end times include the observation. Need to think about 
# which watch an observation on the boundary belongs to. See new dplyr tweet 
# about  join_by helpers used with dates.
create.aerial.watch.by.transect <- function(dat, watchlen, posns, pr){
  
  transID <- unique(dat$AerialTransectID)
  stopifnot(length(transID) == 1)
 
  message("Doing transectID = ", transID) 
  transEnd <- unique(dat$EndDateTime)
  stopifnot(length(transEnd) == 1)
  
  transStart <- unique(dat$StartDateTime)
  stopifnot(length(transStart) == 1)

  # XXX Should check to ensure that there are no observations during off-effort.
  # Currently the code assumes this doesn't happen
  
  # Create and insert off-effort records for this transect, if any
  dat <- pr %>% 
    dplyr::filter(TransectID == transID) %>% 
    create.pr.obs(dat[1,]) %>% 
    rbind(dat) %>% 
    dplyr::arrange(ObsTime)

  # create a factor to identify each chunk of on-effort data
  # XXX Check corner cases - off-effort at start, or end, no off-effort data
  breaks <- which(dat$Alpha == "PAUSE")
  if (!(1 %in% breaks)) breaks <- c(1, breaks)
  if (!(nrow(dat) %in% breaks)) breaks <- c(breaks, nrow(dat))
  # Don't try to cut an data frame with just one row
  if (length(breaks) > 1) 
    dat <- dat %>% 
      dplyr::mutate(chunk = cut(1:nrow(.), breaks = breaks, include.lowest = TRUE))
  

  dat  
  # Now process each chunk and create watches for each one 
  # Deal with empty transect etc
}  

# create nrow(pr) "pause/resume" rows using template as a format
create.pr.obs <- function(pr, template) {
  # Need to nuke all columns of template except surveyID, & transectID before
  # assigning pause/resume start/end.
  template %>% 
    dplyr::mutate(dplyr::across(!any_of(c("AerialTransectID", "AerialSurveyID")), ~ NA)) %>% 
    magrittr::extract(rep(1, nrow(pr)), ) %>% 
    dplyr::mutate(StartDateTime = pr$PauseDateTime, 
                  ObsTime = pr$PauseDateTime, # Needed since obs will be sorted by this column
           EndDateTime = pr$ResumeDateTime,
           Alpha = "PAUSE/RESUME")
}


# OLD: For each row we need to add a watchid identifying the watch, a start 
# time and in time for the watch, and the start position and end position for
#  the watch
