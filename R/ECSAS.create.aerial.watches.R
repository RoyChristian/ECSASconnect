#'@export
#'@title Create watches from aerial transects
#'
#'@description This function converts aerial transect data into watches
#'  (ie segments). 
#'  
#'
#'@param dat Input data frame of transect info. Can contain data for 
#'  multiple surveys. Must contain at least columns for transect ID 
#'  (\code{AerialTransectID}), and transect start/end date and time 
#'  (\code{StartDateTime}, \code{EndDateTime}). All other columns are ignored.
#'  These columns need not be unique between rows (eg. if \code{dat} contains the
#'  actual animal observations). Watches will only be created for
#'  each unique transect.
#'
#'@param watchlen The length in seconds of each watch to be created (default 30
#'  seconds).
#'  
#'@param posns The GPS track positions with one row per second. \code{posns} must
#' contain columns \code{datetime, lat, long}, and \code{alt}, the latter of which
#' may contain \code{NA}.
#'
#'@param ecsas.path Full path name to the ECSAS database. Needed to extract the 
#'    pause/resume info for aerial data. Default \code{NULL}.
#' 
#'@param verbose If \code{TRUE}, the \code{AerialTransectID} of each transect
#'  is printed as it is processed. Default \code{FALSE}.
#'  
#'@details This is mostly used to convert aerial survey transects into segments
#'  for Density Surface Modeling and other analyses. Periods of off-effort during
#'  transects (e.g., while passing over islands) will be ignored. Off-effort info
#'  is extracted directly from the database. Start and end positions of each
#'  watch are assigned from \code{posns}, if supplied, based on matching time.  
#'  A warning is issued if any start/end positions cannot be assigned.
#'  
#'  The final watch in each transect (or before an off-effort break) may not be
#'  a full \code{watchlen} seconds long.
#'  
#'@return A dataframe of watches.
#'@section Author:Dave Fifield
ECSAS.create.aerial.watches <- function(dat, 
                                        watchlen = 30, 
                                        posns = NULL, 
                                        ecsas.path, 
                                        verbose = FALSE) {

  # Get pause resume information
  pr <- ECSAS.get.table(ecsas.path = ecsas.path, "tblAerialPauseResume") %>% 
    dplyr::mutate(PauseDateTime = lubridate::force_tz(PauseDateTime, "UTC"),
           ResumeDateTime = lubridate::force_tz(ResumeDateTime, "UTC"))
  
  # Create watches
  watches <- dat %>%
    split(.$AerialTransectID) %>%
    purrr::map_dfr(create.aerial.watch.by.transect, watchlen, posns, pr, verbose)

  # Assign positions to start and end of watch
  if (!is.null(posns)) {
    watches <- watches %>% 
      dplyr::left_join(posns, by = c("WatchStartTime" = "datetime")) %>% 
      dplyr::rename(LatStart = lat, LongStart = long, Altitude = alt) %>% 
      dplyr::left_join(posns, by = c("WatchEndTime" = "datetime")) %>% 
      dplyr::rename(LatEnd = lat, LongEnd = long) %>% 
      dplyr::select(-alt)

    
    # add all gps points for each watch as a list column or dataframe col.
    poslist <- watches %>% 
      split(1:nrow(.)) %>% 
      map(get.watch.posns, posns = posns)
    
    watches <- watches %>% 
      mutate(posns = poslist)
  }

  # Calc WatchLenKM
  watches <- watches %>% 
    dplyr::mutate(WatchLenKm = geosphere::distGeo(cbind(LongStart, LatStart), 
                                           cbind(LongEnd,  LatEnd))/1000,
           CalcDurMin = as.integer(WatchEndTime - WatchStartTime)/60)
  
  # Check for missing positions
  if (any(is.na(watches$LatStart)) || any(is.na(watches$LatEnd))) {
    message(sprintf("ECSAS.create.aerial.watches: Warning: positions could not be found for some watches"))
  }
  
  watches
}


get.watch.posns <- function(watch, posns) {
  ret <-
    filter(posns,
           between(datetime, watch$WatchStartTime, watch$WatchEndTime))
  ret
}

# Create the watches for a given transect.
#
# Pass in pause/resume table and subset for current transect.
#
# Create a dataframe of bounds that defines the times of chunks of on-effort
# separated by pause/resumes.
#
# For each chunk create n watches with columns for start/end time from start of
# chunk to end of chunk. For the first chunk the starttime is transect start
# time, and end time is pause or end of transect if no pause/resume records. For
# next chunk, start time is resume time of first pause/resume record and end is
# either next pause or end of transect, etc.
create.aerial.watch.by.transect <- function(dat, 
                                            watchlen, 
                                            posns = NULL, 
                                            pr, 
                                            verbose = FALSE){
  
  transID <- unique(dat$AerialTransectID)
  stopifnot(length(transID) == 1)
 
  if (verbose)
    message("Doing transectID = ", transID) 
  
  transEnd <- unique(dat$EndDateTime)
  stopifnot(length(transEnd) == 1)
  
  transStart <- unique(dat$StartDateTime)
  stopifnot(length(transStart) == 1)

  # Create expanded pause/resume records for this transect, if any
  pr.exp <- pr %>% 
    dplyr::select(TransectID, PauseDateTime, ResumeDateTime) %>% 
    dplyr::filter(TransectID == transID) %>% 
    expand.pr() %>% 
    dplyr::arrange(DateTime)

  # add an additional start/end row
  startrow <- dat[1,] %>% 
    dplyr::select(AerialTransectID) %>% 
    dplyr::mutate(DateTime = transStart, 
                  type = "START")
  endrow <- dat[1,] %>% 
    dplyr::select(AerialTransectID) %>% 
    dplyr::mutate(DateTime = transEnd, 
                  type = "END")
  bounds <- rbind(startrow, pr.exp, endrow) %>% 
    dplyr::arrange(DateTime) # just in case

  # Create breaks to cut dat up into chunks between pause/resumes including
  # first and last row indices
  breaks <- which(bounds$type == "PAUSE") %>% 
    c(1, nrow(bounds)) %>% 
    unique()

  stopifnot(length(breaks) > 1)

  # Add a factor to identify each chunk of data. Bounds
  # is just a few rows indicating start/end times of chunks 
  # There are no actual observations here.
  bounds <- bounds %>% 
    dplyr::mutate(chunk = cut(1:nrow(.), breaks = breaks, include.lowest = TRUE))

  # Create watches for each transect chunk
  watches <- bounds %>% 
    split(.$chunk) %>% 
    purrr::map_dfr(create.chunk.watches, watchlen, transID)
  
  watches
}  

# create nrow(pr) "pause/resume" rows using template as a format
expand.pr <- function(pr) {
  template <- dplyr::tribble(~AerialTransectID, unique(pr$TransectID))
  
  pauses <- template[rep(1, nrow(pr)), ] %>% # 1 row per pr row
    dplyr::mutate(DateTime = pr$PauseDateTime, 
                  type = "PAUSE")

  resumes <- template[rep(1, nrow(pr)), ] %>% # 1 row per pr row
    dplyr::mutate(DateTime = pr$ResumeDateTime, 
                  type = "RESUME")
  
  rbind(pauses, resumes)
}

# Create the watches for each chunk.
create.chunk.watches <- function(chunk, watchlen, transID){
  stopifnot(nrow(chunk) == 2)
  
  n.need <- ceiling(lubridate::interval(chunk$DateTime[1], chunk$DateTime[2])/
                    lubridate::seconds(watchlen))
  template <- dplyr::tribble(~AerialTransectID, unique(chunk$AerialTransectID))
  
  # set watchID, start and end times for each watch  
  watches <- template[rep(1, n.need),] %>% 
    dplyr::mutate(WatchID = paste(transID, chunk$chunk[1], 1:n.need, sep = "_"),
                  WatchStartTime = chunk$DateTime[1] + 
                    0:(nrow(.)-1) * lubridate::seconds(watchlen),
                  WatchEndTime = WatchStartTime + lubridate::seconds(watchlen)
    )
  
  # set final watch end time
  watches[nrow(watches), "WatchEndTime"] <- chunk[2, "DateTime"]
  watches
}
