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
#'@details This is mostly used to convert aerial survey transects into segments
#'  for Density Surface Modeling and other analyses.
#'  
#'@return 
#'@section Author:Dave Fifield





ECSAS.create.aerial.watches <- function(dat, watchlen = 30, posns) {
  x <- dat %>% 
    split(dat$AerialTransectID) %>% 
    purrr::map_dfr(create.aerial.watch.by.transect, watchlen, posns)
}


# New idea: pass in pause/resume table and subset for current transect. Then
# insert "observation" records for each pause/resume with ALPHA == "PAUSE/RESUME" 
# StartTime == pause time and EndTime == resume tim
# Then want to break the dataframe into contiguous chunks between each pause/resum.
# Do this by getting the row indices of all pause/resume records and use this as the
# breaks to cut the vector of dataframe row indices 1:nrow(dat). For each chunk 
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

# OLD: For each row we need to add a watchid identifying the watch, a start 
# time and in time for the watch, and the start position and end position for
#  the watch
create.aerial.watch.by.transect <- function(dat, watchlen, posns){
  # Start of the 1st watch is the start date time from the transect, the end of
  # the last watch is the end date time of the transect
  
  outnum <- 1 # Ugh! Growing a dataframe in a loop!
  out <- dat[0,] # gets Empty data frame with the same structure
  
  transID <- unique(dat$AerialTransectID)
  stopifnot(length(transID) == 1)
  
  transEnd <- unique(dat$EndDateTime)
  stopifnot(length(transEnd) == 1)

  curstart <-  unique(dat$StartDateTime) # start of current watch
  stopifnot(length(curstart) == 1)
  curend <- get.curend(curstart, watchlen, transEnd)
    
  # Deal with an empty transect
  if (nrow(dat) == 1 && is.na(dat$ObsTime)) {
    out <- insert.empty.watches(starttime = curstart, endtime = curend,
                                out = out, watchnum = 1, transID = transID,
                                allowPartial = T)
  } else {
    # Sort by time
    dat <- dplyr::arrange(dat, ObsTime)  
    
    
    # curstartpos <- lookup.posn(....) 
  
    watchnum <- 1 # Incremented and appended to transect ID to give watchID
    for (rownum in 1:nrow(dat)) {
  
      # If the time of the current observation row is greater than current
      # watch end time then insert appropriate number of empty watches and
      # update variables
      # 
      # 
      if (dat[rownum,]$ObsTime > curend) {
        # Insert appropriate number of empty watches and update curstart and
        # watchnum, and curend
        
        # insert empty watches. If this is the first watch of the transect
        # then start with watch num 1, otherwise it's watchnum + 1
        res <- insert.empty.watches(starttime = dplyr::if_else(rownum == 1, 
                                                               curstart, curend), 
                                    endtime = dat[rownum,]$ObsTime,
                                    template = dat[1,],
                                    watchlen = watchlen,
                                    out = out, 
                                    watchnum = dplyr::if_else(rownum == 1, watchnum,
                                                       watchnum + 1), 
                                    transID = transID,
                                    allowPartial = FALSE)
        out <- res$out
        watchnum <- res$watchnum # next watch number
        outnum <- nrow(out) + 1
        curstart <- out[nrow(out),]$EndTime # end of last watch is new start
        curend <- get.curend(curstart, watchlen, transEnd)
      }
      
      
      # Deal with pause and resume
      if (FALSE) {
        # Need to modify the end time for any existing rows for this watch 
      } else {
        # Observation row, assign watchID, etc.
        # But how will we know what the end time is. Just add watch Len I guess 
        # and if we strike another pause/resume then will have to go back and find
        #  all the records with this watch ID and fix the end time.
        # Also watch out for end of transect being within the next watchlen minutes
        out[outnum,] <- dat[rownum,]
        out$WatchID <- paste0(transID, watchnum, sep = "_")
        out$StartTime <- curstart
        out$EndTime <- curend
        # out$StartLat <- 0
        # out$StartLong <- 0
        # out$EndLat <- 0
        # out$EndLong <- 0
        outnum <- outnum + 1
      }
    }
  }
}

# Determine the end time for a transect starting at curstart taking the transect 
# end time into account
get.curend <- function(curstart, watchlen, transEnd) {
    # deal with end of transect time 
    tempend <- curstart + lubridate::seconds(watchlen)
    dplyr::if_else(tempend <= transEnd, tempend, transEnd)
}

# Create n.need empty watches each of watchlen seconds starting at starttime.
# Add them to the end of out using outnum as an index, creating the watch ID
# from transId and watchnum.
#
# Return the new out that the new value of watchnum incremented
insert.empty.watches <- function(starttime, endtime, watchlen, template, out, 
                                 watchnum, transID,
                                 allowPartial){
  n.need <- floor(lubridate::interval(starttime, endtime)/lubridate::seconds(watchlen)) +
    as.numeric(allowPartial)
  emprow <- template %>% 
    dplyr::mutate(dplyr::across(AerialObsID|Observer|ObsLat|ObsLong|ObsAlt|ObsTime|Alpha|
                    English|Count|FlySwim|Distance|Association|Behaviour|
                    Age|Plumage|Sex|Notes|Distance_m|InTransect, ~ NA))
  
  emprows <- emprow[rep(1, n.need),]
  # set watchID, start and end times for each inserted empty watch  
  emprows$WatchID <- paste0(transID, watchnum:(watchnum + n.need - 1), sep = "_")
  out$StartTime <- curstart
  out$EndTime <- curend

  
  out <- rbind(out, )

  
  
  list(out = out, watchnum = watchnum + n.need)
}
