#' @export
#'@title Create transect lines from ECSAS watches
#'
#'@description This function aggregates consecutive ECSAS watches into transects. 
#'@param dat  a dataframe of ECSAS watches (1 row per watch). 
#'
#'@param angle.thresh - (default 90 degrees) changes in direction of more than this are considered to start new transects
#'
#'@details
#'Combine watches into transects and create a spatiallinesdataframe for each transect in dat. Transects are defined as all 
#'consecutive watches by the same observer on the same vessel in the same direction on the same day.
#'
#'rows in \code{dat} must contain at least columns named \code{ObserverName}, \code{PlatformDir},
#'\code{PlatformName}, \code{Date}, \code(StartTime), \code{EndTime}, \code{LatStart}, \code{LongStart}, \code{LatEnd}, 
#'\code{LongEnd} as returned by \code{ECSAS.extract}.  \code{LatEnd} and \code{LongEnd} are only necessary for the last watch in
#'in each transect to provide it's endpoint. If either is missing, the end point will be projected along a loxodrome based 
#'on the watch start location, \Code{PlatformDir} (or \code{PlatformDirDeg} if it exists), \code{PlatformSpeed}, and \code{ObsLen}. If either of these is missing then an error
#'is produced.
#'@section Author:Dave Fifield
#'
#'@seealso \code{\link{ECSAS.extract}}
#'
if(FALSE) {


ECSAS.create.transects <- function(dat, angle.thresh = 90, debug = FALSE){
  
  if(debug) browser()
  
  if(is.empty(dat$ObserverName) || is.empty(dat$Direction) || is.empty(dat$Vessel))
    stop("Something is empty")
  
  # note that n.walshs "watches" are already transects - so just assign a unique ID.
  dat$tr.id <- NA
  dat[dat$Observer == "N Walsh",]$tr.id <- group_indices(dat[dat$Observer == "N Walsh",], WatchID)
  
  # sort
  dat %<>% arrange(Observer, Vessel, StartTime)
  
  #init
  tr.id <- max(dat$tr.id, na.rm = T) + 1
  cur.obs <- dat$Observer[1]
  cur.dir <- dat$Direction[1]
  cur.ves <- dat$Vessel[1]
  cur.day <- date(dat$StartTime[1])
  
  # start assigning tr.id until one of day, observer, direction or vessel changes
  for (i in 1:nrow(dat)){
    row <- dat[i,]
    
    if(row$Observer == "N Walsh")
      next
    
    # start new transect?
    # if(cur.obs != row$Observer ||abs((as.numeric(as.character(row$Direction)) - 
    #                                   as.numeric(as.character(cur.dir)))) %% 360 > angle.thresh 
    #    || cur.ves != row$Vessel || cur.day != date(row$StartTime)){
    
    if(cur.obs != row$Observer ||cur.dir != row$Direction || cur.ves != row$Vessel || cur.day != date(row$StartTime)){
      tr.id <- tr.id + 1
      cur.obs <- row$Observer
      cur.dir <- row$Direction
      cur.ves <- row$Vessel
      cur.day <- date(row$StartTime)
    }  
    
    if(is.na(dat[i, "tr.id"]))
      dat[i, "tr.id"] <- tr.id
  }
  
  # add a final point at end of each transect
  dat <- split(dat, dat$tr.id) %>% 
    map_df(add_final_point) %>% 
    mutate(save.lat = Latitude.Start,
           save.long = Longitude.Start) # save from removal by coordinates()
  
  # create lines object
  
  coordinates(dat) <- ~ Longitude.Start + Latitude.Start
  
  lines <- dat %>% 
    split(dat$tr.id) %>% 
    lapply(function(x) Lines(list(Line(coordinates(x))), x$tr.id[1L])) %>% 
    SpatialLines()
  
  # create data for each transect and create spatialLinesDataFrame
  data <- dat@data %>% 
    group_by(tr.id) %>% 
    summarize(Date = unique(date(StartTime)),
              Start = min(as.POSIXct(StartTime, format="%H:%M:%S")),
              End = max(as.POSIXct(EndTime, format="%H:%M:%S")),
              Observer = unique(Observer),
              Vessel = unique(Vessel),
              Totlen = sum(TLength.km),
              Direction = paste(unique(Direction), collapse = ", "),
              #Watches = paste(unique(WatchID), collapse = ", "),
              Watches = list(unique(WatchID)),
              Latitude.Start = first(save.lat),
              Longitude.Start = first(save.long),
              Latitude.End = last(save.lat), # "Start" location of last point in line is actually the end of the transect
              Longitude.End = last(save.long)
    ) %>% 
    as.data.frame()
  
  rownames(data) <- data$tr.id
  l <- SpatialLinesDataFrame(lines, data)
  proj4string(l) <- CRS(latlongproj)
  l
}


}