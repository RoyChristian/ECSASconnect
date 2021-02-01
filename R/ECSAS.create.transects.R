#'@export
#'@title Create transect lines from ECSAS watches
#'
#'@description This function aggregates consecutive ECSAS watches into
#'  transects.
#'@param dat a dataframe of ECSAS watches (1 row per watch).
#'@param angle.thresh (default 90 degrees) changes in direction of more than
#'  this are considered to start new transects (currently not implemented)
#'@param max.lag (default 10 minutes) the maximum time lag allowed between
#'  consecutive watches to be considered part of the same transect.
#'@param CRS if not null, the output coordinate system will be set CRS via
#'  \code{sp::spTransform}
#'@param debug logical (default FALSE), should the debugger, [base::browser()],
#'  be used
#'@param debug.watch watchid to stop at in for loop if debug is true
#'
#'@details Combine watches into transects and create SpatialLinesDataFrame for
#'each transect in \code{dat}. Transects are defined as all consecutive watches
#'by the same observer on the same vessel in the same direction on the same day.
#'
#'Rows in \code{dat} must contain at least columns named \code{ObserverName},
#'\code{PlatformDir}, \code{PlatformName}, \code{Date}, \code{StartTime},
#'\code{EndTime}, \code{LatStart}, \code{LongStart}, \code{LatEnd},
#'\code{LongEnd} as returned by \code{\link{ECSAS.extract()}}.  \code{LatEnd} and
#'\code{LongEnd} are only necessary for the last watch in each transect to
#'provide it's endpoint. If either is \code{NA}, the end point will be projected
#'along a loxodrome based on the watch start location, \code{PlatformDir} (or
#'\code{PlatformDirDeg} if it exists), \code{PlatformSpeed}, and \code{ObsLen}.
#'If either of these is missing then an error is produced.
#'@section Author:Dave Fifield
#'
#'@seealso \code{\link{ECSAS.extract}}
#'  
#'@importFrom magrittr `%>%` `%<>%`
ECSAS.create.transects <- function(dat, angle.thresh = NULL, max.lag = 10, debug = FALSE, CRS = NULL, 
                                   debug.watch = NULL){
  
  if(debug) browser()
  
  if(!is.null(angle.thresh))
    stop("In ECSAS.create.transects: angle.thresh is not yet implemented.")
  
  # xxxx may have to convert platform dir to numeric to implement angle.thresh comparison
  # xxxx use platformdirdeg if it exists????
  
  if(is.empty(dat$ObserverName) || is.empty(dat$PlatformDir) || is.empty(dat$PlatformName))
    stop("Something is empty")
  
  # sort and get time lag between consecutive rows
  dat %<>% dplyr::arrange(ObserverName, PlatformName, Date, StartTime) %>% 
    dplyr::mutate(timelag = lubridate::time_length(StartTime - dplyr::lag(EndTime), unit = "minutes"))
  
  #init
  dat$Sample.Label <- NA_integer_
  Sample.Label <- 1L
  cur.obs <- dat$ObserverName[1]
  cur.dir <- dat$PlatformDir[1]
  cur.ves <- dat$PlatformName[1]
  cur.day <- dat$Date[1]
  
  # start assigning Sample.Label until one of day, observer, direction or vessel changes
  for (i in 1:nrow(dat)){
    row <- dat[i,]
    
    browser(expr = debug && !is.null(debug.watch) && row$WatchID == debug.watch)

    # start new transect?
    # if(cur.obs != row$Observer ||abs((as.numeric(as.character(row$Direction)) - 
    #                                   as.numeric(as.character(cur.dir)))) %% 360 > angle.thresh 
    #    || cur.ves != row$Vessel || cur.day != date(row$StartTime)){

    # Start new transect?    
    if(cur.obs != row$ObserverName || cur.dir != row$PlatformDir || cur.ves != row$PlatformName ||
       cur.day != row$Date ||
       (!is.na(row$timelag) && row$timelag > max.lag)){
      Sample.Label <- Sample.Label + 1L
      cur.obs <- row$ObserverName
      cur.dir <- row$PlatformDir
      cur.ves <- row$PlatformName
      cur.day <- row$Date
    }  
    
    # Fill in Sample.Label for this row.
    if(is.na(dat[i, "Sample.Label"]))
      dat[i, "Sample.Label"] <- Sample.Label
  }

  if(debug) browser()
  
  # add a final point at end of each transect
  dat <- split(dat, dat$Sample.Label) %>% 
    purrr::map_df(add.final.point) %>% 
    dplyr::mutate(save.lat = LatStart,
           save.long = LongStart) # protect from removal by coordinates(), not needed if add.final.point is updated to use SpatialPointsDataFrame to create instead of coordinates()???

  # create lines object
  sp::coordinates(dat) <- ~ LongStart + LatStart
  
  lines <- dat %>% 
    sp::split(dat$Sample.Label) %>% 
    lapply(function(x) sp::Lines(list(sp::Line(sp::coordinates(x))), x$Sample.Label[1L])) %>% 
    sp::SpatialLines()
  
  # create data row for each transect and create spatialLinesDataFrame
  data <- dat@data %>% 
    dplyr::group_by(Sample.Label) %>% 
    dplyr::summarize(
              CruiseID = unique(CruiseID),
              Date = unique(Date),
              StartTime = min(as.POSIXct(StartTime, format="%H:%M:%S")),
              EndTime = max(as.POSIXct(EndTime, format="%H:%M:%S")),
              ObserverName = unique(ObserverName),
              PlatformName = unique(PlatformName),
              Effort = sum(WatchLenKm),
              PlatformDir = paste(unique(PlatformDir), collapse = ", "),
              Watches = list(unique(WatchID)),
              LatStart = dplyr::first(save.lat),
              LongStart = dplyr::first(save.long),
              LatEnd = dplyr::last(save.lat), # "Start" location of last point in line is actually the end of the transect
              LongEnd = dplyr::last(save.long)
    ) %>% 
    as.data.frame()
  
  rownames(data) <- data$Sample.Label
  l <- sp::SpatialLinesDataFrame(lines, data)
  sp::proj4string(l) <- sp::CRS(latlongproj)
  if (!is.null(CRS))
    l <- sp::spTransform(l, CRS)
  l
}

