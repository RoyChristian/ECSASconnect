#' @export
#'@title Create transect lines from ECSAS watches
#'
#'@description This function aggregates consecutive ECSAS watches into transects. 
#'@param dat  a dataframe of ECSAS watches (1 row per watch). 
#'@param angle.thresh - (default 90 degrees) changes in direction of more than this are considered to start new transects (currently not implemented)
#'@param max.lag (default 10 minutes) the maximum time lag allowed between consecutive watches to be considered part of the same transect. 
#'@param CRS - if not null, the output coordinate system will be set CRS via \code{sp::spTransform}
#'@param debug.watch watchid to stop at in for loop if debug is true
#'
#'@details
#'Combine watches into transects and create a spatiallinesdataframe for each transect in dat. Transects are defined as all 
#'consecutive watches by the same observer on the same vessel in the same direction on the same day.
#'
#'rows in \code{dat} must contain at least columns named \code{ObserverName}, \code{PlatformDir},
#'\code{PlatformName}, \code{Date}, \code{StartTime}, \code{EndTime}, \code{LatStart}, \code{LongStart}, \code{LatEnd}, 
#'\code{LongEnd} as returned by \code{ECSAS.extract}.  \code{LatEnd} and \code{LongEnd} are only necessary for the last watch in
#'in each transect to provide it's endpoint. If either is \code{NA}, the end point will be projected along a loxodrome based 
#'on the watch start location, \Code{PlatformDir} (or \code{PlatformDirDeg} if it exists), \code{PlatformSpeed}, and \code{ObsLen}. 
#'If either of these is missing then an error is produced. 
#'@section Author:Dave Fifield
#'
#'@seealso \code{\link{ECSAS.extract}}
#'

ECSAS.create.transects <- function(dat, angle.thresh = NULL, max.lag = 10, debug = FALSE, CRS = NULL, debug.watch = NULL){
  
  if(debug) browser()
  
  if(!is.null(angle.thresh))
    stop("In ECSAS.create.transects: angle.thresh is not yet implemented.")
  
  # xxxx may have to convert platform dir to numeric to imlement angle.thresh comparison
  # xxxx use platformdirdeg if it exists????
  
  if(is.empty(dat$ObserverName) || is.empty(dat$PlatformDir) || is.empty(dat$PlatformName))
    stop("Something is empty")
  
  # sort and get time lag between consecutive rows
  dat %<>% dplyr::arrange(ObserverName, PlatformName, Date, StartTime) %>% 
    dplyr::mutate(timelag = time_length(StartTime - lag(EndTime), unit = "minutes"))
  
  #init
  dat$Sample.Label <- NA_integer_
  Sample.Label <- 1L
  cur.obs <- dat$ObserverName[1]
  cur.dir <- dat$PlatformDir[1]
  cur.ves <- dat$PlatformName[1]
  cur.day <- dat$Date[1]
  
  # start assigning Sample.Label until one of day, observer, direction or vessel changes
  for (i in 1:nrow(dat)){
#    if(Sample.Label == 140) browser()
  
    row <- dat[i,]
    
    if(debug && !is.null(debug.watch) && row$WatchID == debug.watch) browser()

    # start new transect?
    # if(cur.obs != row$Observer ||abs((as.numeric(as.character(row$Direction)) - 
    #                                   as.numeric(as.character(cur.dir)))) %% 360 > angle.thresh 
    #    || cur.ves != row$Vessel || cur.day != date(row$StartTime)){
    
    #XXXX
    # Need to add a fuzz factor to say that start time of each consecutive watch in transect should be be within 
    # so many minutes of end of last one. This prevents watches many hours apart on same day in same direction being combined
    # into same transect.perhaps compte time lag for all rows as time diff between that row and previous one and then just add
    # condition that lag <= fuzzfactor. Can dplyr compute lags on rows easily
    
    if(cur.obs != row$ObserverName || cur.dir != row$PlatformDir || cur.ves != row$PlatformName || cur.day != row$Date ||
       (!is.na(row$timelag) && row$timelag > max.lag)){
      Sample.Label <- Sample.Label + 1L
      cur.obs <- row$ObserverName
      cur.dir <- row$PlatformDir
      cur.ves <- row$PlatformName
      cur.day <- row$Date
    }  
    
    if(is.na(dat[i, "Sample.Label"]))
      dat[i, "Sample.Label"] <- Sample.Label
  }

  if(debug) browser()
  
  # add a final point at end of each transect
  dat <- split(dat, dat$Sample.Label) %>% 
    purrr::map_df(add.final.point) %>% 
    dplyr::mutate(save.lat = LatStart,
           save.long = LongStart) # save from removal by coordinates()

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
              #Watches = paste(unique(WatchID), collapse = ", "),
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

# Add a final point at the end of a transect
add.final.point <- function(df, debug = FALSE){
  if(debug) browser()
  rn <- row.names(df)
  last.point <- df[nrow(df),]
  if(is.na(last.point$LatEnd) || is.na(last.point$LongEnd))
    last.point %<>% project.endpoint()

  # "Start" location of endpoint is the end loc of last watch
  last.point %<>%
    dplyr::mutate(LatStart = LatEnd,
           LongStart = LongEnd,
           WatchLenKm = 0) # Don't want last point to add spurious extra length
  

  df <- dplyr::bind_rows(df, last.point)
  row.names(df) <- c(rn, paste0(rn[length(rn)], "_1"))
  df
}

# projet endpoint for a specific watch and recalc WatchLenKm
project.endpoint <- function(row, debug = FALSE) {
  if(debug) browser()
  
  if(is.empty(row$LongStart) || is.empty(row$LatStart) || is.empty(row$ObsLen) || is.empty(row$PlatformSpeed))
    stop("Error: project.endpoint: one of required variables is empty.")
  
  
  p <- with(row, geosphere::destPoint(cbind(LongStart, LatStart), b = as.numeric(as.character(get.platform.direction(row))), 
                                      d = ((ObsLen/60) * PlatformSpeed * 1.852) * 1000))
  row %>% 
    dplyr::mutate(LongEnd = p[,1],
           LatEnd = p[,2],
           WatchLenKm = geosphere::distGeo(cbind(LongStart, LatStart), cbind(LongEnd,  LatEnd))/1000
    )
}


# return a numeric direction for a watch
get.platform.direction <- function(watch, direction.as.tex = TRUE){
  
  nrow(watch) == 1 || stop(sprintf("get.platform.direction: watch can only have one row not %d.", nrow(watch)))
  
  if(!is.na(watch$PlatformDirDeg))
    res <- watch$PlatformDirDeg
  else {
    if (is.integer(watch$PlatformDir))
      res <- switch(EXPR = as.character(watch$PlatformDir), "2" = 0, "3" = 45, "4" = 90, "5" = 135, "6" = 180, "7" = 225, "8" = 270, "9" = 315, 
                    "get.platform.direction: Error")
    else
      res <- switch(EXPR = watch$PlatformDir, N = 0, NE = 45, E = 90, SE = 135, S = 180, SW = 225, W = 270, NW = 315, "get.platform.direction: Error")
  }
  
  if (is.null(res))
    stop(paste0("get.platform.direction: Error: result is NULL for PlatformDir == ", watch$PlatformDir))
  res
}
