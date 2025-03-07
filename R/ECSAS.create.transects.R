#'@export
#'@title Create transect lines from ECSAS watches
#'
#'@description This function aggregates consecutive ECSAS watches into
#'  transects.
#'@param dat a dataframe of ECSAS watches (1 row per watch).
#'@param angle.thresh (default 90 degrees) changes in direction of more than
#'  this are considered to start new transects (currently not implemented)
#'@param max.lag (default 10 minutes) the maximum time lag allowed between
#'  consecutive watches to be considered part of the same transect. Watches
#'  are sorted by \code{ObserverName}, \code{PlatformName}, \code{Date}, and
#'  \code{StartTime} and time lag is computed as 
#'  \code{StartTime(this_watch) - EndTime(previous_watch)}. 
#'@param CRS if not null, the output coordinate system will be set CRS via
#'  \code{sp::spTransform}
#'@param ignore_empty_dir logical (default FALSE) should watches with a PlatformDir
#'  that is either empty or "no direction" (ie. 1 - see table \code{lkpDirections}
#'  in database), be be allowed. If TRUE, then any such watch is assumed to be
#'  heading in the same direction as the previous one.
#'@param debug logical (default FALSE), should the debugger, [base::browser()],
#'  be used
#'@param debug.watch watchid to stop at in for loop if debug is true
#'
#'@details Combine watches in \code{dat} into transects and return a SpatialLinesDataFrame.
#'A transect is defined as all consecutive watches
#'by the same observer on the same vessel in the same direction on the same day.
#'
#'Rows in \code{dat} must contain at least columns named \code{ObserverName},
#'\code{PlatformDir}, \code{PlatformName}, \code{Date}, \code{StartTime},
#'\code{EndTime}, \code{LatStart}, \code{LongStart}, \code{LatEnd},
#'\code{LongEnd} as returned by \code{\link{ECSAS.extract()}}.
#'
#'Watches are sorted by \code{ObserverName}, \code{PlatformName}, \code{Date}, 
#'and \code{StartTime}, and thus any errors in these will result in erroneous
#'transects.
#'
#'\code{LatEnd} and
#'\code{LongEnd} are only necessary for the last watch in each transect to
#'provide it's endpoint. If either is \code{NA}, the end point will be projected
#'along a loxodrome starting at the watch start location in the direction of \code{PlatformDir} (or
#'\code{PlatformDirDeg} if it exists), based on \code{PlatformSpeed} (in knots),
#'and \code{ObsLen} (in minutes).
#'If \code{PlatformDirDeg} or \code{PlatformDir} is missing or "no direction" 
#'and \code{ignore_empty_dir} is \code{TRUE}, then the point is projected to the
#'north. If either of \code{PlatformSpeed} or \code{ObsLen} is missing then 
#'an error is produced.
#'@section Author:Dave Fifield
#'
#'@seealso \code{\link{ECSAS.extract}}
#'  
#'@importFrom magrittr `%>%` `%<>%`
ECSAS.create.transects <- function(dat, angle.thresh = NULL, max.lag = 10, 
                                   ignore_empty_dir = FALSE,
                                   debug = FALSE, CRS = NULL, debug.watch = NULL){
  
  if(debug) browser()
  
  if(!is.null(angle.thresh))
    stop("In ECSAS.create.transects: angle.thresh is not yet implemented.")
  
  # xxxx may have to convert platform dir to numeric to implement angle.thresh comparison
  # xxxx use platformdirdeg if it exists????
  
  if(is.empty(dat$ObserverName) || (is.empty(dat$PlatformDir) && !ignore_empty_dir) 
     || is.empty(dat$PlatformName))
    stop("Something is empty")
  
  # sort and get time lag between consecutive rows
  dat %<>% dplyr::arrange(ObserverName, PlatformName, Date, StartTime) %>% 
    dplyr::mutate(timelag = lubridate::time_length(StartTime - dplyr::lag(EndTime), unit = "minutes"))
  
  # initialize values for current transect
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

    # XXXX ideas for angle threshold
    # start new transect?
    # if(cur.obs != row$Observer ||abs((as.numeric(as.character(row$Direction)) - 
    #                                   as.numeric(as.character(cur.dir)))) %% 360 > angle.thresh 
    #    || cur.ves != row$Vessel || cur.day != date(row$StartTime)){
    
    # If the first watch had no direction, then unless one of ObserverName, or
    # PlatformName, date changes or timelag is exceeded (starting a new
    # transect) then eventually we may hit a watch with a valid direction which
    # then becomes the current direction. 
    if(is.nd(cur.dir) && !is.nd(row$PlatformDir)) 
      cur.dir <- row$PlatformDir

    # Start new transect? 
    # If anything besides cur.dir changes, or row$timelag > max.lag
    # then start a new transect.
    # 
    # Also....
    #  
    # defns
    # cdc - cur.dir != row$PlatformDir
    # iet - ignore empty == tru
    # ndp - is.nd(row$PlatformDir) == TRUE
    # ndc - is.nd(cur.dir) == TRUE
    # 
    # Cases:
    # 0. something other than direction has changed
    # 1. dir has changed, we are not ignoring -> START NEW no matter what
    # 2. dir has changed, we are ignoring, cur.dir is still ND or NA -> DO NOTHING 
    #    since it doesn't matter what the change is - either it's the first real dir
    #    or it's just a change from ND to NA or vice versa.
    # 3. dir has changed, we are ignoring, cur.dir is a real direction, 
    #    row$PlatformDir is ND or NA -> DO NOTHING (ie. we have seen 1 or more 
    #    of the same direction and now it has changed to ND or NA, so we just 
    #    ignore and keep cur.dir the same)
    # 4. dir has changed, we are ignoring, cur.dir is a real direction, 
    #    row$PlatformDir is a real direction -> START NEW, this is a real
    #    change from one real direction to another
    #    
    # so we need to start a new transect in cases 1 and 4
    # 
    # if((cdc && !iet) || # case 1
    #    (cdc && iet && (!ndc && !ndp))  # case 4
    #    
    #    
    #    
    cdc <- cur.dir != row$PlatformDir
    iet <- ignore_empty_dir == TRUE
    ndp <- is.nd(row$PlatformDir) == TRUE
    ndc <- is.nd(cur.dir) == TRUE

    if( (cur.obs != row$ObserverName || 
          cur.ves != row$PlatformName ||
          cur.day != row$Date ||
          (!is.na(row$timelag) && row$timelag > max.lag)) || # End case 0
          (cdc && !iet) ||                # case 1
          (cdc && iet && (!ndc && !ndp))  # case 4
         ) {
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
    purrr::map_df(add.final.point, ignore_empty_dir = ignore_empty_dir) %>% 
    dplyr::mutate(save.lat = LatStart,
           save.long = LongStart) # protect from removal by coordinates(), not needed if add.final.point is updated to use SpatialPointsDataFrame to create instead of coordinates()???

  # create lines object
  sp::coordinates(dat) <- ~ LongStart + LatStart
  
  lines <- dat %>% 
    sp::split(dat$Sample.Label) %>% 
    lapply(function(x) sp::Lines(list(sp::Line(sp::coordinates(x))), x$Sample.Label[1L])) %>% 
    sp::SpatialLines(proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  
  # create data row for each transect and create spatialLinesDataFrame
  data <- dat@data %>% 
    dplyr::group_by(Sample.Label) %>% 
    dplyr::summarize(
              CruiseID = unique(CruiseID),
              Program = unique(Program),
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
              LatEnd = dplyr::last(save.lat), # Start location of the newly added last point in line is the end of the transect
              LongEnd = dplyr::last(save.long)
    ) %>% 
    as.data.frame()
  
  rownames(data) <- data$Sample.Label # deprecated for tibbles. not sure what it was used for
  l <- sp::SpatialLinesDataFrame(lines, data)
  # sp::proj4string(l) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  if (!is.null(CRS))
    l <- sp::spTransform(l, CRS)
  l
}

#'@export
#'@title Add sample labels to a dataframe of watches
#'
#'@description Add a \code{Sample.Label} column to \code{wtchs} and set it to the
#' corresponding \code{Sample.Label} from \code{trns} that each watch belongs to.
#' 
#'@param wtchs a dataframe of watches containing at least a column name WatchID
#'@param trns a transect object as returned by \code{ECSAS.create.transects()}.
#'
#'@details Simpy unnests the \code{Watches} list-column from \code{trns} and joins
#' it to the \code{wtchs} dataframe matching on \code{WatchID}. Replaces existing 
#'\code{Sample.Label} column if it exists. Throws an error if any row of \code{trns}
#'has no associated watches.
#'
ECSAS.add.sample.label <- function(wtchs, trns) {
  
  # make sure all transects have at least one watch
  stopifnot(all(purrr::map_int(transects.sp@data$Watches, length) > 0))
  
  # Remove existing Sample.Label column
  if ("Sample.Label" %in% names(wtchs)) wtchs %<>% dplyr::select(-Sample.Label)
  
  # unnest list column into individual row for each unique value in Watches,
  # select sample.Label and Watches columns, 
  # rename Watches to WatchID and
  # join to wtchs
  tidyr::unnest(trns, Watches) %>% 
    dplyr::select(Sample.Label, Watches) %>% 
    dplyr::rename(WatchID = Watches) %>% 
    dplyr::right_join(wtchs, by = "WatchID")
}
