is.empty <- function(x){
  any(is.na(x)) || any(is.null(x)) || any(is.nan(x))
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
    dplyr::mutate(LatStart = LatEnd, LongStart = LongEnd,
           WatchLenKm = 0) # Don't want last point to add spurious extra length
  

  df <- dplyr::bind_rows(df, last.point)
  row.names(df) <- c(rn, paste0(rn[length(rn)], "_1"))
  df
}

# projet endpoint for a specific watch and recalc WatchLenKm
project.endpoint <- function(row, debug = FALSE) {
  if(debug) browser()
  
  if(is.empty(row$LongStart) || is.empty(row$LatStart) || is.empty(row$CalcDurMin) ||
     is.empty(row$PlatformSpeed))
    stop("Error: project.endpoint: one of LongStart, LatStart, CalcDurMin, or PlatformSpeed is empty.")
  
  p <- with(row, geosphere::destPoint(cbind(LongStart, LatStart), 
                                      b = as.numeric(as.character(get.platform.direction(row))), 
                                      d = ((CalCDurMin/60) * PlatformSpeed * 1.852) * 1000))
  row %>% 
    dplyr::mutate(LongEnd = p[,1],
           LatEnd = p[,2],
           WatchLenKm = geosphere::distGeo(cbind(LongStart, LatStart), cbind(LongEnd,  LatEnd))/1000
    )
}

# function that checks if argument is a string of length n
checkStringLen <- check_string_len <- function(x, len = 1) {
  res <- checkmate::check_integerish(len, len = 1, any.missing = FALSE, all.missing = FALSE, )
  if (!isTRUE(res))
    return(res)
  
  res <- checkmate::check_string(x)  
  if (!isTRUE(res))
    return(res)

  if (nchar(x) != len)
    return(sprintf("String must be of length %d, actual length: %d", len, nchar(x)))
  
  return(TRUE)
}

assert_string_len <- assertStringLen <- checkmate::makeAssertionFunction(checkStringLen)
test_string_len <- testStringLen <- checkmate::makeTestFunction(checkStringLen)
expect_string_len <- expectStringLen <- checkmate::makeExpectationFunction(checkStringLen)

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
