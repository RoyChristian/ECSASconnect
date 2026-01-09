is.empty <- function(x){
  any(is.na(x)) || any(is.null(x)) || any(is.nan(x))
}

# Add a final point at the end of a transect
add.final.point <- function(df, ignore_empty_dir = FALSE){
  
  # can't remember what this is for, but judging by the code below, perhaps
  # in some use cases the sample_label is stored in the rowname??
  rn <- row.names(df)
  last.point <- df[nrow(df),]
  if(is.na(last.point$LatEnd) || is.na(last.point$LongEnd))
    last.point %<>% project.endpoint(ignore_empty_dir = ignore_empty_dir)

  # "Start" location of endpoint is the end loc of last watch
  last.point %<>%
    dplyr::mutate(LatStart = LatEnd, LongStart = LongEnd,
           WatchLenKm = 0) # Don't want last point to add spurious extra length
  
  df <- dplyr::bind_rows(df, last.point)
  row.names(df) <- c(rn, paste0(rn[length(rn)], "_1"))
  df
}

# project endpoint for a specific watch and recalc WatchLenKm.
# If ignore_empty_dir is TRUE, and row$PlatformDir is 1, "ND" or NA, then
# the point will be projected to the north.
project.endpoint <- function(row, ignore_empty_dir = FALSE) {
  
  if(is.empty(row$LongStart) || is.empty(row$LatStart) || is.empty(row$CalcDurMin) ||
     is.empty(row$PlatformSpeed))
    stop(paste0("Error: project.endpoint: CruiseID = ", row$CruiseID, ", WatchID = ",
                row$WatchID, ", one of LongStart, LatStart, CalcDurMin, or ", 
                "PlatformSpeed is empty."))
  
  p <- with(row, geosphere::destPoint(cbind(LongStart, LatStart), 
          b = as.numeric(as.character(get.platform.direction(row, 
                                                             ignore_empty_dir))), 
          d = ((CalcDurMin/60) * PlatformSpeed * 1.852) * 1000))
  row %>% 
    dplyr::mutate(LongEnd = p[,1],
           LatEnd = p[,2],
           WatchLenKm = geosphere::distGeo(cbind(LongStart, LatStart), cbind(LongEnd,  LatEnd))/1000
    )
}

# Checks each string in specs to ensure it is a 4-letter code
# Returns TRUE is all ok, otherwise a single string which is a concatenation
# of all errors separated by ";"
check_species_codes <- function(specs) {
  # Check length of each species code
  x <- purrr::map(specs, check_string_len, len = 4) %>% 
    setNames(specs)
    
  # Remove ones with no issues
  probs <- Filter(Negate(isTRUE), x) 
  
  # Convert list of named error strings to char vector which includes species
  # codes
  if (length(probs) != 0){
    res <- purrr::imap_chr(probs, ~ paste0("Species code '", .y, "': ", .x)) %>% 
      stringr::str_flatten(collapse = "; ")
  } else
    res <- TRUE
  
  res
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

# return a numeric direction for a watch.
# If ignore_empty_dir is TRUE, and dir is 1 (or "ND", or NA), just return
# 0 (ie. north)
get.platform.direction <- function(watch, ignore_empty_dir = FALSE){
  
  nrow(watch) == 1 || stop(sprintf("get.platform.direction: watch can only have one row not %d.", nrow(watch)))

  # Just default to returning north if direction is NA and were ignoring
  # empty directions.
  if (ignore_empty_dir && is.na(watch$PlatformDir))  
    return(0)
  
  if("PlatformDirDeg" %in% colnames(watch) && !is.na(watch$PlatformDirDeg))
    res <- watch$PlatformDirDeg
  else {
    if (is.numeric(watch$PlatformDir))
      if (isTRUE(ignore_empty_dir))
        res <- switch(EXPR = as.character(watch$PlatformDir), "1" = 0, "2" = 0, "3" = 45, 
                    "4" = 90, "5" = 135, "6" = 180, "7" = 225, "8" = 270, 
                    "9" = 315, "get.platform.direction: Error")
      else  
        res <- switch(EXPR = as.character(watch$PlatformDir), "2" = 0, "3" = 45, 
                    "4" = 90, "5" = 135, "6" = 180, "7" = 225, "8" = 270, 
                    "9" = 315, "get.platform.direction: Error")
    else
      if (isTRUE(ignore_empty_dir))
        res <- switch(EXPR = watch$PlatformDir, ND = 0, N = 0, NE = 45, E = 90, SE = 135, 
                      S = 180, SW = 225, W = 270, NW = 315,
                      "get.platform.direction: Error")
      else
        res <- switch(EXPR = watch$PlatformDir, N = 0, NE = 45, E = 90, SE = 135, 
              S = 180, SW = 225, W = 270, NW = 315,
              "get.platform.direction: Error")
  }
  
  if (is.null(res) || res == "get.platform.direction: Error" )
    stop(paste0("get.platform.direction: Error: illegal PlatformDir: ",
      watch$PlatformDir))
  res
}

# Check if a direction is empty of set to "No particular direction"
# which is 1 (see lkpDirections). This should really lookup the "ND" 
# code in lkpDirections and make sure it is 1.
is.nd <- function(x) {
  is.na(x) || is.null(x) || is.nan(x) || x == 1
}


# Used by other functions to get an open channel to the database. Most
# user-facing functions allow supplying either an open channel or a path to the
# database. Those functions call this one to decide which mechanism to use. When
# user-facing functions are called with an open channel, it is left open after
# they exit otherwise it is closed. return list(channel, leave.open)
ecsas.get.channel <- function(ecsas.path = NULL, channel = NULL) {
  
  checkmate::assert(checkmate::check_file_exists(ecsas.path),
                    checkmate::check_class(channel, "RODBC"))
  
  ret <- list()
  
  # preferentially use channel if not null otherwise fall back to open
  # ecsas.path
  if (!is.null(channel)){
    ret$channel = channel
    ret$leave.open = TRUE
  }else if(!is.null(ecsas.path)){
    # open connection. This should work on both 32-bit R and 64-bit R as long as
    # user has both 32-bit and 64-bit Access drivers installed. See 
    # https://stackoverflow.com/questions/45064057/reading-data-from-32-bit-access-db-using-64-bit-r
    # and https://www.microsoft.com/en-US/download/details.aspx?id=13255
    ret$channel <- RODBC::odbcConnectAccess2007(ecsas.path, uid="") 
    ret$leave.open <- FALSE
  } else
    stop("ecsas.get.channel: could get channel to database.")
  
  ret
}


# Runs insert_SQL (assumed to be in insert query) and returns the autonumber
# value of the inserted record if the table has an autonumber field otherwise NA
access_insert_return_id <- function(channel, insert_sql) {
  # Run the INSERT
  res <- RODBC::sqlQuery(channel, insert_sql)
  
  # Detect failure: sqlQuery returns a character vector of length > 0 on error
  if (is.character(res) && length(res) > 0) {
    stop("Insert failed: ", paste(res, collapse = " | "))
  }
  
  # Query @@IDENTITY immediately
  id_res <- RODBC::sqlQuery(channel, "SELECT @@IDENTITY AS new_id", errors = FALSE)
  
  # Extract scalar
  if (is.data.frame(id_res) && ncol(id_res) == 1) {
    val <- id_res[[1]][1]
    # Access semantics: 0 means "no identity generated"
    if (is.na(val) || val == 0) return(NA_integer_)
    return(as.integer(val))
  }
  
  # Fallback if unexpected
  return(NA_integer_)
}