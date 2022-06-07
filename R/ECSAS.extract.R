#'@export
#'@title Extract data from ECSAS database
#'
#'
#'@description This function will connect to the Access database, create a
#'  series of queries and import the desired information in a data frame.
#'
#'@param species \[character:\sQuote{NULL}]\cr Optional. Alpha code (or vector
#'  of Alpha codes, e.g., c("COMU,"TBMU", "UNMU")) for the species desired in
#'  the extraction.
#'@param years \[integer:\sQuote{NULL}]\cr Optional. Either a single year or a
#'  vector of two years denoting "from" and "to" (inclusive).
#'@param cruise.ids \[integer:\sQuote{NULL}]\cr Optional. Integer vector of cruise ID's to extract.
#'@param lat \[numeric(2):\sQuote{\code{c(-90, 90)}}] \cr Vector of two numbers
#'  giving the southern and northern limits of the range desired.
#'@param long \[numeric(2):\sQuote{\code{c(-180, 180)}}] \cr Vector of two
#'  numbers giving the western and eastern limits of the range desired. Note
#'  that west longitude values must be negative.
#'@param obs.keep \[character:\sQuote{NA}]\cr Names of the observers to keep for
#'  the extraction. Name format: Surname_FirstName (eg: "Bolduc_Francois").
#'@param obs.exclude \[character:\sQuote{NA}]\cr Name of the observer to exclude
#'  for the extraction. Name format: Surname_FirstName (eg: "Bolduc_Francois").
#'@param sub.program
#'  \[character:\sQuote{\code{c("All","Atlantic","Quebec","Arctic","ESRF","AZMP","FSRS")}}]\cr
#'   From which sub.program the extraction must be made. \sQuote{\code{All}}
#'  subprograms will include the observations made in the PIROP program.
#'@param intransect DEPRECATED - please use \code{intransect.only.}
#'@param intransect.only \[logical(1):\sQuote{TRUE}]\cr If TRUE, return only
#'  observations coded as "In Transect", otherwise return all observations. See
#'  the ECSAS survey protocol for more details:
#'
#'  Gjerdrum, C., D.A. Fifield, and S.I. Wilhelm. 2012. Eastern Canada Seabirds
#'  at Sea (ECSAS) standardized protocol for pelagic seabird surveys from moving
#'  and stationary platforms. Canadian Wildlife Service Technical Report Series
#'  No. 515. Atlantic Region. vi + 37 pp.
#'@param distMeth \[integer or character:\sQuote{\code{c(14,20)}}]\cr Integer(s)
#'  specifying the distance sampling method code(s) (see tblWatch.DistMeth in
#'  ECSAS). Acceptable values are a single integer, a vector of integers, or
#'  \sQuote{\code{All}}. The default will includes all watches with
#'  perpendicular distances for both flying and swimming birds. If
#'  \sQuote{\code{All}}, then observations from all distance sampling methods
#'  will be returned, which may include observations from the PIROP program if
#'  no other options preclude this.
#'@param ind.tables.only \[logical(1):\sQuote{FALSE}]\cr Indicates if two
#'  individual tables for watch/cruise, and observations should be returned
#'  rather than a single table with all columns combined. See Value section.
#'@param ecsas.path \[character:\sQuote{NULL}]\cr Full path name to the ECSAS
#'  database. If NULL, the path is built from \code{ecsas.drive} and
#'  \code{ecsas.file}.
#'@param ecsas.drive
#'  \[character:\sQuote{\code{"C:/Users/christian/Dropbox/ECSAS"}}]\cr Path to
#'  folder containing the ECSAS Access database. The default value is likely no
#'  longer useful and should be deprecated.
#'@param ecsas.file  \[character:\sQuote{\code{"Master ECSAS_backend v
#'  3.31.mdb"}}]\cr Name of the ECSAS Access database file. The default value is
#'  likely no longer useful and should be deprecated.
#'
#'
#'@details
#'
#'The distance traveled during the watch is returned in the column
#'\code{WatchLenKm}. If lat/long coordinates are available for both the start
#'and end locations of the watch, then it is calculated as the shortest distance
#'between these two points on the WGS84 ellipsoid using [geosphere::distGeo()]
#'and, in this case, \code{WatchLenKmHow} will contain \code{"distGeo"}.
#'Otherwise \code{WatchLenKm} is calculated as the \code{PlatformSpeed *
#'CalcDurMin} where \code{CalcDurMin} is the length of the watch in minutes
#'computed from start and end times. In this case, \code{WatchLenKmHow} will
#'contain \code{"Dead Reckoning"}.
#'
#'@return By default the function will produce a data frame that contains all
#'  the pertinent information. Note that watches with no observations (the so
#'  called "zeros" are included by default).
#'
#'  If \code{ind.tables.only} is \code{FALSE} (the default), then a single
#'  dataframe is returned containing all pertinent cruise, watch and sightings
#'  table info for each observation. If a given watch had no observations, then
#'  sighting related fields will be \code{NA}.
#'
#'  If \code{ind.tables.only} is \code{TRUE}, then a list is returned with the
#'  following elements:
#'
#'  \tabular{ll}{ \code{watches} \tab the combined columns from the watch and
#'  cruise tables only.\cr \code{sightings}\tab the columns from the sightings
#'  table only. }
#'
#'  Note it is not currently possible to extract the watch and cruise tables
#'  separately.
#'
#'@section Author:Christian Roy, Dave Fifield
#'
#'@seealso \code{\link{QC.extract}}

# debugging
# rm(list=ls())
# years <- c(2016)
# lat <- c(39.33489,74.65058)
# long <- c(-90.50775,-38.75887)
# sub.program <- "Atlantic"
# ecsas.drive <- "C:/Users/fifieldd/Documents/Offline/R/ECSASconnect Fresh/Test"
# ecsas.file <- "Master ECSAS v 3.51.mdb"
# intransect <- T
# distMeth <- 14
# species <- c("ATPU")
# obs.exclude <- NA
# obs.keep <- NA

ECSAS.extract <-  function(species = NULL, 
                           years = NULL, 
                           cruise.ids = NULL,
                           lat = c(-90,90), 
                           long = c(-180, 180), 
                           obs.keep = NA, 
                           obs.exclude = NA,
                           sub.program = c("All","Atlantic","Quebec","Arctic","ESRF","AZMP","FSRS"), 
                           intransect = NULL,
                           intransect.only = TRUE,
                           distMeth = c(14, 20),
                           ind.tables.only = FALSE,
                           ecsas.path = NULL,
                           ecsas.drive = "C:/Users/christian/Dropbox/ECSAS", 
                           ecsas.file = "Master ECSAS_backend v 3.31.mdb", 
                           debug = FALSE) {

  if(debug) browser()
  
  if(!missing(intransect))
    stop("Function argument 'intransect' is deprecated - please use intransect.only instead.")
  
  # check args
  coll = checkmate::makeAssertCollection()
  
  checkmate::assert(
    checkmate::check_null(species),
    check_string_len(species, len = 4), 
    add = coll
  )
  
  checkmate::assert(
    checkmate::check_null(years),
    checkmate::check_integerish(years, any.missing = FALSE, len = 1),
    checkmate::check_integerish(years, any.missing = FALSE, len = 2, sorted = TRUE),
    add = coll
  )

  checkmate::assert(
    checkmate::check_null(cruise.ids),
    checkmate::check_integerish(cruise.ids, any.missing = FALSE),
    add = coll
  )

  checkmate::assert_numeric(lat, any.missing = FALSE, len = 2, sorted = TRUE, add = coll)
  checkmate::assert_numeric(long, any.missing = FALSE, len = 2, sorted = TRUE, add = coll)
  checkmate::assert_character(obs.keep, min.len = 1, add = coll)
  checkmate::assert_character(obs.exclude, min.len = 1, add = coll)
  checkmate::assert_subset(sub.program, eval(formals()$sub.program), add = coll)
  checkmate::assert_logical(intransect.only, len = 1, any.missing = FALSE, add = coll)
  
  checkmate::assert(
    checkmate::check_integerish(distMeth, any.missing = FALSE, min.len = 1), 
    checkmate::check_choice(distMeth, "All" ),
    add = coll
  )

  checkmate::assert(
    checkmate::check_file_exists(ecsas.path),
    checkmate::check_file_exists(file.path(ecsas.drive, ecsas.file)), 
    add = coll
  )

  checkmate::assert(
    checkmate::check_null(ind.tables.only),
    checkmate::check_logical(ind.tables.only, any.missing = FALSE, len = 1),
    add = coll
  )
  
  checkmate::assert_logical(debug, any.missing = FALSE, len = 1, add = coll)
  checkmate::reportAssertions(coll)
  
  # initialize various SQL sub-clauses here. Simplifies if-then-else logic below.
  intransect.selection <- ""  
  year.selection <- ""
  cruise.ids.selection <- ""
  sp.selection <- ""
  distMeth.selection <- ""
  selected.sub.program <- ""
    
  # If no subprogram is specified then just leave sub-program out of it, which
  # allows sub-program to be blank. Prior to version 0.6.8 it would only return
  # watches where one of these was true.
  sub.program.names <- c("Atlantic","Quebec","Arctic","ESRF","AZMP","FSRS")
  if(any(is.na(match(sub.program, c(sub.program.names, "All"))))){
     stop(paste("Unknown sub.program(s) specified. Sub-program should be one of: ",
                paste(sub.program.names, collapse=" "), "or All"))
  }
  # Not sure how to make it check for all argument names
  sub.program <- match.arg(sub.program, several.ok = TRUE) 
  
  # If "All" is still in sub.program at this point then include all sub.programs
  # in results, which just means not including anything in the sql WHERE clause
  # for sub.program. Note that there are some cruises where none of the
  # sub.programs is TRUE. 
  #
  # "All" will be in sub.program when we get here either because it was
  # there by default (and thus no sub.program was supplied by user), or because
  # the user passed a sub.program arg which contains "All".
  if(!any(sub.program == "All")){
    selected.sub.program <- paste0("AND (", 
                                  paste0(
                                     paste0("(tblCruise.", sub.program[sub.program != "All"], ") = TRUE"), 
                                      collapse=" OR "
                                     ), 
                                  ")")
  }

  ### open connection. This should work on both 32-bit R and 64-bit R as long as
  # user has both 32-bit and 64-bit Access drivers installed. See 
  # https://stackoverflow.com/questions/45064057/reading-data-from-32-bit-access-db-using-64-bit-r
  # and https://www.microsoft.com/en-US/download/details.aspx?id=13255
  if(!is.null(ecsas.path)) {
    channel1 <- RODBC::odbcConnectAccess2007(ecsas.path, uid="")
  } else {
    channel1 <- RODBC::odbcConnectAccess2007(file.path(ecsas.drive, ecsas.file), uid="")
  }
  
  # generic where-clause start and end. "1=1" is a valid expression that does nothing but is syntactically
  # correct in case there are no other where conditions.
  where.start <-  "WHERE ((1=1)"
  where.end <- ")"

  # Write SQL selection for intransect birds
  if(intransect.only){
    intransect.selection <- "AND ((tblSighting.InTransect)=True)"
  }

  # write SQL selection for latitude and longitude
  lat.selection <-  paste("AND ((tblWatch.LatStart)>=", lat[1], " And (tblWatch.LatStart)<=", lat[2], ")", sep="")
  long.selection <- paste("AND ((tblWatch.LongStart)>=", long[1], " And (tblWatch.LongStart)<=", long[2], ")", sep="")

  # SQL for distMeth. 
  #
  # This is tricky...
  # Due to lazy evaluation, the second clause in the if-statement will only 
  #  ever be evaluated when length(distMeth) == 1, thus the second clause
  #  makes sense and doesn't need any(..) wrapped around it.
  # 
  # distMeth can be:
  #     a single number - if-statement executes,
  #     a vector of numbers, - if-statement executes,
  #     "All" - if-satement doesn't execute and thus distMeth is unconstrained.
  if (length(distMeth) != 1 || distMeth != "All"){
    distMeth.selection <- paste0("AND (",
                            paste0(
                              paste0("(tblWatch.DistMeth)=", distMeth), 
                            collapse = " OR "), ")")
  }

  # Deal with cruise.ids if supplied
  if (!is.null(cruise.ids)) {
  cruise.ids.selection <- paste0("AND (",
                            paste0(
                              paste0("(tblWatch.CruiseID)=", cruise.ids), 
                            collapse = " OR "), ")")
  }
  
  #write SQL selection for year
  if (!missing(years) && !is.null(years)) { # Years provided and not null?
    if(length(years) == 1)
      year.selection <- paste0("AND ((DatePart('yyyy',[Date])) = ", years, ")")
    else if (length(years) == 2)
      year.selection <- paste0("AND ((DatePart('yyyy',[Date]))Between ", years[1], " And ", years[2], ")")
    else
      stop("Years must be either a single number or a vector of two numbers.")
  }

  # SQL query to import the species table. Just go ahead and import whole thing since it's short (~600 rows)
  query.species <- paste(
    paste(
      "SELECT tblSpeciesInfo.Alpha",
      "tblSpeciesInfo.English",
      "tblSpeciesInfo.Latin",
      "tblSpeciesInfo.Class",
      "tblSpeciesInfo.Seabird",
      "tblSpeciesInfo.Waterbird",      
      "tblSpeciesInfo.SpecInfoID",
      sep = ", "
    ),
    "FROM tblSpeciesInfo",
    sep = " "
  )

  # Execute query for species.  
  speciesInfo <-  RODBC::sqlQuery(channel1, query.species) %>% 
    ensure_data_is_returned 
    
  # handle species specification
  if (!missing(species)) {
    ### Make sure that species is in capital letters
    species <- toupper(species)

    ### make sure the species are in the database.
    wrong.sp <- species[!(species %in% speciesInfo$Alpha)]
    if (length(wrong.sp) > 0){
        if(length(wrong.sp) == 1){
          stop(paste("species code", wrong.sp, "is not included in the database", sep = " "))
        }else{
          stop(paste("species codes", paste(wrong.sp, collapse = " and "), "are not included in the database", sep = " "))
      }
    }

    # Form the WHERE clause that is based on the species number instead of the alpha code
    nspecies <- paste0(sapply(1:length(species), function(i) {
      paste("(tblSighting.SpecInfoID)=", speciesInfo[speciesInfo$Alpha == species[i], ]$SpecInfoID, sep = "")
      }), collapse = " Or ")
    sp.selection <- paste("AND (", nspecies, ")", sep = "")
  } 

  # Write the query to import the table for sighting
  query.sighting <-  paste(paste("SELECT tblSighting.WatchID",
                                "tblSighting.SpecInfoID",
                                "tblSighting.FlockID",
                                "tblSighting.ObsLat",
                                "tblSighting.ObsLong",
                                "tblSighting.ObsTime",
                                "tblSighting.Distance AS [DistanceCode]",
                                "tblSighting.InTransect",
                                "tblSighting.Association",
                                "tblSighting.Behaviour",
                                "tblSighting.FlightDir",
                                "tblSighting.FlySwim",
                                "tblSighting.Count",
                                "tblSighting.Age",
                                "tblSighting.Plumage",
                                "tblSighting.Sex",
                                "tblWatch.LatStart",
                                "tblWatch.LongStart",
                                "tblWatch.Date", sep=", "),
                        "FROM tblWatch INNER JOIN tblSighting ON tblWatch.WatchID = tblSighting.WatchID",
                        paste(where.start,
                               lat.selection,
                               long.selection,
                               sp.selection,
                               intransect.selection,
                               distMeth.selection,
                               year.selection,
                               cruise.ids.selection,
                               where.end,
                               sep = " "
                        )
                    )

  #Write the query to import the watches table
  query.watches <-  paste(paste("SELECT tblWatch.CruiseID",
                                "tblCruise.Program",
                                "tblCruise.[Start Date] AS [StartDate]",
                                "tblCruise.[End Date] AS [EndDate]",
                                "tblWatch.WatchID",
                                "tblWatch.TransectNo",
                                "tblWatch.PlatformClass",
                                "tblWatch.WhatCount",
                                "tblWatch.TransNearEdge",
                                "tblWatch.TransFarEdge",
                                "tblWatch.DistMeth",
                                "tblWatch.Observer AS [ObserverID]",
                                "tblWatch.Observer2 AS [Observer2ID]",
                                "tblWatch.Date AS [Date]",
                                "tblWatch.StartTime",
                                "tblWatch.EndTime",
                                "tblWatch.LatStart",
                                "tblWatch.LongStart",
                                "tblWatch.LatEnd",
                                "tblWatch.LongEnd",
                                "tblWatch.PlatformSpeed",
                                "tblWatch.PlatformDir",
                                "tblWatch.PlatformDirDeg",
                                "tblWatch.ObsLen",
                                "tblWatch.PlatformActivity",
                                "tblWatch.Snapshot",
                                "tblWatch.ObservationType AS [Experience]",
                                "tblCruise.PlatformType AS [PlatformTypeID]",
                                "tblCruise.PlatformName AS [PlatformID]",
                                "tblWatch.Visibility",
                                "tblWatch.SeaState",
                                "tblWatch.Windspeed",
                                "tblWatch.Windforce",
                                "tblWatch.WindDir",
                                "tblWatch.Weather",
                                "tblWatch.Glare",
                                "tblWatch.Swell",
                                "tblWatch.IceType",
                                "tblWatch.IceConc",
                                "tblWatch.ObsSide",
                                "tblWatch.ObsOutIn",
                                "tblWatch.ObsHeight",
                                "tblWatch.ScanType",
                                "tblWatch.ScanDir",
                                "tblCruise.Atlantic",
                                "tblCruise.Quebec",
                                "tblCruise.Arctic",
                                "tblCruise.ESRF",
                                "tblCruise.AZMP",
                                "tblCruise.FSRS",
                                "DatePart('yyyy',[Date]) AS [Year]",
                                "DatePart('m',[Date]) AS [Month]",
                                "DatePart('ww',[Date]) AS Week",
                                "DatePart('y',[Date]) AS [Day]", sep = ", "),
                          "FROM tblCruise INNER JOIN tblWatch ON tblCruise.CruiseID = tblWatch.CruiseID",
                          paste(where.start,
                                lat.selection,
                                long.selection,
                                #"AND ((([PlatformSpeed]*[ObsLen]/60*1.852)) Is Not Null And (([PlatformSpeed]*[ObsLen]/60*1.852))>0)",
                                distMeth.selection,
                                cruise.ids.selection,
                                selected.sub.program,
                                year.selection,
                                where.end,
                                sep=" "),
                          sep=" ")

  # Import all the tables needed
  sightings <- RODBC::sqlQuery(channel1, query.sighting) %>% ensure_data_is_returned 
  watches <- RODBC::sqlQuery(channel1, query.watches) %>% ensure_data_is_returned 
  cruise.notes <- RODBC::sqlFetch(channel1, "tblCruiseNotes") %>% 
    ensure_data_is_returned %>% 
    dplyr::rename(CruiseNote = Note)
  distance <- RODBC::sqlFetch(channel1, "lkpDistanceCenters") %>% ensure_data_is_returned 
  observer <- RODBC::sqlFetch(channel1, "lkpObserver") %>% ensure_data_is_returned 
  platform.name <- RODBC::sqlFetch(channel1, "lkpPlatform") %>% 
    ensure_data_is_returned %>% 
    dplyr::rename(PlatformName = PlatformText)
  platform.activity <- RODBC::sqlFetch(channel1, "lkpPlatformType") %>% ensure_data_is_returned 
  seastates <- RODBC::sqlFetch(channel1, "lkpSeastate") %>% ensure_data_is_returned 
  
  #close connection
  RODBC::odbcClose(channel1)

  # Calculate watch length in km via dead reckoning if start and end positions
  # are not avail, otherwise use ellipsoid method. Note use of more accurate
  # CalcDurMin rather than the (integer) ObsLen.
  watches  %<>% 
    dplyr::mutate(CalcDurMin = as.numeric(difftime(EndTime, StartTime, units = "mins")),
      WatchLenKm = dplyr::case_when(
        is.na(LatStart) | is.na(LongStart) | is.na(LatEnd) | is.na(LongEnd) ~ 
                                                (PlatformSpeed * (CalcDurMin / 60) * 1.852),
        TRUE ~ geosphere::distGeo(cbind(LongStart, LatStart), cbind(LongEnd,  LatEnd))/1000),
      WatchLenKmHow = dplyr::case_when(
        is.na(LatStart) | is.na(LongStart) | is.na(LatEnd) | is.na(LongEnd) ~ "Dead Reckoning",
                                                                       TRUE ~ "distGeo")
      ) %>% 
    dplyr::rename(SeaStateID = SeaState) %>% 
    dplyr::left_join(seastates, by="SeaStateID") %>% 
    dplyr::left_join(observer, by = "ObserverID") %>% 
    dplyr::left_join(platform.name, by = "PlatformID") %>% 
    dplyr::left_join(platform.activity, by = "PlatformTypeID") %>% 
    dplyr::left_join(cruise.notes, by = "CruiseID") %>% 
    dplyr::mutate(ObserverName = as.factor(gsub(", " , "_", as.character(ObserverName)))) %>% 
    dplyr::select(CruiseID, CruiseNote, Program, PlatformName, Atlantic, Quebec, Arctic, ESRF, 
                  AZMP, FSRS, StartDate, EndDate, WatchID, TransectNo, ObserverName, PlatformClass, 
                  WhatCount, TransNearEdge, TransFarEdge, DistMeth, Date, Year, Month, Week, Day,
                  StartTime, EndTime, CalcDurMin, LatStart, LongStart, LatEnd, LongEnd, PlatformSpeed,
                  PlatformDir, PlatformDirDeg, PlatformActivity, ObsLen, WatchLenKm, WatchLenKmHow,
                  Snapshot, Experience, Visibility, SeaState, Swell, Windspeed, Windforce, WindDir, Weather, 
                  Glare, IceType, IceConc, ObsSide, ObsOutIn, ObsHeight, ScanType, ScanDir)

  # Select or exclude the observers
  if(!is.na(obs.exclude)){
    watches %<>% dplyr::filter(!(ObserverName %in% obs.exclude))
  }

  if(!is.na(obs.keep)){
    watches %<>% dplyr::filter(ObserverName %in% obs.keep)
  }

  # Merge and filter the tables for the sightings
  sightings %<>% 
    dplyr::left_join(speciesInfo, by = "SpecInfoID") %>% 
    dplyr::left_join(distance, by = "DistanceCode") %>% 
    dplyr::select(FlockID, WatchID, Alpha, English, Latin, Class, Seabird, Waterbird, ObsLat,
                 ObsLong, ObsTime, Distance, DistanceCode, InTransect, Association, Behaviour,
                 FlightDir, FlySwim, Count, Age, Plumage, Sex)

  # Return the final product
  if (ind.tables.only) {
    list(watches = droplevels(watches), sightings = droplevels(sightings))
  } else {
    droplevels(dplyr::left_join(watches, sightings, by = "WatchID"))
  }
  # End
}