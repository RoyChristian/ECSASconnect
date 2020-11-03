#' @export
#'@title Extract data from ECSAS database
#'
#'@description The function will connect to the Access database, create a series of queries and import the desired information in a data frame.
#'@param species Optional. Alpha code (or vector of Alpha codes, e.g., c("COMU,"TBMU", "UNMU")) for the species desired in the extraction.
#'@param years Optional. Either a single year or a vector of two years denoting "from" and "to" (inclusive).
#'@param lat Pair of coordinate giving the southern and northern limits of the range desired.
#'@param long Pair of coordinate giving the western and eastern limits of the range desired. Note that west longitude values must be negative.
#'@param obs.keep Name of the observer to keep for the extraction. The name of the observer must be followed by it's first name (eg: "Bolduc_Francois").
#'@param obs.exclude Name of the observer to exclude for the extraction.The name of the observer must be followed by it's first name (eg: "Bolduc_Francois").
#'@param sub.program From which sub.program the extraction must be made. Options are Quebec, Atlantic, Arctic, ESRF, AZMP, FSES, or All
#'All subprograms will include the observations made in the PIROP program.
#'@param intransect Should we keep only the birds counted on the transect (if TRUE, the default) or extract all observations (if FALSE).
#'@param distMeth Integer specifying the distance sampling method code (tblWatch.DistMeth in ECSAS). Default is c(14, 20) which includes all watches
#'   with perpendicular distances for both flying and swimming birds. If "All", then observations from all distance sampling methods will be returned.
#'@param ecsas.path (default NULL) full path name to the ECSAS database. If NULL, the path is built from \code{ecsas.drive} and \code{ecsas.file}.
#'@param ecsas.drive path to folder containing the ECSAS Access database
#'@param ecsas.file  name of the ECSAS Access database
#'@details 
#'The function will produce a data frame that contains all the pertinent information. Note that watches with no observations (the so called "zeros" are
#'included by default).
#'
#'The distance traveled during the watch is returned in the column \code{WatchLenKm}. If lat/long coordinates
#'are available for both the start and end locations of the watch, then it is calculated as the shortest 
#'distance between these two points on the WGS84 ellipsoid using \code{geosphere::DistGeo} and, in this case,
#'\code{WatchLenKmHow} will contain \code{"distGeo"}. Otherwise \code{WatchLenKm} 
#'is calculated as the \code{PlatformSpeed * CalcDurMin} where \code{CalcDurMin} is the length of the 
#'watch in minutes computed from start and end times. In this case, \code{WatchLenKmHow} will contain 
#'\code{"Dead Reckoning"}. 
#' 
#'@section Author:Christian Roy, Dave Fifield
#'
#'@seealso \code{\link{QC.extract}}

ECSAS.extract <-  function(species,  years, lat=c(-90,90), long=c(-180, 180), obs.keep=NA, obs.exclude=NA,
           sub.program=c("All","Atlantic","Quebec","Arctic","ESRF","AZMP","FSRS"), intransect=TRUE, distMeth = c(14, 20),
           ecsas.path = NULL,
           ecsas.drive="C:/Users/christian/Dropbox/ECSAS", ecsas.file="Master ECSAS_backend v 3.31.mdb", 
           debug = FALSE) {

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

  if(debug) browser()
  
  # test for 32-bit architecture
  if (Sys.getenv("R_ARCH") != "/i386")
    stop("You are not running a 32-bit R session. You must run ECSAS.extract in a 32-bit R session due to limitations in the RODBC Access driver.")

  # initialize various SQL sub-clauses here. Simplifies if-then-else logic below.
  intransect.selection <- ""  
  year.selection <- ""
  sp.selection <- ""
  distMeth.selection <- ""
  selected.sub.program <- ""
    
  ### Make sure arguments works with sub.programs
  sub.program.names <- c("Atlantic","Quebec","Arctic","ESRF","AZMP","FSRS")
  if(any(is.na(match(sub.program, c(sub.program.names,"All"))))){
     stop(paste("Unknown sub.program(s) specified. Sub-program should be one of: ",
                paste(sub.program.names, collapse=" "), "or All"))
  }
  sub.program <- match.arg(sub.program, several.ok=TRUE) #Not sure how to make it check for all argument names

  ###setwd and open connection
  if(!is.null(ecsas.path)) {
    channel1 <- RODBC::odbcConnectAccess(ecsas.path, uid="")
  } else {
    channel1 <- RODBC::odbcConnectAccess(file.path(ecsas.drive, ecsas.file), uid="")
  }
  
  # generic where-clause start and end. "1=1" is a valid expression that does nothing but is syntactically
  # correct in case there are no other where conditions.
  where.start <-  "WHERE ((1=1)"
  where.end <- ")"

  #Write SQL selection for intransect birds
  if(intransect){
    intransect.selection <- "AND ((tblSighting.InTransect)=True)"
  }

  #write SQL selection for latitude and longitude
  lat.selection <-  paste("AND ((tblWatch.LatStart)>=", lat[1], " And (tblWatch.LatStart)<=", lat[2], ")", sep="")
  long.selection <- paste("AND ((tblWatch.LongStart)>=", long[1], " And (tblWatch.LongStart)<=", long[2], ")", sep="")

  # SQL for distMeth
  if (length(distMeth) != 1 || distMeth != "All"){
    distMeth.selection <- paste0("AND (",paste0(paste0("(tblWatch.DistMeth)=", distMeth), collapse= " OR "), ")")
  }

  #write SQL selection for the different type of sub.programs
  if(any(sub.program != "All")){
    selected.sub.program <- paste0("AND (", paste0(paste0("(tblCruise.", sub.program[sub.program != "All"], ")=TRUE"), collapse=" OR "), ")")
  }

  #write SQL selection for year
  if (!missing(years)) {
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

  #Excute query for species
  specieInfo <-  RODBC::sqlQuery(channel1, query.species) %>% ensure_data_is_returned 
  
  # handle species specification
  if (!missing(species)) {
    ### Make sure that species is in capital letters
    species <- toupper(species)

    ### make sure the species are in the database.
    wrong.sp <- species[!species %in% specieInfo$Alpha]
    if (length(wrong.sp) > 0){
        if(length(wrong.sp) == 1){
          stop(paste("species code", wrong.sp, "is not included in the database", sep = " "))
        }else{
          stop(paste("species codes", paste(wrong.sp, collapse = " and "), "are not included in the database", sep = " "))
      }
    }

    # Remove speciesinfo records where Alpha is NA, since they can't currently be specified for selection anyway. This helps
    # for the indexing below.
    specieInfo <- dplyr::filter(specieInfo, !is.na(Alpha))
    
    # Form the WHERE clause that is based on the species number instead of the alpha code
    nspecies <- paste0(sapply(1:length(species), function(i) {
      paste("(tblSighting.SpecInfoID)=", specieInfo[specieInfo$Alpha == species[i], ]$SpecInfoID, sep = "")
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
                                selected.sub.program,
                                year.selection,
                                where.end,
                                sep=" "),
                          sep=" ")


  #Import all the tables needed
  Sighting <- RODBC::sqlQuery(channel1, query.sighting) %>% ensure_data_is_returned 
  watches <- RODBC::sqlQuery(channel1, query.watches) %>% ensure_data_is_returned 
  cruise.notes <- RODBC::sqlFetch(channel1, "tblCruiseNotes") %>% ensure_data_is_returned 
  distance <- RODBC::sqlFetch(channel1, "lkpDistanceCenters") %>% ensure_data_is_returned 
  observer <- RODBC::sqlFetch(channel1, "lkpObserver") %>% ensure_data_is_returned 
  platform.name <- RODBC::sqlFetch(channel1, "lkpPlatform") %>% ensure_data_is_returned 
  platform.activity <- RODBC::sqlFetch(channel1, "lkpPlatformType") %>% ensure_data_is_returned 
  seastates <- RODBC::sqlFetch(channel1, "lkpSeastate") %>% ensure_data_is_returned 
  #close connection
  RODBC::odbcCloseAll()

  #name change for the second column
  names(platform.name)[2] <- "PlatformName"
  cruise.notes %<>% dplyr::rename(CruiseNote = Note)

  # Calculate watch length in km via dead reckoning if start and end positions are not avail, otherwise use ellipsoid method.
  # Note use of more accurate CalcDurMin rather than the (integer) ObsLen
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
    dplyr::rename(SeaStateID = SeaState)

  #merge and filter the tables for the sigthings
  Sighting2 <- plyr::join(plyr::join(Sighting, specieInfo, by = "SpecInfoID", type = "left"),
                    distance, by = "DistanceCode") [,c("FlockID", "WatchID","Alpha","English","Latin","Class",
                                                    "Seabird", "Waterbird","ObsLat","ObsLong","ObsTime","Distance","DistanceCode",
                                                    "InTransect","Association", "Behaviour","FlightDir","FlySwim",
                                                    "Count","Age","Plumage","Sex")]



  #merge and filter the tables for the watches
  Watches2 <- plyr::join(
                plyr::join(
                  plyr::join(
                    plyr::join(
                      plyr::join(watches, seastates, by="SeaStateID", type = "left"), 
                      observer, by = "ObserverID"
                    ),
                    platform.name, by = "PlatformID", type="left"
                  ),
                  platform.activity, by = "PlatformTypeID",type="left"
                ),
                cruise.notes, by = "CruiseID", type = "left"
                )[,c("CruiseID", "CruiseNote", "Program", "PlatformName",
                    "Atlantic", "Quebec", "Arctic", "ESRF", "AZMP", "FSRS", "StartDate", "EndDate", "WatchID",
                    "TransectNo",
                    "ObserverName", "PlatformClass", "WhatCount", "TransNearEdge", "TransFarEdge","DistMeth",
                    "Date","Year","Month","Week","Day","StartTime", "EndTime", "CalcDurMin",
                    "LatStart","LongStart", "LatEnd", "LongEnd", "PlatformSpeed",
                    "PlatformDir", "PlatformDirDeg", "PlatformActivity", "ObsLen", "WatchLenKm", "WatchLenKmHow",
                    "Snapshot","Experience",
                    "Visibility", "SeaState", "Swell", "Windspeed", "Windforce", "Weather", "Glare", "IceType",
                    "IceConc", "ObsSide", "ObsOutIn", "ObsHeight", "ScanType", "ScanDir")]

  ###Create the final table by joining the observations to the watches
  final.df <- plyr::join(Watches2, Sighting2, by = "WatchID", type = "left", match = "all")

  #Change the way the observer names are stored in the table
  final.df$ObserverName <- as.factor(sapply(1:nrow( final.df),
                                        function(i){gsub(", " , "_", as.character(final.df$ObserverName[i]) )}))

  #Select or exlude the observers
  if(!is.na(obs.exclude)){
    keep1 <- setdiff(levels(final.df$ObserverName), obs.exclude)
    final.df <- subset(final.df, final.df$ObserverName %in% keep1)
    final.df <- droplevels(final.df)
  }

  if(!is.na(obs.keep)){
    final.df <- subset(final.df, final.df$ObserverName %in% obs.keep)
    final.df <- droplevels(final.df)
  }

  # Export the final product
  return(droplevels(final.df))
  
  #End
}