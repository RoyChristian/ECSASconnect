#' @export
#'@title Extract the information for the Global ECSAS database
#'
#'@description The function will connect to the Access database, create a series of queries and import the desired information in a data frame.
#'@param species Optional. Alpha code (or vector of Alpha codes, e.g., c("COMU,"TBMU", "UNMU")) for the species desired in the extraction. 
#'@param years Optional. Either a single year or a vector of two years denoting "from" and "to" (inclusive).
#'@param lat Pair of coordinate giving the southern and northern limits of the range desired.
#'@param long Pair of coordinate giving the western and eastern limits of the range desired. Note that west longitude values must be negative.
#'@param obs.keep Name of the observer to keep for the extraction. The name of the observer must be followed by it's first name (eg: "Bolduc_Francois").
#'@param obs.exclude Name of the observer to exlude for the extraction.The name of the observer must be followed by it's first name (eg: "Bolduc_Francois").
#'@param database From which database the extraction must be made. Options are Quebec, Atlantic, both regions or all the observations. All the observations will inlcude the observations made in the PIROP program.
#'@param intransect Should we keep only the birds counted on the transect or not.
#'@param distMeth Integer specifying the distance sampling method code (tblWatch.DistMeth in ECSAS). Default is 14 (Perpendicular distanes for both flying and swimming birds).
#'@param ecsas.drive Where is located the ECSAS Access database
#'@param ecsas.file  What is the name of the ECSAS Access database
#'@details
#'The function will produce a data frame that will contains all the pertinent information.
#'@section Author:Christian Roy, Dave Fifield
#'
#'@seealso \code{\link{QC.extract}}

ECSAS.extract <-  function(species,  years, lat=c(-90,90), long=c(-180, 180), obs.keep=NA, obs.exclude=NA,
           database=c("All","Atlantic","Quebec","Arctic","ESRF","AZMP","FSRS"), intransect=T, distMeth = 14,
           ecsas.drive="C:/Users/christian/Dropbox/ECSAS",
           ecsas.file="Master ECSAS_backend v 3.31.mdb"){

# debugging
# rm(list=ls())
# years <- c(2016)
# lat <- c(39.33489,74.65058)
# long <- c(-90.50775,-38.75887)
# database <- "Atlantic"
# ecsas.drive <- "C:/Users/fifieldd/Documents/Offline/R/ECSAS connect/Test"
# ecsas.file <- "Master ECSAS v 3.51.mdb"
# intransect <- T
# distMeth <- 14
# species <- c("ATPU")
# obs.exclude <- NA
# obs.keep <- NA

  # test for 32-bit architecture
  if (Sys.getenv("R_ARCH") != "/i386")
    stop("You are not running a 32-bit R session. You must run ECSAS.extract in a 32-bit R session due to limitations in the RODBC Access driver.")
  
  ###Make sure arguments works with databases
  dbnames<-c("Atlantic","Quebec","Arctic","ESRF","AZMP","FSRS")
  if(any(is.na(match(database,c(dbnames,"All"))))){
     stop(paste("Some names not matching",paste(dbnames,collapse=" "),"or All"))  
  }
  database<- match.arg(database,several.ok=TRUE) #Not sure how to make it check for all argument names

  ###setwd and open connection
  wd<-getwd()
  setwd(ecsas.drive)
  channel1 <- odbcConnectAccess(ecsas.file, uid="")

  # generic where-clause start and end. "1=1" is a valid expression that does nothing but is syntactically
  # correct in case there are no other where conditions.
  where.start <-  "WHERE ((1=1)"
  where.end <- ")"
  
  #Write SQL selection for intransect birds
  if(intransect){
    intransect.selection <- "AND ((tblSighting.InTransect)=True)"
  }else{
    intransect.selection <- ""
  }

  #write SQL selection for latitude and longitude
  lat.selection <-  paste("AND ((tblWatch.LatStart)>=",lat[1]," And (tblWatch.LatStart)<=",lat[2],")",sep="")
  long.selection <- paste("AND ((tblWatch.LongStart)>=",long[1]," And (tblWatch.LongStart)<=",long[2],")",sep="")

  # SQL for distMeth
  distMeth.selection <- paste0("AND (",paste0(paste0("(tblWatch.DistMeth)=",distMeth),collapse=" OR "),")") 

  #write SQL selection for the different type of databases
  if(any(database=="All")){
    selected.database <- "" 
  }else{
    selected.database <- paste0("AND (",paste0(paste0("(tblCruise.",database,")=TRUE"),collapse=" OR "),")") 
  }

  #write SQL selection for year
  if (!missing(years)) {
    if(length(years) == 1)
      year.selection <- paste0("AND ((DatePart('yyyy',[Date])) = ", years, ")")
    else if (length(years) == 2)
      year.selection <- paste0("AND ((DatePart('yyyy',[Date]))Between ",years[1]," And ",years[2],")")
    else
      stop("Years must be either a single number or a vector of two numbers.")
  } else { 
    year.selection <- ""
  }
  
  # handle species specification
  if (!missing(species)) {
    ###Make sure that species is in capital letters
    species <- toupper(species)
    
    #write SQL selection for species
    if(length(species)>=2){
      nspecies <-paste0(sapply(1:length(species),function(i){paste("(tblSpeciesInfo.Alpha)='",species[i],"'",sep="")}), collapse=" Or ")
      sp.selection <- paste("((",nspecies,")",sep="")
    }else{
      sp.selection <- paste("(((tblSpeciesInfo.Alpha)='",species,"')",sep="")
    }


    #SQL queries to import the species table
    query.species <- paste(paste("SELECT tblSpeciesInfo.Alpha",
                                 "tblSpeciesInfo.English",
                                 "tblSpeciesInfo.Latin",
                                 "tblSpeciesInfo.Class",
                                 "tblSpeciesInfo.SpecInfoID", sep=", "),
                           "FROM tblSpeciesInfo",
                           "WHERE",
                           sp.selection,
                           "AND ((tblSpeciesInfo.Class)='Bird'))", sep=" ")

    #Excute query for species
    specieInfo <-  sqlQuery(channel1, query.species)

    ###make sure the species are in the database.
    if(nrow(specieInfo)!=length(species)){
      wrong.sp <-species[!species%in%specieInfo$Alpha]
      if(length(wrong.sp)==1){
        stop(paste("species code",wrong.sp,"is not included in the database",sep=" "))
      }else{
        stop(paste("species code",paste(wrong.sp, collapse=" and "),"are not included in the database",sep=" "))
      }
    }


    #Write a second query that is based on the species number instead of the alpha code
    if(length(species)>=2){
      nspecies2 <- paste0(sapply(1:length(species),function(i){paste("(tblSighting.SpecInfoID)=",specieInfo$SpecInfoID[i],sep="")}), collapse=" Or ")
      sp.selection2 <- paste("AND (",nspecies2,")",sep="")
    }else{
      sp.selection2 <- paste("AND ((tblSighting.SpecInfoID)=",specieInfo$SpecInfoID,")",sep="")
    }

    #Write the query to import the table for sighting

    query.sighting <-  paste(paste("SELECT tblSighting.WatchID",
                                 "tblSighting.FlockID",
                                 "tblSighting.SpecInfoID",
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
                           paste("FROM tblWatch",
                                 "INNER JOIN tblSighting ON tblWatch.WatchID = tblSighting.WatchID", sep=" "),
                           paste(where.start, 
                                 lat.selection,
                                 long.selection,
                                 sp.selection2,
                                 intransect.selection,
                                 year.selection,
                                 where.end,
                                 sep=" "))

  } else { # no species was specified, so just get them all


    #SQL query to import the species info table
    query.species <- paste(paste("SELECT tblSpeciesInfo.Alpha",
                                 "tblSpeciesInfo.English",
                                 "tblSpeciesInfo.Latin",
                                 "tblSpeciesInfo.Class",
                                 "tblSpeciesInfo.SpecInfoID", sep=", "),
                           "FROM tblSpeciesInfo",
                           "WHERE ((tblSpeciesInfo.Class)='Bird')", sep=" ")

    #Excute query for species
    specieInfo <-  sqlQuery(channel1, query.species)


    #Write the query to import the table for sighting
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
                           paste("FROM tblWatch",
                                 "INNER JOIN tblSighting ON tblWatch.WatchID = tblSighting.WatchID", sep=" "),
                           paste(where.start,
                                 lat.selection,
                                 long.selection,
                                 intransect.selection,
                                 distMeth.selection,
                                 year.selection,
                                 where.end,
                                 sep=" "))
  }



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
                                "tblWatch.ObsLen",                                
                                "tblWatch.PlatformActivity",                                
                                "([PlatformSpeed]*[ObsLen]/60*1.852) AS [WatchLenKm]",
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
                                "DatePart('y',[Date]) AS [Day]", sep=", "),
                          "FROM tblCruise INNER JOIN tblWatch ON tblCruise.CruiseID = tblWatch.CruiseID",
                          paste(where.start,
                                lat.selection,
                                long.selection,
                                #"AND ((([PlatformSpeed]*[ObsLen]/60*1.852)) Is Not Null And (([PlatformSpeed]*[ObsLen]/60*1.852))>0)",
                                distMeth.selection,
                                selected.database,
                                year.selection,
                                where.end,
                                sep=" "),
                          sep=" ")


  #Import all the tables needed
  Sighting <- sqlQuery(channel1, query.sighting )
  watches <- sqlQuery(channel1, query.watches)
  distance <- sqlFetch(channel1, "lkpDistanceCenters")
  observer <- sqlFetch(channel1, "lkpObserver")
  platform.name <- sqlFetch(channel1, "lkpPlatform")
  platform.activity <- sqlFetch(channel1, "lkpPlatformType")
  seastates <- sqlFetch(channel1, "lkpSeastate")
  #close connection
  odbcCloseAll()

  #name change for the second column
  names(platform.name)[2] <- "PlatformName"

  # rename to do matching on seastates below. 
  watches <- plyr:::rename(watches, c("SeaState" = "SeaStateID"))
  
  #merge and filter the tables for the sigthings
  Sighting2 <- join(join(Sighting,specieInfo,by="SpecInfoID",type="left"),
                    distance,by="DistanceCode") [,c("FlockID", "WatchID","Alpha","English","Latin","Class",
                                                    "ObsLat","ObsLong","ObsTime","Distance","DistanceCode",
                                                    "InTransect","Association", "Behaviour","FlightDir","FlySwim",
                                                    "Count","Age","Plumage","Sex")]

  
  
  #merge and filter the tables for the watches
  Watches2 <- join(join(join(join(watches, seastates, by="SeaStateID", type = "left"), observer, by="ObserverID"),
                platform.name, by="PlatformID", type="left"),
                platform.activity,by="PlatformTypeID",type="left") [,c("CruiseID","Program", 
                  "Atlantic", "Quebec", "Arctic", "ESRF", "AZMP", "FSRS", "StartDate", "EndDate", "WatchID", "TransectNo",
                  "ObserverName", "PlatformClass", "WhatCount", "TransNearEdge", "TransFarEdge","DistMeth", 
                  "Date","Year","Month","Week","Day","StartTime",
                  "EndTime", "LatStart","LongStart", "LatEnd", "LongEnd", "PlatformSpeed",
                  "PlatformDir", "ObsLen", "WatchLenKm", "Snapshot","Experience",
                  "Visibility", "Swell", "Windspeed", "Windforce", "Weather", "Glare", "IceType",
                  "IceConc", "ObsSide", "ObsOutIn", "ObsHeight", "ScanType", "ScanDir")]

  ###Create the final table by joining the observations to the watches
  final.df <- join(Watches2, Sighting2, by="WatchID", type="left", match="all")
  
  #Change the way the observer names are stored in the table
  final.df$ObserverName <- as.factor(sapply(1:nrow( final.df),
                                        function(i){gsub(", ","_",as.character(final.df$ObserverName[i]) )}))

  #Select or exlude the observers
  if(!is.na(obs.exclude)){
    keep1 <- setdiff(levels(final.df$ObserverName), obs.exclude)
    final.df <- subset(final.df, final.df$ObserverName%in%keep1)
    final.df <-droplevels(final.df)
  }

  if(!is.na(obs.keep)){
    final.df <- subset(final.df, final.df$ObserverName%in%obs.keep)
    final.df <-droplevels(final.df)
  }

  #Return to the working drive
  setwd(wd)
  #Export the final product
  return(final.df)
  #End
}
      
