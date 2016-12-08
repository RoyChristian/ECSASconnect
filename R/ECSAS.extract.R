#' @export
#'@title Extract the information for the Global ECSAS database
#'
#'@description The function will connect to the Access database, create a series of queries and import the desired information in a data frame.
#'@param species Optional Alpha code for the species desired in the extraction. If more than one species all the desired species must be saved into a vector ex: c("COMU,"TBMU", "UNMU")
#'@param years Years desired for the extraction
#'@param lat Pairs of coordinate giving the southern and northern limits of the range desired.
#'@param long Pairs of coordinate giving the western and eastern limits of the range desired. Note that longitude values must be negative.
#'@param obs.keep Name of the observer to keep for the extraction. The name of the observer must be followed by it's first name (eg: "Bolduc_Francois").
#'@param Obs.exclude Name of the observer to exlude for the extraction.The name of the observer must be followed by it's first name (eg: "Bolduc_Francois").
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


ECSAS.extract <-  function(species,  years=c(2006,2013), lat=c(-90,90), long=c(-180, 180), Obs.keep=NA, Obs.exclude=NA,
           database=c("Atlantic","Quebec","Both","All"), intransect=T, distMeth = 14,
           ecsas.drive="C:/Users/christian/Dropbox/ECSAS",
           ecsas.file="Master ECSAS_backend v 3.31.mdb"){

  ###Make sure arguments works
  database<- match.arg(database)

  ###setwd and open connection
  wd<-getwd()
  setwd(ecsas.drive)
  channel1 <- odbcConnectAccess(ecsas.file, uid="")

  ##correction for year=1
  ## currently a hack
  ## we need to year to make the query work I add the year before to the extraction and delete it
  hack.year=FALSE
  if(length(years)==1){
    years <- c(years-1,years)
    hack.year=TRUE
  }
  ##alternative way to include only one year
  if(years[1]==years[2]){
    years[1] <- years[2]-1
    hack.year=TRUE
  }

  #Make sure the temp table doesn't exist
  if("tblspselect"%in%sqlTables(channel1)$TABLE_NAME){
    sqlDrop(channel1, "tblspselect")
  }

  #Write SQL selection for intransect birds
  if(intransect){
    intransect.selection=paste("AND ((tblSighting.InTransect)=True)")
  }else{
    intransect.selection=paste(")")
  }

  #write SQL selection for latitude and longitude
  lat.selection <-  paste("WHERE (((tblWatch.LatStart)>=",lat[1]," And (tblWatch.LatStart)<=",lat[2],")",sep="")
  long.selection <- paste("AND ((tblWatch.LongStart)>=",long[1]," And (tblWatch.LongStart)<=",long[2],")",sep="")

  # SQL for distMeth
  distMeth.selection <- paste0("AND ((tblWatch.DistMeth)=", distMeth, ")")

  #write SQL selection for the different type of cruise
  if(database=="Atlantic"){
    selected.database <- "AND ((tblCruise.Atlantic)=TRUE)"
  }else{
    if(database=="Quebec"){
      selected.database <- "AND ((tblCruise.Quebec)=TRUE)"
    }else{
      if(database=="Both"){
        selected.database <- "AND ((tblCruise.Atlantic)=TRUE) OR ((tblCruise.Quebec)=TRUE)"
      }else{
        selected.database <- ""
      }}}


  #write SQL selection for year
  year.selection <- paste("AND ((DatePart('yyyy',[Date]))Between ",years[1]," And ",years[2],"))",sep="")

  # handle species specification
  if (!missing(species)) {
    ###Make sure that sp is in capital letters
    sp <- toupper(sp)
    #write SQL selection for species
    if(length(sp)>=2){
      nspecies <-paste0(sapply(1:length(sp),function(i){paste("(tblSpeciesInfo.Alpha)='",sp[i],"'",sep="")}), collapse=" Or ")
      sp.selection <- paste("((",nspecies,")",sep="")
    }else{
      sp.selection <- paste("(((tblSpeciesInfo.Alpha)='",sp,"')",sep="")
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
    if(nrow(specieInfo)!=length(sp)){
      wrong.sp <-sp[!sp%in%specieInfo$Alpha]
      if(length(wrong.sp)==1){
        stop(paste("species code",wrong.sp,"is not included in the database",sep=" "))
      }else{
        stop(paste("species code",paste(wrong.sp, collapse=" and "),"are not included in the database",sep=" "))
      }
    }


    #Write a second query that is based on the species number instead of the alpha code
    if(length(sp)>=2){
      nspecies2 <- paste0(sapply(1:length(sp),function(i){paste("(tblSighting.SpecInfoID)=",specieInfo$SpecInfoID[i],sep="")}), collapse=" Or ")
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
                           paste(lat.selection,
                                 long.selection,
                                 sp.selection2,
                                 intransect.selection,
                                 year.selection,
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
                           paste(lat.selection,
                                 long.selection,
                                 intransect.selection,
                                 distMeth.selection,
                                 year.selection,
                                 sep=" "))
  }



  #Write the query to import the watches table
  query.watches <-  paste(paste("SELECT tblWatch.CruiseID",
                                "tblCruise.Program",
                                "tblCruise.[Start Date] AS [StartDate]",
                                "tblCruise.[End Date] AS [EndDate]",
                                "tblWatch.WatchID AS [WatchID]",
                                "tblWatch.DistMeth",
                                "tblWatch.Observer AS [ObserverID]",
                                "tblWatch.Date AS [Date]",
                                "tblWatch.StartTime",
                                "tblWatch.EndTime",
                                "tblWatch.LatStart AS [LatStart]",
                                "tblWatch.LongStart AS [LongStart]",
                                "([PlatformSpeed]*[ObsLen]/60*1.852) AS [WatchLenKm]",
                                "tblWatch.Snapshot",
                                "tblWatch.ObservationType AS [Experience]",
                                "tblCruise.PlatformType AS [PlatformTypeID]",
                                "tblCruise.PlatformName AS [PlatformID]",
                                "tblWatch.Visibility",
                                "tblWatch.SeaState",
                                "tblWatch.Swell",
                                "tblWatch.Weather",
                                "tblWatch.Glare",
                                "tblCruise.Atlantic",
                                "tblCruise.Quebec",
                                  "DatePart('yyyy',[Date]) AS [Year]",
                                "DatePart('m',[Date]) AS [Month]",
                                "DatePart('ww',[Date]) AS Week",
                                "DatePart('y',[Date]) AS [Day]", sep=", "),
                          "FROM tblCruise INNER JOIN tblWatch ON tblCruise.CruiseID = tblWatch.CruiseID",
                          paste(lat.selection,
                                long.selection,
                                #"AND ((([PlatformSpeed]*[ObsLen]/60*1.852)) Is Not Null And (([PlatformSpeed]*[ObsLen]/60*1.852))>0)",
                                distMeth.selection,
                                selected.database,
                                year.selection,
                                sep=" "),
                          sep=" ")


  #Import all the tables needed
  Sighting <- sqlQuery(channel1, query.sighting )
  watches <- sqlQuery(channel1, query.watches)
  distance <- sqlFetch(channel1, "lkpDistanceCenters")
  observer <- sqlFetch(channel1, "lkpObserver")
  platform.name <- sqlFetch(channel1, "lkpPlatform")
  platform.activity <- sqlFetch(channel1, "lkpPlatformType")
  #close connection
  odbcCloseAll()

  #name change for the second column
  names(platform.name)[2] <- "PlatformName"

  #merge and filter the tables for the sigthings
  Sighting2 <- join(join(Sighting,specieInfo,by="SpecInfoID",type="left"),
                    distance,by="DistanceCode") [,c("FlockID", "WatchID","Alpha","English","Latin","Class",
                                                    "ObsLat","ObsLong","ObsTime","Distance","DistanceCode",
                                                    "InTransect","Association", "Behaviour","FlightDir","FlySwim",
                                                    "Count","Age","Plumage","Sex")]

  #merge and filter the tables for the watches
  Watches2 <- join(join(join(watches, observer, by="ObserverID"),
                        platform.name, by="PlatformID", type="left"),
                   platform.activity,by="PlatformTypeID",type="left") [,c("CruiseID","Program", "Atlantic","Quebec", "StartDate",
                                                                          "EndDate","WatchID","DistMeth", "ObserverName","Date","Year","Month","Week","Day","StartTime",
                                                                          "EndTime", "LatStart","LongStart", "WatchLenKm", "Snapshot","Experience",
                                                                          "PlatformName","PlatformType", "Visibility","SeaState","Swell","Weather","Glare")]

  ###Create the final table by joining the observations to the watches
  final.df <- join(Watches2,Sighting2, by="WatchID", type="left", match="all")
  #Change the way the observer names are stored in the table
  final.df$ObserverName <- as.factor(sapply(1:nrow( final.df),
                                        function(i){gsub(", ","_",as.character(final.df$ObserverName[i]) )}))

  #Select or exlude the observers
  if(!is.na(Obs.exclude)){
    keep1 <- setdiff(levels(final.df$ObserverName), Obs.exclude)
    final.df <- subset(final.df, final.df$ObserverName%in%keep1)
    final.df <-droplevels(final.df)
  }

  if(!is.na(Obs.keep)){
    final.df <- subset(final.df, final.df$ObserverName%in%Obs.keep)
    final.df <-droplevels(final.df)
  }

  #Return to the working drive
  setwd(wd)
  #Export the final product
  return(final.df)
  #End
}
