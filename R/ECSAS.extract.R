#' @export
#'@title Extract the information for the Global ECSAS database
#'
#'@description The function will connect to the Access database, create a series of queries and import the desired information in a data frame.
#'@param sp Alpha code for the specie desired in the extraction. If more than one species all the desired species must be saved into a vector ex: c("COMU,"TBMU", "UNMU") 
#'@param years Years desired for the extraction
#'@param lat Pairs of coordinate giving the southern and northern limits of the range desired.
#'@param long Pairs of coordinate giving the western and eastern limits of the range desired. Note that longitude values must be negative.
#'@param obs.keep Name of the observer to keep for the extraction. The name of the observer must be followed by it's first name (eg: "Bolduc_Francois").
#'@param Obs.exclude
#'@param Qc
#'@param snapshot
#'@param intransect
#'@param ecsas.drive
#'@param ecsas.file
#'@details
#'The function will produce a data frame that will contains all the pertinent information. 
#'@section Author:Christian Roy
#'
#'@seealso \code{\link{QC.extract}}


ECSAS.extract <-
function(sp="ATPU",  years=c(2006,2013), lat=c(30,70), long=c(-70, -30), Obs.keep=NA, Obs.exclude=NA, 
                         Qc=F, snapshot=T, intransect=T, ecsas.drive="C:/Users/christian/Dropbox/ECSAS", 
                         ecsas.file="Master ECSAS_backend v 3.31.mdb"){
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

#Make sure the tempo table doesn't exist
if("tblspselect"%in%sqlTables(channel1)$TABLE_NAME){
    sqlDrop(channel1, "tblspselect")
}

#write queries
  if(length(sp)>=2){
    multi.sp <- paste0(sapply(1:length(sp),function(i){paste("(tblSpeciesInfo.Alpha)='",sp[i],"'",sep="")}), collapse=" Or ")
    sp.selection <- paste("WHERE ((",multi.sp,") AND ((tblSpeciesInfo.Class)='Bird') AND ((lkpDistanceCenters.Distance) Is Not Null)")
  }else{
    unique.sp <- paste("(tblSpeciesInfo.Alpha)='",sp,"')",sep="")
    sp.selection <- paste("WHERE ((",unique.sp," AND ((tblSpeciesInfo.Class)='Bird') AND ((lkpDistanceCenters.Distance) Is Not Null)")
  }
  
  if(intransect){
  intransect.selection=paste("AND ((tblSighting.InTransect)=True))")
  }else{
  intransect.selection=paste(")") 
  }
  
  
  if(snapshot){
  snapshot.selection=paste("AND ((tblWatch.Snapshot)=True)")
  }else{
  snapshot.selection=paste("") 
  }
  
  lat.selection <-  paste("WHERE (((tblWatch.LatStart)>=",lat[1]," And (tblWatch.LatStart)<=",lat[2],")",sep="")  
  long.selection <- paste("AND ((tblWatch.LongStart)>=",long[1]," And (tblWatch.LongStart)<=",long[2],")",sep="")  
  Quebec <- paste("AND ((tblCruise.Quebec)=",Qc,")",sep="")
  year.selection <- paste("AND ((DatePart('yyyy',[Date]))Between ",years[1]," And ",years[2],"))",sep="")  

  #First select species
  my.query <- paste(paste("SELECT tblSpeciesInfo.Alpha",
                          "tblSpeciesInfo.English",
                          "tblSpeciesInfo.Latin",
                          "tblSpeciesInfo.Class",
                          "lkpDistanceCenters.Distance",
                          "tblSighting.FlockID",
                          "tblSighting.OldPiropID",
                          "tblSighting.WatchID",
                          "tblSighting.InTransect",
                          "tblSighting.Association",
                          "tblSighting.InexpAssoc",
                          "tblSighting.Behaviour",
                          "tblSighting.InexpFeed",
                          "tblSighting.FlightDir",
                          "tblSighting.FlySwim",
                          "tblSighting.SpecInfoID",
                          "tblSighting.Count",
                          "tblSighting.Age",
                          "tblSighting.Plumage",
                          "tblSighting.Sex",
                          "tblSighting.GroupID",
                          "tblSighting.InGroup", sep=", "),
                          paste("FROM tblSpeciesInfo",
                                "INNER JOIN (tblSighting LEFT JOIN lkpDistanceCenters ON tblSighting.Distance = lkpDistanceCenters.DistanceCode)",
                                "ON tblSpeciesInfo.SpecInfoID = tblSighting.SpecInfoID", sep=" "),
                          sp.selection,
                          intransect.selection,
                          sep=" ")
  
  #Run query and save in temporary table
  sp.query <- sqlQuery(channel1, my.query)
  sp.query <- subset(sp.query,  sp.query$Distance<300) 
  sqlSave(channel1, sp.query, tablename = "tblspselect")
  
  
  #Query for all the transects now
  my.query2<- paste(paste("SELECT tblCruise.[CruiseID] AS [CruiseID]",
                           "tblCruise.[Start Date] AS [StartDate]",
                           "tblCruise.[End Date] AS [EndDate]",
                           "tblWatch.WatchID AS [WatchID]",
                           "tblWatch.Observer AS [ObserverID]",
                           "tblWatch.Date AS [Date]",
                           "tblWatch.StartTime",
                           "tblWatch.EndTime",
                           "tblWatch.LatStart AS [LatStart]",
                           "tblWatch.LongStart AS [LongStart]",
                           "([PlatformSpeed]*[ObsLen]/60*1.852) AS [WatchLenKm]",
                           "tblspselect.FlySwim",
                           "tblspselect.Alpha AS [Alpha]",
                           "tblspselect.English",
                           "tblspselect.Latin",
                           "tblspselect.Distance AS [Distance]",
                           "tblspselect.InTransect",
                           "tblspselect.Count AS [Count]",
                           "tblWatch.Snapshot",
                           "tblWatch.ObservationType AS [Experience]",
                           "tblCruise.PlatformType AS [PlatformTypeID]",
                           "tblWatch.Visibility",
                           "tblWatch.SeaState",
                           "tblWatch.Swell",
                           "tblWatch.Weather",
                           "tblWatch.Glare",
                           "tblCruise.Quebec",
                           "DatePart('yyyy',[Date]) AS [Year]",
                           "DatePart('m',[Date]) AS [Month]",
                           "DatePart('ww',[Date]) AS Week",
                           "DatePart('y',[Date]) AS [Day]", sep=", "),
                      paste("FROM lkpObserver INNER JOIN (tblCruise INNER JOIN (lkpPlatformClass INNER JOIN",
                            "(lkpSeaState INNER JOIN (lkpScanType INNER JOIN (tblspselect",
                            "RIGHT JOIN tblWatch ON tblspselect.WatchID = tblWatch.WatchID)",
                            "ON lkpScanType.ScanTypeID = tblWatch.ScanType) ON lkpSeaState.SeaStateID = tblWatch.SeaState)",
                            "ON lkpPlatformClass.PlatformClassID = tblWatch.PlatformClass)",
                            "ON tblCruise.CruiseID = tblWatch.CruiseID) ON lkpObserver.ObserverID = tblWatch.Observer", sep=" "),
                     paste(lat.selection,
                           long.selection,
                           "AND ((([PlatformSpeed]*[ObsLen]/60*1.852)) Is Not Null And (([PlatformSpeed]*[ObsLen]/60*1.852))>0)",
                           Quebec,
                           snapshot.selection,
                           year.selection,
                           sep=" "),
                      paste("ORDER BY tblWatch.WatchID"), sep=" ")
  #Do query
  transects.df <- sqlQuery(channel1, my.query2)

  ######one year hack
  if(hack.year==TRUE){
    transects.df <- subset(transects.df ,transects.df$Year==max(years)) 
  }
  
  
  #Fix the observer problem
  my.query3<- "SELECT lkpObserver.ObserverName, lkpObserver.ObserverID FROM lkpObserver"
  observer.df <-  sqlQuery(channel1, my.query3)
  transects.df <- join(transects.df, observer.df , by="ObserverID")
  transects.df <-droplevels(transects.df)
  
  my.query4<- "SELECT lkpPlatformType.PlatformTypeID, lkpPlatformType.PlatformType FROM lkpPlatformType"          
  platform.df <-  sqlQuery(channel1, my.query4)
  transects.df <- join(transects.df, platform.df, by="PlatformTypeID")
  transects.df <-droplevels(transects.df)
  
  #Fix the Observer name
  transects.df$ObserverName <- as.factor(sapply(1:nrow(transects.df), function(i){gsub(", ","_",as.character(transects.df$ObserverName[i]) )}))
    
  #delete tempo table and close connection
  sqlDrop(channel1, "tblspselect")
  odbcCloseAll()
  
  #Check for observers
  if(!is.na(Obs.exclude)){
    keep1 <- setdiff(levels(transects.df$ObserverName), Obs.exclude)
    transects.df <- subset(transects.df, transects.df$ObserverName%in%keep1)
    transects.df <-droplevels(transects.df)
  }
 
  if(!is.na(Obs.keep)){
    transects.df <- subset(transects.df, transects.df$ObserverName%in%Obs.keep)
    transects.df <-droplevels(transects.df)
  }
  transects.df$ObserverID <- transects.df$ObserverName
 
 
  #Put in a nice form
  transects.df <- transects.df[,c("CruiseID","StartDate","EndDate","WatchID",        
                                 "Date","Year","Month","Week","Day", 
                                 "StartTime","EndTime","LatStart","LongStart","WatchLenKm",     
                                 "Alpha","English","Latin","Distance","Count",    
                                 "FlySwim","InTransect","Snapshot",       
                                 "ObserverID","Experience",
                                 "Visibility","SeaState","Swell","Weather", "Glare",
                                 "PlatformType")]
  
  #Return to the working drive
  setwd(wd)
  #Export the final product
  return(transects.df)
  #End
}
