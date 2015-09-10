#' @export
#'@title Extract the information for the Quebec ECSAS database
#'
#'@description The function will connect to the Access database, create a series of queries and import the desired information in a data frame.
#'@param sp Alpha code for the specie desired in the extraction. If more than one species all the desired species must be saved into a vector ex: c("COMU,"TBMU", "UNMU") 
#'@param years Years desired for the extraction
#'@param lat Pairs of coordinate giving the southern and northern limits of the range desired.
#'@param long Pairs of coordinate giving the western and eastern limits of the range desired. Note that longitude values must be negative.
#'@param obs.keep Name of the observer to keep for the extraction. The name of the observer must be followed by it's first name (eg: "Bolduc_Francois").
#'@param Obs.exclude Name of the observer to exlude for the extraction.The name of the observer must be followed by it's first name (eg: "Bolduc_Francois").
#'@param intransect Should we keep only the birds counted on the transect or not. 
#'@param ecsas.drive Where is located the Quebec ECSAS Access database
#'@param ecsas.file  What is the name of the Quebec ECSAS Access database
#'@details
#'The function will produce a data frame that will contains all the pertinent information. 
#'@section Author:Christian Roy
#'
#'@seealso \code{\link{ECSAS.extract}}


QC.extract <-
function(sp="ATPU",  years=c(2006,2013), lat=c(30,70), long=c(-70, -30), 
                       Obs.keep=NA, Obs.exclude=NA, intransect=T, 
                       ecsas.drive="C:/Users/christian/Dropbox/ECSAS", ecsas.file="Oiseaux marins 2006-2014.accdb"){

  wd<-getwd()
  setwd(ecsas.drive)
  channel1 <- odbcConnectAccess2007(ecsas.file, uid="")
  
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
  #Make sure the tempo table doesn't exist
  if("tblmissionselect"%in%sqlTables(channel1)$TABLE_NAME){
    sqlDrop(channel1, "tblmissionselect")
  }
  
  
  #write queries
  if(length(sp)>=2){
    multi.sp <- paste0(sapply(1:length(sp),function(i){paste("([Code esp?ces].[Code AN])='",sp[i],"'",sep="")}), collapse=" Or ")
    sp.selection <- paste("WHERE ((",multi.sp,") AND ((OBSERVATION.[Distance parallele]) Like '[A-D]') AND ((OBSERVATION.Mission)<>'NA')")
  }else{
    unique.sp <- paste("([Code esp?ces].[Code AN])='",sp,"')",sep="")
    sp.selection <- paste("WHERE ((",unique.sp," AND ((OBSERVATION.[Distance parallele]) Like '[A-D]') AND ((OBSERVATION.Mission)<>'NA')")
  }

  if(intransect){
  intransect.selection=paste(" AND ((OBSERVATION.InTransect) ='En cours'))")
  }else{
  intransect.selection=paste(")") 
  }

  #write queries
  lat.selection <-  paste("WHERE (((Transect.Latitude)>=",lat[1]," And (Transect.Latitude)<=",lat[2],")",sep="")  
  long.selection <- paste("AND ((Transect.Longitude)>=",long[1]," And (Transect.Longitude)<=",long[2],")",sep="")  
  year.selection <- paste("AND ((DatePart('yyyy',[Date]))Between ",years[1]," And ",years[2],"))",sep="")  
  

  my.query <-  paste(paste("SELECT [Code esp?ces].[Code AN] As[Alpha]", 
                   "[Code esp?ces].[Nom AN] As [English]", 
                   "[Code esp?ces].[Nom LAT] As [Latin]", 
                   "OBSERVATION.[Distance parallele] As [Distance]", 
                   "OBSERVATION.[Nb individu] As [Count]",
                   "OBSERVATION.[ID transect] As [ID]", 
                   "OBSERVATION.Mission", 
                   "OBSERVATION.Snapshot As [FlySwim]", 
                   "OBSERVATION.InTransect",sep=", "),
              paste("FROM [Code Distance Par] INNER JOIN ([Code esp?ces] INNER JOIN OBSERVATION", 
                    "ON [Code esp?ces].[Code FR] = OBSERVATION.[Code espece])", 
                    "ON [Code Distance Par].[Code distance] = OBSERVATION.[Distance parallele]",sep=" "),
              sp.selection, 
              intransect.selection,
              sep=" ")
  
  
    sp.query <- sqlQuery(channel1, my.query)
    levels(sp.query$FlySwim) <-c("Swim", "Swim", "Fly", "Fly")
    levels(sp.query$Distance)<- c("25","75","150","250")
    sp.query$Distance <- as.numeric(as.character(sp.query$Distance))
    sqlSave(channel1, sp.query, tablename = "tblspselect")
    
    
    my.query2 <-  paste(paste("SELECT Mission.Mission",
                              "Mission.[Date Debut] AS [StartDate]",
                              "Mission.[Date Fin] AS [EndDate]",
                              "Mission.[Nom Plateforme]",
                              "Mission.[Observateur 1]",sep=", "),
                        paste("FROM Mission"),
                        paste("WHERE (((Mission.[Nom Plateforme])Not In ('Avion','Partenavia','Islander'))",
                              "AND ((Mission.[Type transect])<>'R'))", sep=" "),
                        sep=" ")
  
    mission.query  <- sqlQuery(channel1, my.query2)
    mission.query$StartDate <-as.character(mission.query$StartDate)
    mission.query$EndDate <-as.character(mission.query$EndDate)
    sqlSave(channel1, mission.query, tablename = "tblmissionselect")
    
    my.query3 <- paste(paste("SELECT tblmissionselect.Mission AS CruiseID", 
                             "tblmissionselect.StartDate", 
                             "tblmissionselect.EndDate", 
                             "Transect.ID AS WatchID", 
                             "[Code Observateurs].Nom", 
                             "[Code Observateurs].Pr?nom", 
                             "Transect.Date",
                             "Transect.Date_heure AS [StartTime]",
                             "Transect.Latitude AS LatStart", 
                             "Transect.Longitude AS LongStart", 
                             "[VitPlateforme(knts)]*0.5399568*5/60 AS WatchLenKm", 
                             "tblspselect.Alpha", 
                             "tblspselect.English", 
                             "tblspselect.Latin", 
                             "tblspselect.Distance", 
                             "tblspselect.FlySwim", 
                             "tblspselect.InTransect",
                             "tblspselect.Count", 
                             "Transect.Visibilite AS Visibility", 
                             "Transect.Mer AS SeaState", 
                             "Transect.[Vagues(m)] AS Swell", 
                             "[Code plateforme].[Type de plateforme] AS PlatformType", 
                             "DatePart('yyyy',[Date]) AS [Year]",
                             "DatePart('m',[Date]) AS [Month]",
                             "DatePart('ww',[Date]) AS [Week]",
                             "DatePart('y',[Date]) AS [Day]", sep=", "),
                        paste("FROM [Code plateforme] RIGHT JOIN ((tblspselect RIGHT JOIN Transect", 
                              "ON tblspselect.ID = Transect.ID) LEFT JOIN ([Code Observateurs]", 
                              "RIGHT JOIN tblmissionselect ON [Code Observateurs].[Code observateur] = tblmissionselect.Observateur1)", 
                              "ON Transect.Mission = tblmissionselect.Mission)", 
                              "ON [Code plateforme].[Nom plateforme] = tblmissionselect.NomPlateforme", sep=" "),
                       paste(lat.selection,
                             long.selection,
                             "AND ((tblmissionselect.Mission)<>'NA')",
                             year.selection,
                             sep=" "),
                       paste("ORDER BY Transect.ID"), sep=" ")
    
  
    #Do query
    transects.df <- sqlQuery(channel1, my.query3)
    transects.df$ObserverID <-as.factor(paste(transects.df$Nom,transects.df$Pr?nom, sep="_"))
    levels(transects.df$FlySwim)<-c("F","W")
    
    ######one year hack
    if(hack.year==TRUE){
      transects.df <- subset(transects.df ,transects.df$Year==max(years)) 
    }
    
    #delete tempo table and close connection
    sqlDrop(channel1, "tblspselect")
    sqlDrop(channel1, "tblmissionselect")
    odbcCloseAll()
  
    #Check for observers
    if(!is.na(Obs.exclude)){
      keep1 <- setdiff(levels(transects.df$ObserverID), Obs.exclude)
      transects.df <- subset(transects.df, transects.df$ObserverID%in%keep1)
      transects.df <-droplevels(transects.df)
    }
  
    if(!is.na(Obs.keep)){
      transects.df <- subset(transects.df, transects.df$ObserverID%in%Obs.keep)
      transects.df <-droplevels(transects.df)
    }
  
  
    #Put in a nice form
    transects.df <- transects.df[,c("CruiseID","StartDate","EndDate","WatchID",        
                                   "Date","Year","Month","Week","Day", 
                                   "StartTime","LatStart","LongStart","WatchLenKm",     
                                   "Alpha","English","Latin","Distance","Count",    
                                   "FlySwim","InTransect",      
                                   "ObserverID","Visibility","SeaState","Swell","PlatformType")]
    #Return to the working drive
    setwd(wd)
    #Export the final product
    return(transects.df)
    #End
}
