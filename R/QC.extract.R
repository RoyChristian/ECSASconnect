QC.extract <-
function(sp="ATPU",  years=c(2006,2013), lat=c(30,70), long=c(-70, -30), 
                       intransect=T, Obs.keep=NA, Obs.exclude=NA,
                       ecsas.drive="C:/Users/christian/Dropbox/ECSAS", ecsas.file="Oiseaux marins 2006-2014.accdb"){

  wd<-getwd()
  setwd(ecsas.drive)
  channel1 <- odbcConnectAccess2007(ecsas.file, uid="")
  
  #Make sure the tempo table don't exist
  try(sqlDrop(channel1, "tblspselect"), silent=T)
  try(sqlDrop(channel1, "tblmissionselect"), silent=T)


  #write queries
  if(length(sp)>=2){
    multi.sp <- paste0(sapply(1:length(sp),function(i){paste("([Code espèces].[Code AN])='",sp[i],"'",sep="")}), collapse=" Or ")
    sp.selection <- paste("WHERE ((",multi.sp,") AND ((OBSERVATION.[Distance parallele]) Like '[A-D]') AND ((OBSERVATION.Mission)<>'NA')")
  }else{
    unique.sp <- paste("([Code espèces].[Code AN])='",sp,"')",sep="")
    sp.selection <- paste("WHERE ((",unique.sp," AND ((OBSERVATION.[Distance parallele]) Like '[A-D]') AND ((OBSERVATION.Mission)<>'NA')")
  }

  if(intransect){
  intransect.selection=paste(" AND ((OBSERVATION.InTransect) ='En cours'))")
  }else{
  intransect.selection=paste(")") 
  }


  #write queries
  lat.selection <-  paste("WHERE (((Transect.Latitude)>=",lat[1]," And (Transect.Latitude)<",lat[2],")",sep="")  
  long.selection <- paste("AND ((Transect.Longitude)>=",long[1]," And (Transect.Longitude)<-",long[2],")",sep="")  
  year.selection <- paste("AND ((DatePart('yyyy',[Date]))Between ",years[1]," And ",years[2],"))",sep="")  
  

  my.query <-  paste(paste("SELECT [Code espèces].[Code AN] As[Alpha]", 
                   "[Code espèces].[Nom AN] As [English]", 
                   "[Code espèces].[Nom LAT] As [Latin]", 
                   "OBSERVATION.[Distance parallele] As [Distance]", 
                   "OBSERVATION.[Nb individu] As [Count]",
                   "OBSERVATION.[ID transect] As [ID]", 
                   "OBSERVATION.Mission", 
                   "OBSERVATION.Snapshot As [FlySwim]", 
                   "OBSERVATION.InTransect",sep=", "),
              paste("FROM [Code Distance Par] INNER JOIN ([Code espèces] INNER JOIN OBSERVATION", 
                    "ON [Code espèces].[Code FR] = OBSERVATION.[Code espece])", 
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
                             "[Code Observateurs].Prénom", 
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
    transects.df$ObserverID <-as.factor(paste(transects.df$Nom,transects.df$Prénom, sep="_"))
    levels(transects.df$FlySwim)<-c("F","W")
    
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
