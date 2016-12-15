
buildSOMEC<-function(
  
  input="OiseauxMarins2006-2014.accdb",
  del.old=TRUE
  
){  

  ### get new column names
  #names <- read_excel("C:/Users/User/Documents/SCF2016_FR/ECSASconnect/noms_colonnes_SOMEC-QC.xlsx") 
  data(new_names)
  namesT<-new_names[new_names[,"table"]=="transects",]
  namesO<-new_names[new_names[,"table"]=="observations",]
  namesM<-new_names[new_names[,"table"]=="missions",]

  ### Connect to access old backup database
  db_old <- odbcConnectAccess2007(input)
  on.exit(odbcCloseAll())
  transects<-sqlFetch(db_old,"Transect",as.is=TRUE)
  observations<-sqlFetch(db_old,"OBSERVATION",as.is=TRUE)
  missions<-sqlFetch(db_old,"Mission",as.is=TRUE)
  
  m<-match(names(transects),namesT$access_name)
  names(transects)<-namesT$new_name[m]

  m<-match(names(observations),namesO$access_name)
  names(observations)<-namesO$new_name[m]

  m<-match(names(missions),namesM$access_name)
  names(missions)<-namesM$new_name[m]
  
  path<-unlist(strsplit(input,"/"))
  if(length(path)==1){
    path<-getwd()  
  }else{
    path<-paste0(path[-length(path)],collapse="/")
  }
  
  file.copy(input,paste0(path,"/SOMEC.accdb"))

  ### connect to new database (use a copy from the backup and delete the three tables (Transect, OBSERVATION, Mission) from the copy after the update at the end)
  db <- odbcConnectAccess2007(paste0(path,"/SOMEC.accdb"))
  on.exit(odbcCloseAll())
  
  # export new transects table
  tmp<-sqlColumns(db, "Transect")
  varTypes<-tolower(as.character(tmp$TYPE_NAME))
  names(varTypes)<-names(transects)
  sqlSave(db,transects,tablename=NULL,append=TRUE,rownames=FALSE,addPK=FALSE,fast=TRUE,colnames=FALSE,varTypes=varTypes)

  # export new observations table
  tmp<-sqlColumns(db, "OBSERVATION")
  varTypes<-tolower(as.character(tmp$TYPE_NAME))
  names(varTypes)<-names(observations)
  sqlSave(db,observations,tablename=NULL,append=TRUE,rownames=FALSE,addPK=FALSE,fast=TRUE,colnames=FALSE,varTypes=varTypes)

  # export new missions table
  tmp<-sqlColumns(db, "Mission")
  varTypes<-tolower(as.character(tmp$TYPE_NAME))
  names(varTypes)<-names(missions)
  sqlSave(db,missions,tablename=NULL,append=TRUE,rownames=FALSE,addPK=FALSE,fast=TRUE,colnames=FALSE,varTypes=varTypes)
  
  # export column names information from old database and raw data
  codes_colonnes<-new_names
  sqlSave(db,codes_colonnes,tablename=NULL,rownames=FALSE,addPK=FALSE,fast=TRUE,colnames=FALSE)

  # drop old tables
  if(del.old){
    sqlDrop(db,"Transect")
    sqlDrop(db,"OBSERVATION")
    sqlDrop(db,"Mission")
  }
  
}




