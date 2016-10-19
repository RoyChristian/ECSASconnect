### A function that creates ? template for a new mission with three sheets transects obervationss and missions based on the nom_colonnes_SOMEC-QC.xlsx



new_mission<-function(
  
  input="C:/Users/User/Documents/SCF2016_FR/ECSASconnect/noms_colonnes_SOMEC-QC.xlsx",
  output="new_mission"
  
){
  
  ext<-substr(input,nchar(input)-4,nchar(input))
  if(!(ext%in%c(".xlsx","accdb"))){
    stop(paste("No .xlsx or .accdb file name given in input:",input))
  }
  if(!any(grep("/",input))){
    input<-paste0(getwd(),"/",input)  	
  }
  path<-unlist(strsplit(input,"/"))
  if(length(path)==1){
    path<-getwd()
  }else{
    path<-paste(path[-length(path)],collapse="/")
  }	
  sheets<-c("observations","transects","missions")
  if(ext==".xlsx"){
    names <- as.data.frame(read_excel(input))
    sapply(sheets,function(i){
      n<-names[names$table==i,"new_name"]
      f<-as.data.frame(matrix(rep(NA,length(n)),ncol=length(n)))
      names(f)<-n
      write.xlsx(f,paste0(path,"/",output,".xlsx"),sheetName=i,showNA=FALSE,append=TRUE,row.names=FALSE)
    })    
  }else{
    db<-odbcConnectAccess2007(input)
    obs<-sqlFetch(db,"observations",stringsAsFactors=FALSE,max=1)
    tran<-sqlFetch(db,"transects",stringsAsFactors=FALSE,max=1)
    mis<-sqlFetch(db,"missions",stringsAsFactors=FALSE,max=1)
    on.exit(odbcClose(db))
    f<-list(obs,tran,mis)
    names(f)<-sheets
    sapply(sheets,function(i){
      write.xlsx(f[[i]][NA,],paste0(path,"/",output,".xlsx"),sheetName=i,showNA=FALSE,append=TRUE,row.names=FALSE)
    })
  }
  cat("File created at ",paste0(path,"/",output,".xlsx"))
}