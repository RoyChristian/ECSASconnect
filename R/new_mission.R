### A function that creates ? template for a new mission with three sheets transects obervationss and missions based on the nom_colonnes_SOMEC-QC.xlsx



new_mission<-function(
  
  input=NULL,
  output="new_mission"
  
){
  ext<-if(is.null(input)){".data"}else{substr(input,nchar(input)-4,nchar(input))}
  if(!is.null(input) & !any(grep("/",input))){
    input<-paste0(getwd(),"/",input)  	
  }
  path<-if(is.null(input)){getwd()}else{unlist(strsplit(input,"/"))}
  if(length(path)==1){
    path<-getwd()
  }else{
    path<-paste(path[-length(path)],collapse="/")
  }	
  sheets<-c("observations","transects","missions")
  if(ext=="accdb"){
    db<-odbcConnectAccess2007(input)
    on.exit(odbcClose(db))
    obs<-sqlFetch(db,"observations",stringsAsFactors=FALSE,max=1)
    tran<-sqlFetch(db,"transects",stringsAsFactors=FALSE,max=1)
    mis<-sqlFetch(db,"missions",stringsAsFactors=FALSE,max=1)
    f<-list(obs,tran,mis)
    names(f)<-sheets
    write.xlsx(f, paste0(path,"/",output,".xlsx"), row.names=FALSE)
  }else{
    if(ext==".xlsx"){
      names<-as.data.frame(read_excel(input))
    }else{
      data(new_names)
      nms<-new_names
    }
    sheet.list <- lapply(sheets, create_worksheet, nms, path, output)
    names(sheet.list) <- sheets
    write.xlsx(sheet.list, paste0(path,"/",output,".xlsx"), row.names=FALSE)
  }
  cat("File created at ",paste0(path,"/",output,".xlsx"))
}

# return a dataframe corresponding to a worksheet
create_worksheet <- function(i, nms, path, output){
  n<-unique(nms[nms$table==i,"new_name"]) #take out duplicates because of multiple names
  f<-as.data.frame(matrix(rep(NA,length(n)),ncol=length(n)))
  names(f)<-n
  f
}


