### A function that creates ? template for a new mission with three sheets transects obervationss and missions based on the nom_colonnes_SOMEC-QC.xlsx



new_mission<-function(
  
  input="C:/Users/User/Documents/SCF2016_FR/ECSASconnect/noms_colonnes_SOMEC-QC.xlsx",
  output="nouvelle_mission"
  
){
  
  if(substr(input,nchar(input)-4,nchar(input))!=".xlsx"){
    stop(paste("No .xlsx file name given in input:",x))
  }
  if(!any(grep("/",input))){
    input<-paste0(getwd(),"/",input)  	
  }
  names <- as.data.frame(read_excel(input)) 
  path<-unlist(strsplit(input,"/"))
  if(length(path)==1){
    path<-getwd()
  }else{
    path<-paste(path[-length(path)],collapse="/")
  }	
  sapply(unique(names[,"table"]),function(i){
    n<-names[names$table==i,"new_name"]
    f<-as.data.frame(matrix(rep(NA,length(n)),ncol=length(n)))
    names(f)<-n
    write.xlsx(f,paste0(path,"/",output,".xlsx"),sheetName=i,showNA=FALSE,append=TRUE,row.names=FALSE)
  })
  cat("File created at ",paste0(path,"/",output,".xlsx"))
  
}