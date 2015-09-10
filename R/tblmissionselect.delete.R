#' @export
#'@title MAke sute that the table tblmissionselect doesnt exist in the ECSAS database
#'
#'@description Sometimes the table tblmissionselect is not properly deleted from the ECSAS database after the extraction. This function make sure that the file is properly deleted.
#'@param ecsas.drive Where is located the ECSAS Access database
#'@param ecsas.file  What is the name of the ECSAS Access database
#'@details
#'Delete the table tblmissionselect form the ECSAS database
#'@section Author:Christian Roy

tblmissionselect.delete<- function(ecsas.drive = "C:/Users/Christian/Google Drive/ECSAS/", ecsas.file ="Oiseaux marins 2006-2014.accdb"){
  wd<-getwd()
  setwd(ecsas.drive)
  channel1 <- odbcConnectAccess(ecsas.file, uid="")
  sqlDrop(channel1, "tblmissionselect")
  odbcCloseAll()
}