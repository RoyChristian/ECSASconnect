#' @export
#'@title MAke sute that the table tblspselect doesnt exist in the ECSAS database
#'
#'@description Sometimes the table tblspselect is not properly deleted from the ECSAS database after the extraction. This function make sure that the file is properly deleted.
#'@param ecsas.drive Where is located the ECSAS Access database
#'@param ecsas.file  What is the name of the ECSAS Access database
#'@details
#'Delete the table tblspselect form the ECSAS database
#'@section Author:Christian Roy

tblspselect.delete<- function(ecsas.drive = "C:/Users/Christian/Google Drive/ECSAS/", ecsas.file ="Master ECSAS v 3.33.mdb"){
  wd<-getwd()
  setwd(ecsas.drive)
  channel1 <- odbcConnectAccess(ecsas.file, uid="")
  sqlDrop(channel1, "tblspselect")
  odbcCloseAll()
}