\name{ECSAS.extract}
\alias{ECSAS.extract}
\title{
Extract the information for the Global ECSAS database
}
\description{
The function will connect to the Access database, create a series of queries and import the desired information in a data frame.
}
\usage{
ECSAS.extract(sp = "ATPU", years = c(2006, 2013), lat = c(30, 70), long = c(-70, -30), 
             Obs.keep = NA, Obs.exclude = NA, Qc = F, snapshot = T, intransect = T, 
             ecsas.drive = "C:/Users/CR001/ECSAS/Data", 
             ecsas.file = "Master ECSAS_backend v 3.31.mdb")
}
\arguments{
  \item{sp}{
Alpha code for the specie desired in the extraction. If more than oine species all 
the desired species must be saved into a vector ex: c("COMU,"TBMU", "UNMU") 
}
  \item{years}{
Years desired for the extraction
}
  \item{lat}{
Pairs of coordinate giving the southern and northern limits of the range desired. 
}
  \item{long}{
Pairs of coordinate giving the western and eastern limits of the range desired. 
Note that longitude values must be negative.
}
  \item{Obs.keep}{
Name of the observer(s) to keep for the extraction. The name of the observer 
must be followed by it's first name (eg: "Bolduc_Francois").
}
  \item{Obs.exclude}{
Name of the observer(s) to exclude for the extraction. The name of the observer 
must be followed by it's first name (eg: "Bolduc_Francois").
}
  \item{Qc}{
Should we exclude the data collected in the Quebec program (TRUE) or not (FALSE)
}
  \item{snapshot}{
Should we keep only the observations were the Snapshot method was used (TRUE) or not (FALSE) 
}
  \item{intransect}{
Should we keep only the observation that are in the transect (TRUE) or not (FALSE) 
}
  \item{ecsas.drive}{
Directory where the ECAS database is saved
}
  \item{ecsas.file}{
Name of the ECAS database with the file extension included. 
}
}
\details{
The function will produce a data frame that will contains all the pertinent information. 
}
}
\author{
Christian Roy
}
\note{
The SQL queries used by this function are based on the SQL queries included in the FrontEnd of the database. 
}
\seealso{
\code{\link{QC.extract}}\cr
}
\examples{
#Not run
#path_to_ecsas <- "C:/Users/CR001/ECSAS/Data" # Change for each user
#name_ecsas <- "Master ECSAS_backend v 3.31.mdb"
#foo <- ECAS.extract(sp="RAZO",  years=c(2008,2010), lat=c(30,70), 
#                    long=c(-70, -30), Obs.keep="Bolduc_Francois", Obs.exclude=NA,
#                         snapshot=T, intransect=T, ecsas.drive= path_to_ecsas,
#                         ecsas.file=name.ecsas)
#str(foo)
#head(foo)
#End
