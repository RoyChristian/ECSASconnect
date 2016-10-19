\name{QC.extract}
\alias{QC.extract}
\title{
Extract the information for the Quebec ECAS database
}
\description{
The function will connect to the Access database, create a series of queries and import the desired information in a data frame. 
Observations made though the airplane program or with the angular method are rejected by default.  
}
\usage{
QC.extract(sp = "ATPU", years = c(2006, 2013), lat = c(30, 70), long = c(-70, -30), 
           intransect = T, Obs.keep = NA, Obs.exclude = NA, 
           ecsas.drive = "C:/Users/CR001/ECSAS/Data", 
           ecsas.file = "Oiseaux marins 2006-2014.accdb")
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
  \item{intransect}{
Should we keep only the observation that are in the transect (TRUE) or not (FALSE) 
}
  \item{Obs.keep}{
Name of the observer(s) to keep for the extraction. The name of the observer 
must be followed by it's first name (eg: "Bolduc_Francois").
}
  \item{Obs.exclude}{
Name of the observer(s) to exclude for the extraction. The name of the observer 
must be followed by it's first name (eg: "Bolduc_Francois").
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
\author{
Christian Roy
}
\note{
The SQL queries used by this function are based on the SQL queries included in the FrontEnd of the database. 
}
\seealso{
\code{\link{ECAS.extract}}\cr
}
\examples{
#Not run
#path_to_ecsas <- "C:/Users/CR001/ECSAS/Data" # Change for each user
#name_ecsas <- "Oiseaux marins 2006-2014.accdb"
#foo <- QC.extract(sp="ATPU",  years=c(2011,2013), lat=c(30,70), 
#                    long=c(-70, -30), Obs.keep=NA, Obs.exclude=NA),
#                    ecsas.drive= path_to_ecsas , ecsas.file= name_ecsas)
#str(foo)
#head(foo)
#End
}
