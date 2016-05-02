\name{ECASconnect-package}
\alias{ECASconnect-package}
\alias{ECASconnect}
\docType{package}
\title{
Import data from the Eastern Canada Seabirds at Sea (ECAS) database
}
\description{
This package will allow the user extract data from the ECAS Access database. 
The package will work only with R-32 bits since the drivers for Access only exist in 32 bits.
}
\details{
\tabular{ll}{
Package: \tab ECASconnect\cr
Type: \tab Package\cr
Version: \tab 0.6\cr
Date: \tab 2016-05-02\cr
License: \tab GPL-2\cr
}
The package contains only two functions. One for the global ECAS database and one for the database in Quebec. The output of the two functions are slightly different because of differences in the database.   
}
\author{
Christian Roy

Maintainer: Francois Bolduc <francois.Bolduc@ec.gc.ca>
}

\examples{
#Not run
#path_to_ecas <- "C:/Database/CWS/ECSAS" # Change for each user
#name_ecas <- "Master ECSAS_backend v 3.31.mdb"
#foo <- ECAS.extract(sp="RAZO",  years=c(2008,2010), lat=c(30,70), long=c(-70, -30), Obs.keep="Bolduc_Francois", Obs.exclude=NA,
#                         snapshot=T, intransect=T, ecas.drive= path_to_ecas,
#                         ecas.file=name.ecas)
#str(foo)
#head(foo)
#End
}
