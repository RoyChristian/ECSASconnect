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
Version: \tab 0.6.1\cr
Date: \tab 2016-05-09\cr
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
#code <- c("ALCI", "RAZO")
#pathECSAS<-"C:/Users/royc/Documents/My Data/ECSAS"
#fileECSAS<-"Master ECSAS v 3.33.mdb"
#foo.df <- ECSAS.extract(sp=code,  years=c(1800,2017), lat=c(-89,89), long=c(-120, 120), Obs.keep=NA, Obs.exclude=NA, 
#                         database="All", snapshot=F, intransect=T, ecsas.drive=pathECSAS, ecsas.file=fileECSAS)
#str(foo.df)
#head(foo.df)
#End
}
