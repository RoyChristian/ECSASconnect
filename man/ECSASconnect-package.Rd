\name{ECASconnect-package}
\alias{ECASconnect-package}
\alias{ECASconnect}
\docType{package}
\title{
Import and export data from the Eastern Canada Seabirds at Sea (ECSAS) and the SOMEC database (Suivi des oiseaux en mer de l'est du Canada).
}
\description{
This package will allow the user extract data from the ECSAS Access database and to export data from the Quebec SOMEC databse. 
The package will work only with R-32 bits since the drivers for Access only exist in 32 bits.
}
\details{
\tabular{ll}{
Package: \tab ECASconnect\cr
Type: \tab Package\cr
Version: \tab 0.6.2\cr
Date: \tab 2016-10-18\cr
License: \tab GPL-2\cr
}
The package contains only two functions for extracting data from the ECSAS database. One for the global ECSAS database and one for the database in Quebec. The output of the two functions are slightly different because of differences in the database.   
}
\author{
Christian Roy, Francois Rousseu

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
