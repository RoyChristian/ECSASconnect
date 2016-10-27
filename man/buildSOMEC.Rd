\name{buildSOMEC}
\alias{buildSOMEC}
\title{
Build new SOMEC database
}
\description{
Builds the new SOMEC database from the old OiseauxMarins2006-2014.accdb databse
}
\usage{
buildSOMEC(input="OiseauxMarins2006-2014.accdb",del.old=TRUE)
}
\arguments{
  \item{input}{Path to the old database with its name}
  \item{del.old}{Delete old tables Transect, OBSERVATION and Mission?}
}
\details{
Makes a copy of the old database and then adds 4 new tables (transects, observations, missions and codes_colonnes). 
}
\value{
An access database.
}
\author{
Francois Rousseu
}
\seealso{
\code{\link{data2SOMEC}}
}

