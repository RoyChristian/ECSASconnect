\name{data2SOMEC}
\alias{data2SOMEC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Import raw survey data to the SOMEC database.
}
\description{
This function takes an excel file with the data from a mission and imports it into the SOMEC access database.
}
\usage{
data2SOMEC(input,dbpath,file_name,prefix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{Name and path to the .xlsx file to add to the SOMEC access database.}
  \item{dbpath}{Name and path to the SOMEC access database.}
  \item{file_name}{Name of the .xlsx file that contains the appropriate file names.}
  \item{prefix}{Name of the prefix used to build the id of a mission. The prefix will be pasted to the start date of the mission to build the id (e.g. "HUD")}
%%     ~~Describe \code{x} here~~
}
}
\details{
The new data will be appended to the database, namely to the three tables transects, observations and missions.
}
\value{
Lines are added to the access database.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Francois Rousseu
}
\note{
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
