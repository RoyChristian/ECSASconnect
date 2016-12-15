\name{new_mission}
\alias{new_mission}
\title{
Build a template for a new mission
}
\description{
This function creates an .xlsx excel files to use as a template for a new mission.
}
\usage{
new_mission(input=NULL,output="new_mission")
}
\arguments{
  \item{input}{Name and path to the .xlsx or .accdb file containing appropriate names for generating a mission template. If the file is an .xlsx file, it has to contain a column "table" giving the type of table (observations, transects or missions) and a column "new_name" giving the names to be used. If the file given is a .accdb file (the SOMEC-QC database), names will simply be taken from the corresponding tables. If \code{input = NULL}, names will be taken from the \code{\link{new_names}} dataset.}
  \item{output}{Name of the .xlsx file created. The file will be created in the same directory as the input file.}
}
\details{
A new file should be generated before each new mission and used to gather and collect the data.
}
\value{
An .xlsx file written at the given location.
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
\code{\link{data2SOMEC}}, \code{\link{new_names}}
}
\examples{
\dontrun{new_mission(output="new_cruise")}
}
