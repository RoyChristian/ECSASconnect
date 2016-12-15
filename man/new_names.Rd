\name{new_names}
\alias{new_names}
\docType{data}
\title{
Correspondance between old names and new names
}
\description{
This table gives the correspondance between old names in raw data and in the old access database and the new names in the SOMEC database and future data.
}
\usage{data("new_names")}
\format{
  A data frame with 06 columns.
  \describe{
    \item{\code{table}}{The table where the name is found}
    \item{\code{type}}{The type of data in the access databse}
    \item{\code{data_name}}{The old name in the raw compilation excel files}
    \item{\code{access_name}}{The name in the old access database}
    \item{\code{new_name}}{The new name to use in the excel and access database}
    \item{\code{name_EN}}{Undefined}
  }
}
\details{
This table is use to manipulate the older data. 
}
\references{
}
\examples{
data(new_names)
}
\keyword{datasets}
