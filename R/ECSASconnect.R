# Print message when user executes "library(ECSASconnect). Shamelessly borrowed from mgcv.
.onAttach <- function(...) {
  library(help=ECSASconnect)$info[[1]] -> version

  if (!is.null(version)) {
    version <- version[pmatch("Version",version)]
    um <- strsplit(version," ")[[1]]
    version <- um[nchar(um)>0][2]
  } else {
    version <- "Unknown version"
  }

  hello <- paste("This is ECSASconnect ", version, ".")
  packageStartupMessage(hello)
}


