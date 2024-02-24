ECSASconnect
=======
Import data from the Eastern Canada Seabirds at Sea (ECSAS) database

Installation
=======

    if(!require(devtools)) install.packages("devtools")
    devtools::install_github("RoyChristian/ECSASconnect")

Using with 64-bit R
=======
As of R 4.2 32-bit is no longer supported and all R sessions are 64-bit. To make ECSASconnect work you need to install the 64-bit ODBC driver
that comes with the 2010 ACCESS Redistributable at https://www.microsoft.com/en-US/download/details.aspx?id=13255. See also 
https://stackoverflow.com/questions/45064057/reading-data-from-32-bit-access-db-using-64-bit-r for more info.
