ECSASconnect
=======
Import data from the Eastern Canada Seabirds at Sea (ECSAS) database

Version 0.6.2
=======
* All changes below refer to ECSAS.extract
* Removed snapshot argument since it did not do what was intended.
* Added distMeth param (default 14) to extract only watches (and sightings) collected under the given distance sampling
 methodology. The default (14 see lkpDistMeth in ECSAS database) corresponds to "Flying birds: perpendicular distance;
 Swimming birds: perpendicular distance" and is now the default observation protocol used in ECSAS.
* Added detection of 32-bit R
* Fixed 1 year extraction temporary solution and made year argument optional
* Changed "database" argument to "sub.program" in keeping with ECSAS naming conventions.
* Fixed bug where seastate lookup code was being returned instead of seastate
* Changed "sp" argument to "species" and made it optional, removing the default "ATPU".
* changed default lat and long to include the whole planet
* Added FlockID, and Association from tblSighting to the output
* Added Association from tblSighting to the output. This is useful for filtering ship following birds (code 19) from analysis
* Added tblCruise.Program to output. Will later add a param to filter by program. This will give an easy way to filter
 out PIROP, etc and just select ECSAS data.

Version 0.6.1
=======
* Added a sanity check to make sure that the requested species where inclued in the database before extraction
* The extraction is not limited to the Quebec and Atlantic regions anymore and it's now possible to extract all the observations. 

Version 0.6.0
=======
* Made extensive changes to ECSAS.extract to manipulate the tables in R instad of ACCESS

Version 0.5.0
=======
* Modified ECSAS.extract to get the ship name in the extraction
* Modified ECSAS.extract weather and glare to the observation
* Modified ECSAS.extract to be compliant with the format of Master ECSAS v 3.33.mdb
* Implemented a temporary solution to the one year extaction to ECSAS.extract
* Implemented a temporary solution to the one year extaction to QC.extract
* Added the tblmissionselect.delete function and the associated help file
* Added the tblspselect.delete function and the associated help file

Version 0.4.0
=======
* Imported files on Github
