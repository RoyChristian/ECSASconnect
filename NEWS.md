## Version 0.6.9 
- specifying "species" argument to ECSAS.extract() caused a malformed SQL clause that
contained a condition for every row in SpececiesInfo that had NA Alpha field
- specifying more than one species (ie. species = c("DOVE", "TBMU")) in ECSAS.extract() caused an
error message claiming only one species could be specified, contrary to the
docs.
- added "include.empty.watches" (default TRUE) argument to ECSAS.extract(). When TRUE,
there is a single row in the returned dataframe for each watch where the
specified species was not observed. When FALSE, watches with no observations
(of the specified species) are not returned.



## Version 0.6.5
* Improvements to SOMEC2ECSAS
* SeaState now appears in output again.

## Version 0.6.4
* Changed default for distMeth to be c(14,20) to include by default watches where both perpendicular and radial distances were recorded 


## Version 0.6.3
* Added several new functions for importing new data to the QC SOMEC access database and exporting this data to the ECSAS database. 
* Added a new version of the database argument (now called sub.program) in ECSAS.extract and columns for the different sub.programs are also extracted
* Added the possibility to select more than one distMeth
* Fixed a potential bug that could cause confusion between function rename from package plyr or dplyr
* Made arguments obs.keep ans obs.exclude more consistent with lower cases

## Version 0.6.2
* All changes below refer to ECSAS.extract
* Removed snapshot argument since it did not do what was intended.
* Added distMeth param (default 14) to extract only watches (and sightings) collected under the given distance sampling
 methodology. The default (14 see lkpDistMeth in ECSAS database) corresponds to "Flying birds: perpendicular distance;
 Swimming birds: perpendicular distance" and is now the default observation protocol used in ECSAS.
* Added detection of 32-bit R
* Fixed 1 year extraction temporary solution and made year argument optional
* Fixed bug where seastate lookup code was being returned instead of seastate
* Changed "sp" argument to "species" and made it optional, removing the default "ATPU".
* changed default lat and long to include the whole planet
* Added FlockID, and Association from tblSighting to the output
* Added Association from tblSighting to the output. This is useful for filtering ship following birds (code 19) from analysis
* Added tblCruise.Program to output. Will later add a param to filter by program. This will give an easy way to filter
 out PIROP, etc and just select ECSAS data.

## Version 0.6.1
* Added a sanity check to make sure that the requested species where inclued in the database before extraction
* The extraction is not limited to the Quebec and Atlantic regions anymore and it's now possible to extract all the observations. 

## Version 0.6.0
* Made extensive changes to ECSAS.extract to manipulate the tables in R instad of ACCESS

## Version 0.5.0
* Modified ECSAS.extract to get the ship name in the extraction
* Modified ECSAS.extract weather and glare to the observation
* Modified ECSAS.extract to be compliant with the format of Master ECSAS v 3.33.mdb
* Implemented a temporary solution to the one year extaction to ECSAS.extract
* Implemented a temporary solution to the one year extaction to QC.extract
* Added the tblmissionselect.delete function and the associated help file
* Added the tblspselect.delete function and the associated help file

## Version 0.4.0
* Imported files on Github

## TODO LIST FR

- Check if read_excel scraps some information in columns "commentaires"" and others (check warnings when using data2SOMEC and importing a file in SOMEC)

- Check for a possible confusion between columns Activite, Comportement and VolEau and what's written in the SOMEC database at the end

- Watch out for possible bug in readxl:::xlsx_col_types where sheets seem to be numbered from 0 when in read_excel its from 1
