ECSASconnect
=======
Import data from the Eastern Canada Seabirds at Sea (ECSAS) database

**For CWS computers, Java must be installed from the allowed program first for necessary package rJava to work.**

**For InGeo computers, set the path to Java in the ArcGIS folder for rJava to work:**

`Sys.setenv(JAVA_HOME = "C:/Program Files (x86)/ArcGIS/Desktop10.2/java/jre")`


Version 0.6.2
=======
* Added several new functions for importing new data to the QC SOMEC access database and exporting this data to the ECSAS database. 
* Added the possibility to extract all or some databases from ECSAS.

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
