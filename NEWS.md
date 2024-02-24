## Version 0.6.9 ### Bug fixes:
- specifying "species" argument to ECSAS.extract() caused a malformed SQL clause that
contained a condition for every row in SpececiesInfo that had NA Alpha field
- specifying more than one species (ie. species = c("DOVE", "TBMU")) in ECSAS.extract() caused an
error message claiming only one species could be specified, contrary to the
docs.
### New features:
- added "include.empty.watches" (default TRUE) argument to ECSAS.extract(). When TRUE,
there is a single row in the returned dataframe for each watch where the
specified species was not observed. When FALSE, watches with no observations
(of the specified species) are not returned.