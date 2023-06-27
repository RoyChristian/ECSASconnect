load("C:/Users/fifieldd/OneDrive - EC-EC/Projects/Atlantic dsm/R/Rdata/aerialtest.Rda")
load("C:/Users/fifieldd/OneDrive - EC-EC/Projects/Atlantic dsm/R/Rdata/gpsdat.Rda")


debug(ECSAS.create.aerial.watches)
# debug(ECSASconnect:::create.aerial.watch.by.transect)
# debug(ECSASconnect:::expand.pr)
# debug(ECSASconnect:::create.chunk.watches)
ECSAS.create.aerial.watches(ecsas.air.dat, 30, gps.dat, 
  "C:/Users/fifieldd/OneDrive - EC-EC/Projects/Atlantic dsm/Data/Master_ECSAS_v_3.69.mdb",
  verbose = T)
