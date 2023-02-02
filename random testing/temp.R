load("C:/Users/fifieldd/OneDrive - EC-EC/Projects/Atlantic dsm/R/Rdata/aerialtest.Rda")
debug(ECSAS.create.aerial.watches)
debug(ECSASconnect:::create.aerial.watch.by.transect)
ECSAS.create.aerial.watches(ecsas.air.dat, 30, NULL, 
  "C:/Users/fifieldd/OneDrive - EC-EC/Projects/Atlantic dsm/Data/Master_ECSAS_v_3.69.mdb")
