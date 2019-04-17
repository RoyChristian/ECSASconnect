if (FALSE) {

## 07Jul2015
## read in and merge data from aerial surveys
require(ggplot2)
require(lubridate)
require(plyr)
require(rworldmap)
require(rworldxtra)
require(mapproj)
#setwd("~/projects/surveys")

## read in files of compiled data

############################################################
## bird data
dat <- read.csv ("sightings_20170419.csv", stringsAsFactors=F, na.strings="na")
dat$ts <- with(dat, ymd_hms(paste(year, mon, day, hour, min, sec), tz = "UTC"))
dat$observer <- as.factor(dat$observer)
dat$side <- as.factor(dat$side)
dat$species <- as.factor(dat$species)
dat$fly_water <- as.factor(dat$fly_water)
dat$distance <- as.factor(dat$distance)
str(dat)
dat <- subset(dat, select = -c(year,mon,day,hour,min,sec,start_end))
head(dat)
subset(dat, is.na(ts)==T)
length(unique(dat$species))
subset(dat, species == "GRSH ")
#remove sharp-shinned hawk sighting, 1 sighting
dat <- subset(dat, species != "SSHA")
#correct some species names
dat$species[dat$species == "GRSH "] <- "GRSH"
dat$species[dat$species == "GARB "] <- "GARB"
dat$species[dat$species == "HERG "] <- "HERG"
dat$species[dat$species == "NOGA "] <- "NOGA"
dat$species[dat$species == "UNLA "] <- "UNLA"
dat$species[dat$species == "UHPH "] <- "UNPH"
dat$species[dat$species == "seal"] <- "SEAL"
dat$species[dat$species == "SEAL "] <- "SEAL"

dat$sex[dat$sex == "Male"] <- "male"
dat$age[dat$association == "immature"] <- "immature"
dat$association[dat$association == "immature"] <- ""
dat$observer[dat$observer == "Ronconi"] <- "ronconi"
dat$observer[dat$observer == "Gjerdrum"] <- "gjerdrum"
dat$observer[dat$observer == "Gjerdrum "] <- "gjerdrum"
dat$side[dat$side == "Port"] <- "port"
dat$side[dat$side == "Starboard"] <- "starboard"
dat$fly_water[dat$fly_water == "F"] <- "f"
dat$age[dat$age == "adult "] <- "adult"
dat$age[dat$age == "adults with breeding plumage"] <- "adult"
dat$age[dat$age == "adult; over the water ss=3-4"] <- "adult"
dat$age[dat$age == "Juvenile"] <- "immature"
subset(dat, age == "Juvenile")
unique(dat$age)
levels(dat$species)
dat$species <- factor(dat$species)

###################################################
## transect start_end data, plus pauses
wpt <- read.csv ("start.end_20170419.csv", stringsAsFactors=F, na.strings="na")
wpt$ts <- with(wpt, ymd_hms(paste(year, mon, day, hour, min, sec), tz = "UTC"))
wpt <- subset(wpt, select = c(survey.id, Route, Transect.ID,start_end,ts))
wpt <- rename(wpt, c("start_end"="start.end"))	
unique(wpt$start.end)
wpt$start.end[wpt$start.end == "end "] <- "end"

str(wpt)
str(dat)

#####################
## gps data
# trk <- read.csv ("razo_20150217.csv", stringsAsFactors=FALSE)
# str(trk)
# 
# trk$Longitude <- -1*trk$Longitude
# trk$ts <- with(trk, ymd_hms(paste(YYYY, MM, DD, hh, mm, ss.s), tz = "UTC"))
# trk <- subset(trk, select = c(Latitude, Longitude, ts))
# trk <- rename(trk, c("Latitude"="lat", "Longitude"="lon", "ts"="ts"))
# str(trk)
# subset(trk, is.na(Latitude))

## list of all GPS files
path <- "./GPStracks"
fn <- dir(path=path, full.names=TRUE)
#ids <- sub(path, "", fn)
#ids <- sub("g.txt", "", ids)
## subset out just the 'METADATA' files and .csv files
fn <- fn[grep(".csv", fn)]
fn

read.gps.files <- function(filename) {
	trk <- read.csv(filename, header = T )
	trk <- trk[1:length(trk$Latitude)-1,] #remove last row (contains text)
	trk$Latitude <- as.numeric(as.character(trk$Latitude))
	trk$Longitude <- -1*trk$Longitude
	trk$ts <- with(trk, ymd_hms(paste(YYYY, MM, DD, hh, mm, ss.s), tz = "UTC"))
	trk <- subset(trk, select = c(Latitude, Longitude, ts))
	trk <- rename(trk, c("Latitude"="lat", "Longitude"="lon", "ts"="ts"))
	trk <- trk[-which(diff(trk$ts)==0), ] #remove duplicate records
	start <- min(trk$ts)
	end <- max(trk$ts)
	#get sequence of 1 sec time intervals
	seq <- seq(from = start, to = end , by = "secs")
	seq <- data.frame(seq)
	## interpolate for all time stamps
  seq$lat <- approx(trk$ts, trk$lat, xout = seq$seq, method="linear", rule = 2)$y
	seq$lon <- approx(trk$ts, trk$lon, xout = seq$seq, method="linear", rule = 2)$y
  seq <- rename(seq, c("seq"="ts"))
  return(seq)
}

## read in and combine all of the files
gps <- do.call("rbind", lapply(fn, read.gps.files))
#strange steps needed to make ts work
gps$ts.2 <- as.character(gps$ts)
gps$ts <- as.POSIXct(gps$ts.2, format = "%Y-%m-%d %H:%M:%S")
gps <- subset(gps, select = -c(ts.2))
gps$date <- as.Date(gps$ts)
head(gps)


p <- ggplot(data=subset(gps, date %in% as.Date("2015-09-16")), 
            aes(ts, lon, col = as.factor(date)))
p + geom_path() #+ geom_point() 



##check in plot
## get world map
newmap <- getMap(resolution = "high")  # different resolutions available
# subset world map if needed
Fundy.df <- subset(newmap.df, long > -67.5 & long < -64 & lat > 43 & lat < 46.5)
GM.df <- subset(newmap.df, long > -67.25 & long < -66.5 & lat > 44 & lat < 44.85)


## check plot of track
Fundy.df <- fortify(Fundy.df)
GM.df <- fortify(GM.df)

str(Fundy.df)
p <- ggplot(data=Fundy.df, aes(long, lat))
p + geom_path(aes(group=group)) + 
  coord_map() + 
  xlab("Longitude") + 
  ylab("Latitude") +
  theme_bw() +
  geom_path(data=gps, aes(lon, lat, col=as.factor(date)), shape=1, size=0.5) 
  

#######MERGE GPS DATA TO SIGHTINGS AND START.END################
## append lat/lon to count data
tmp <- merge(dat, gps, by = "ts", all.x=T, all.y=F)  ## without the zero data
subset(tmp, is.na(lat)==T)
tmp[3230:3250,]
dups <-which(diff(tmp$ts)==0)
dups
#add a unique identifier for each survey and transect ID
tmp$sur.tr.id <- paste(tmp$survey.id, ".", tmp$Transect.ID, sep = "")

str(Fundy.df)
p <- ggplot(data=Fundy.df, aes(long, lat))
p + geom_path(aes(group=group)) + 
  coord_map() + 
  xlab("Longitude") + 
  ylab("Latitude") +
  theme_bw() +
  geom_point(data=subset(tmp, 
  	Route %in% c("fundy.2","fundy.4") & #"GM.shoals",
  	species %in% c("MURA","RAZO")), 
  	aes(lon, lat, col=as.factor(date)), shape=1, size=0.5) 

p <- ggplot(data=GM.df, aes(long, lat))
p + geom_path(aes(group=group)) + 
  coord_map() + 
  xlab("Longitude") + 
  ylab("Latitude") +
  theme_bw() +
  geom_point(data=subset(tmp, 
  	Route %in% c("GM.shoals","fundy.2","fundy.4") &
  	species %in% c("MURA","RAZO") &lon > -67.25 & lon < -66.5 & lat > 44.25 & lat < 44.75), 
  	aes(lon, lat, col=as.factor(date), size = count), shape=1) 
#lon > -67.5 & lon < -66 & lat > 44 & lat < 45.5

str(tmp)
#save the sightings data
write.csv(tmp, file = "sightings.gps.20170419.csv")
saveRDS(tmp, "sightings.gps.20170419.rds")
tmp <- readRDS("sightings.gps.20170419.rds")
#save winter Fundy surveys
write.csv(subset(tmp, 
  	Route %in% c("GM.shoals","fundy.2","fundy.4", "SaintMarysBay")),
  	#species %in% c("MURA","RAZO")),
	file = "Fundy.winter.20170419.csv")

data=subset(tmp, 
  	Route %in% c("GM.shoals","fundy.2","fundy.4", "SaintMarysBay") &
  	species %in% c("MURA","RAZO"))
str(tmp)
unique(tmp$Route)
#subset for phalarope
phal <- subset(tmp, Route %in% c("lurcher.shoals", "BR.ledges", "GM.ledges"))
with(phal, table(species,survey.id))
write.csv(phal, file = "sightings.phal.20161220.csv")
saveRDS(phal, "sightings.phal.20161220.rds")

#subset for Fundy surveys
fund <- subset(tmp, Route %in% c("GM.shoals","fundy.2","fundy.4", "SaintMarysBay"))
fund$species <- droplevels(fund$species) 
with(fund, table(species))
#waterfowl, excluding geese
with(subset(fund, species %in% c('ABDU','SUSC','COEI',"BLSC",'BUFF','LTDU','UNDU','UNSC','WWSC','UNLO','COLO')), table(droplevels(species)))
#alcids
with(subset(fund, species %in% c('ALCI','ATPU','BLGU','DOVE','COMU','MURA','UNMU')), table(droplevels(species)))
#gulls
with(subset(fund, species %in% c('BLKI','GBBG','HERG','GLGU','ICGU','UNGU','UNLA','WWGU')), table(droplevels(species)))

#alcids
with(subset(fund, Transect.ID %in% c('7ex','8ex','9ex','10ex') & species %in% c('BLKI','GBBG','HERG','GLGU','ICGU','UNGU','UNLA','WWGU')), table(droplevels(species), Transect.ID))


write.csv(fund, file = "sightings.fundy.20170419.csv")
saveRDS(fund, "sightings.fundy.20170419.rds")

## check proportion of sightings in different distance classes
phal$plane[phal$survey.id %in% c( "20150901.lurcher.shoals","20150902.BR.ledges","20150902.GM.ledges" )] <- "part"
phal$plane[phal$survey.id %in% c( "20150915.GM.ledges", "20150915.BR.ledges", "20150916.lurcher.shoals")] <- "isla"
phal$plane.side <- paste(phal$plane, ".", phal$side, sep = "")
head(phal)

phal$survey.side <- paste(phal$survey.id, ".", phal$side, sep = "")

with(subset(phal, species != "GARB"), round(prop.table(table(plane.side,distance),1), digits=2))
with(subset(phal, species != "GARB"), round(prop.table(table(survey.side,distance),1), digits=2))

with(subset(phal, species != "GARB"), round(prop.table(table(observer,distance),1), digits=2))
with(subset(tmp, species != "GARB"), round(prop.table(table(observer,distance),1), digits=2))


## append lat/lon to start.end data
tmp <- merge(wpt, gps, by = "ts", all.x=T, all.y=F)  ## without the zero data
subset(tmp, is.na(lat)==T)
tmp
dups <-which(diff(tmp$ts)==0)
dups
str(wpt)

p <- ggplot(data=Fundy.df, aes(long, lat))
p + geom_path(aes(group=group)) + 
  coord_map() + 
  xlab("Longitude") + 
  ylab("Latitude") +
  theme_bw() +
  geom_point(data=tmp, aes(lon, lat, col=as.factor(start.end)), shape=1, size=0.5) 
str(tmp)

#add a unique identifier for each survey and transect ID
tmp$sur.tr.id <- paste(tmp$survey.id, ".", tmp$Transect.ID, sep = "")
start <- subset(tmp, start.end == "start")
	start <- subset(start, select = c(survey.id, Route, Transect.ID, ts,lat,lon,sur.tr.id))
	start <- rename(start, c("ts"="start.ts", "lat"="start.lat", "lon"="start.lon"))
end <- subset(tmp, start.end == "end")
	end <- subset(end, select = c(ts,lat,lon,sur.tr.id))
	end <- rename(end, c("ts"="end.ts", "lat"="end.lat", "lon"="end.lon"))
start.end <- merge(start, end, by = "sur.tr.id", all.x=T, all.y=F)
start.end
head(start.end)
str(start.end)
#measure transect length
require(argosfilter)
start.end$length.km <- with(start.end , distance(start.lat, end.lat, start.lon, end.lon)) 
start.end$length.km <- round(start.end$length.km, digits=3) #round to nearest m
head(start.end)
#subset(start.end, Route == "lurcher.shoals")

## get avg line length for a survey:
tmp <- ddply(subset(phal), .(survey.id), summarize, 
	mean.length = round(mean(length.km),digits=1),
	SD.length = round(sd(length.km),digits=1),
	min.length = round(min(length.km),digits=1),
	max.length = round(max(length.km),digits=1),
	n.lines = round(length(length.km),digits=0),
	total.length = round(sum(length.km),digits=1))
tmp

tmp <- ddply(subset(fundy, survey.id %in% c('20160314.fundy.2','20160330.fundy.4')), .(Route), summarize, #
	mean.length = round(mean(length.km),digits=1),
	SD.length = round(sd(length.km),digits=1),
	min.length = round(min(length.km),digits=1),
	max.length = round(max(length.km),digits=1),
	n.lines = round(length(length.km),digits=1),
	total.length = round(sum(length.km),digits=1))
tmp

with(subset(fundy, survey.id %in% c('20160314.fundy.2','20160330.fundy.4')), sd(length.km))
#save the gps files
write.csv(gps, file = "gps.20170419.csv")
saveRDS(gps, "gps.20170419.rds")
#save the transect start.end
write.csv(start.end, file = "start.end.gps.20170419.csv")
saveRDS(start.end, "start.end.gps.20170419.rds")
#subset phalarope data
phal <- subset(start.end, Route %in% c("lurcher.shoals", "BR.ledges", "GM.ledges"))
with(phal, table(Transect.ID,survey.id))
write.csv(phal, file = "start.end.phal.20161220.csv")
saveRDS(phal, "start.end.phal.20161220.rds")

#subset Fundy data
fundy <- subset(start.end, Route %in% c("GM.shoals","fundy.2","fundy.4", "SaintMarysBay"))
with(fundy, table(Transect.ID,survey.id))
write.csv(fundy, file = "start.end.fundy.20170419.csv")
saveRDS(fundy, "start.end.fundy.20170419.rds")


str(tmp)
test <- readRDS("gps.20161216.rds")
write.csv(tmp, file = "birds.csv")

start.end

p <- ggplot(data=start.end, aes(Transect.ID, length.km))
p + geom_point() + facet_wrap(~Route, ncol=1, scale = "free")







##get RAZO data for geoAviR
#rz <- subset(tmp, species=="MURA")
##exclude the big clusters for distance analysis
rz <- subset(tmp, count < 19)

rz <- tmp
with(rz, table(count, species))
rz$distance <- as.character(rz$distance) 
rz$distance[rz$distance == "a"] <- "A"
rz$distance[rz$distance == "b"] <- "B"
rz$distance[rz$distance == "c"] <- "C"
rz$distance[rz$distance == "z"] <- "D"
rz <- subset(rz, select = c(Transect.ID, distance, kilometers, lat, lon, species, ts, count))
str(rz)
with(rz, table(Transect.ID, species))
rz <- rename(rz, c("Transect.ID"="transect.id", "ts"="Date"))


write.table(rz, file = "razo.all.csv", quote = F, sep = ";", dec = ".")


##Plot data
# subset world map if needed
GM.df <- subset(newmap.df, long > -67.5 & long < -66 & lat > 44.25 & lat < 44.75)

## check plot of track
GM.df <- fortify(GM.df)

str(GM.df)
p <- ggplot(data=GM.df, aes(long, lat))
p + geom_path(aes(group=group)) + 
  coord_map() + 
  xlab("Longitude") + 
  ylab("Latitude") +
  theme_bw() +
  geom_point(data=seq, aes(lon, lat), shape=1, col="black", size=0.2)+
  geom_point(data=subset(tmp, count < 20 & !(species %in% c("GARB","SEAL","UNKN"))),
              aes(lon, lat, colour = factor(species), size = count)) +
  scale_size_area()

}