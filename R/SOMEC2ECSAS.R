# 1 - colonne InTransect
# 2 - nom Alpha en francais
# 3 - Dates manquantes	



SOMEC2ECSAS<-function(
	
	d="C:/Users/User/Documents/SCF2016_FR/ECSASdata/SOMEC-QC.accdb", # path to the access database
	file="C:/Users/User/Documents/SCF2016_FR/ECSASdata/outfile.csv", # name of the output file
	date="2014-01-01", # date after which data has to be uploaded
	step="5 min" # length of the transect bouts to be cut into
	
){
	
	# d = name of the QC database to connect to
	# watch_len = duration of watch blocks in minutes  
	
	seg_len<-function(x,names=c("LatStart","LongStart","LatEnd","LongEnd")){
		rad<-function(y){(y*pi)/180}
		r_lat_debut<-rad(x[,names[1]])
		r_long_debut<-rad(x[,names[2]])
		r_lat_fin<-rad(x[,names[3]])
		r_long_fin<-rad(x[,names[4]])
		#sapply(names,function(i){rad(x[,i])})
		longueur_km<-acos(sin(r_lat_debut)*sin(r_lat_fin)+cos(r_lat_debut)*cos(r_lat_fin)*cos(r_long_fin-r_long_debut))*6371
		longueur_km
	}
	
	
	
	split_transect<-function(d,step){
		x2<-d #contient tout
		g<-grep("deb|déb|fin",d[,"site"],ignore.case=TRUE)
		if(length(g)==1){
			g<-unique(c(max(grep("ini",d[,"site"],ignore.case=TRUE)),g)) #certains transects n'ont pas de début, mais 2 description initiale
		}
		x<-d[g,] #contient uniquement le début et la fin
		
		step_sec<-60*as.numeric(unlist(strsplit(step," "))[1])
		period<-as.POSIXct(x$date_heure,tz="GMT")
		temp<-seq(min(period),max(period)+step_sec,by=step,include.lowest=TRUE,right=TRUE) #attention au +step_sec
		#if(max(temp)<max(period)){
		#  temp<-c(temp,max(temp)+step_sec)               
		#}
		StartTime<-as.POSIXct(temp[-length(temp)],tz="GMT")
		EndTime<-as.POSIXct(temp[-1],tz="GMT") 
		n<-length(temp)
		val_lat<-with(x,seq(latitude[1],tail(latitude,1),length.out=n))
		val_lon<-with(x,seq(longitude[1],tail(longitude,1),length.out=n))
		LatStart<-val_lat[-n]
		LongStart<-val_lon[-n]
		LatEnd<-val_lat[-1]
		LongEnd<-val_lon[-1]
		res<-data.frame(StartTime,EndTime,LatStart,LongStart,LatEnd,LongEnd,stringsAsFactors=FALSE)
		res[,"WatchIDOrig"]<-paste(x$mission[1],res$StartTime,sep="_") #ID temporaire pour par la suite être transformé
		
		#val<-sapply(res[,"StartTime"],function(j){which.min(abs(as.POSIXct(x2$date_heure,tz="GMT")-j))[1]}) 
		val<-sapply(res[,"StartTime"],function(j){
			w<-which(as.POSIXct(x2$date_heure,tz="GMT")<=j) #on donne la valeur précédente la plus près
			if(any(w)){
				max(w)
			}else{
				NA
			}   
		}) 
		
		res[,"Visibility"]<-x2[,"visibilite"][val]
		res[,"SeaState"]<-x2[,"mer"][val]
		res[,"WindSpeed"]<-x2[,"vit_vent"][val]
		res[,"WindDir"]<-x2[,"dir_vent"][val]
		res[,"Swell"]<-x2[,"vagues"][val]
		res[,"PlatformActivity"]<-x2[,"act_plateforme"][val]
		res[,"PlatformSpeed"]<-x2[,"vit_plateforme"][val]
		res[,"PlatformDirection"]<-x2[,"dir_plateforme"][val]
		res[,"WatchNote"]<-x2[,"commentaire"][val]
		
		res[,"Start Date"]<-min(x[,"date"])
		res[,"End Date"]<-max(x[,"date"])
		res[,"PortStart"]<-NA
		res[,"PortEnd"]<-NA
		res[,"ObsLen"]<-step_sec/60
		res[,"WatchLenKm"]<-seg_len(res)
		res[,"ID"]<-NA
		
		y<-obs[which(obs$id_transect==x$id_transect[1] & obs$mission==x$mission[1]),]
		if(nrow(y)){
			val<-sapply(substr(y[,"date_heure"],1,19),function(j){
				w1<-which(substr(res[,"StartTime"],1,19)<=j)
				w2<-which(substr(res[,"EndTime"],1,19)>j)
				int<-intersect(w1,w2)
				if(!any(int)){
					NA
				}else{
					if(length(int)>1){
						NA
					}else{
						int
					}
				}
			}) 
			res2<-data.frame(WatchIDOrig=res$WatchIDOrig[val],y,stringsAsFactors=FALSE)
			res<-dplyr:::full_join(res2,res,type="full",by="WatchIDOrig")
			names(res)[names(res)=="latitude"]<-"ObsLat"
			names(res)[names(res)=="longitude"]<-"ObsLong"
			names(res)[names(res)=="heure"]<-"ObsTime"
			names(res)[names(res)=="commentaire"]<-"SightingNote"
			names(res)[names(res)=="code_espece"]<-"Alpha"
			names(res)[names(res)=="activite"]<-"FlySwim" #verfiiiffffffff
			names(res)[names(res)=="nb_individu"]<-"Count"
			names(res)[names(res)=="dis_par"]<-"Distance"
			names(res)[names(res)=="in_transect"]<-"InTransect"
			names(res)[names(res)=="commentaire"]<-"SightingNote"
			names(res)[names(res)=="code_obs"]<-"Observer"
			names(res)[names(res)=="date"]<-"Date"
			res[,"SightingIDOrig"]<-NA
		}else{
			res2<-data.frame(WatchIDOrig=res$WatchIDOrig[1],stringsAsFactors=FALSE)
			res<-join(res2,res,type="full")
			res[,c("ObsLat","ObsLong","ObsTime","WatchNote","SightingIDOrig","Alpha","FlySwim","Count","Distance","InTransect","SightingNote","Observer","Date")]<-NA
		}
		
		res[,"Alpha"]<-ifelse(is.na(res[,"Alpha"]),"RIEN",res[,"Alpha"])
		res[,"Date"]<-ifelse(is.na(res[,"Date"]),sort(as.character(res[,"Date"]))[1],as.character(res[,"Date"]))
		
		z<-mis[mis$mission==x$mission[1],]
		res[,"PlatformText"]<-z$nom_plateforme[1]
		res[,"CruiseMainObserver"]<-z$observateur1[1]
		res[,"CruiseNote"]<-NA
		res[,"CruiseIDOrig"]<-x[,"mission"][1]
		res<-res[,names(f2)]
		res
	}
	
	cat(paste("Reading",d,"database...\n\n",collapse=" "))
	db<-odbcConnectAccess2007(d)
	tran<-sqlFetch(db,"transects",stringsAsFactors=FALSE)
	obs<-sqlFetch(db,"observations",stringsAsFactors=FALSE)
	mis<-sqlFetch(db,"missions",stringsAsFactors=FALSE)
	sp<-sqlFetch(db,"Code espèces",stringsAsFactors=FALSE)
	odbcClose(db)
	
	# do it only on new 2014 2015 data (be careful GOR140228 not in new missions to add)
	tran<-tran[substr(tran$date,1,10)>=date,]
	obs<-obs[obs$mission%in%unique(tran$mission),]
	mis<-mis[mis$mission%in%unique(tran$mission),]
	
	#
	tran<-tran[!(is.na(tran$latitude) | is.na(tran$longitude)),]
	tran<-with(tran,tran[order(mission,id_transect,date,heure),])
	
	obs<-obs[!(is.na(obs$latitude) | is.na(obs$longitude)),]
	obs<-with(obs,obs[order(mission,id_transect,date,heure),])
	
	# temporary file to get end names
	file2<-"C:/Users/User/Documents/SCF2016_FR/ECSASconnect/Example input data.xls"
	sheets<-excel_sheets(file2)
	f2 <- read_excel(file2,sheet=sheets[1]) 
	
	l<-dlply(tran,.(mission,id_transect))
	cat(paste("Splitting",length(l),"transects\n\n",collapse=" "))
	l<-lapply(l,split_transect,step=step)
	#for(i in seq_along(l)){
	#  temp<-split_transect(l[[i]])
	#}
	ans<-do.call("rbind",l)
	ans<-with(ans,ans[order(CruiseIDOrig,StartTime,ObsTime),])
	ans[,"WatchIDOrig"]<-as.numeric(factor(ans[,"WatchIDOrig"]))
	ans[,"SightingIDOrig"]<-1:nrow(ans)
	ans[,"Alpha"]<-toupper(ans[,"Alpha"])
	CodeFR<-ans[,"Alpha"]
	ans[,"Alpha"]<-sp$CodeAN[match(ans[,"Alpha"],sp$CodeFR)]
	ans[,"Alpha"]<-ifelse(ans[,"Alpha"]%in%c("NOBI","RIEN"),"",ans[,"Alpha"])
	ans[,"Distance"]<-toupper(ans$Distance)
	ans[,"FlySwim"]<-ifelse(ans[,"FlySwim"]%in%c("EAU","Eau","Sur l'eau","eau"),"W",ans[,"FlySwim"])
	ans[,"FlySwim"]<-ifelse(ans[,"FlySwim"]%in%c("VOL","Vol","vol"),"F",ans[,"FlySwim"])
	ans[,"InTransect"]<-ifelse(ans[,"InTransect"]%in%c("En cours"),"Y",ifelse(!is.na(ans[,"InTransect"]),"N",ans[,"InTransect"]))
	ans[,"PlatformActivity"]<-ifelse(ans[,"PlatformActivity"]%in%c("EnDéplacement"),"Steaming",ans[,"PlatformActivity"])
	
	if(any(is.na(ans[,"Alpha"]))){
		cat("No matches for following Alpha codes:",paste(unique(CodeFR[is.na(ans[,"Alpha"])]),collapse=" "),"\n")
	}
	
	ans<-ddply(ans,.(CruiseIDOrig,WatchIDOrig),function(k){k[rev(order(k$Alpha)),]})  # reorder to elimnate empty observations when there are already observations.
	ans<-ans[!(duplicated(ans[,c("CruiseIDOrig","StartTime")]) & ans$Alpha%in%c("")),]
	
	row.names(ans)<-NULL
	
	
	
	
	
	###################
	### write data
	
	options(xlsx.date.format="yyyy-mm-dd")
	options(xlsx.datetime.format="yyyy-mm-dd h:mm:ss")
	#write.xlsx(ans,paste0(getwd(),"/test.xlsx"),sheetName="test",col.names=TRUE,row.names=FALSE,append=FALSE,showNA=FALSE)
	#write.csv(ans,paste0(getwd(),"/test.csv"))
	cat("Showing first 6 lines of output data:\n\n\n")
	head(ans)
	cat(paste("Writing outfile","\"",file,"\"","to",collapse=" "))
	write.table(ans,file,row.names=FALSE,sep=";",na="")
	ans
	
}
