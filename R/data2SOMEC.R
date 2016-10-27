

data2SOMEC<-function(
	
	input,
	dbpath,
	prefix,
	old=FALSE
	
){	
	
	### helper functions
	decim<-function(x){
		y<-gsub(",",".",gsub("\"|N|o|O|W|N","",x))
		y<-strsplit(y,"°|'")
		y<-sapply(y,function(i){
			if(length(i)>1){
				i<-as.numeric(i)
				i[1]+i[2]/60+i[3]/3600
			}else{
				as.numeric(i)
			}
		})
		if(any(grep("W|w|O|o",x))){
			y<-(-1)*y
		}
		y #? v?rifier que tout est bien n?gatif dans les longitudes
	}
	
	fix_date<-function(x){
		g<-grep("-00",x) 
		if(any(g)){
			temp<-cumsum(as.numeric(!duplicated(x))) #on prend la valeur de date pr?c?dente qui n'est pas -00
			temp<-temp[match(x,x)]
			m<-match(temp[g]-1,temp)
			x[g]<-x[m]
		}
		x
	}
	
	codify_date<-function(x){
		res<-lapply(x,function(i){
			ans<-unlist(strsplit(i,"-"))
			ans<-substr(ans,nchar(ans)-1,nchar(ans))
			paste0(ans,collapse="")
		})
		unlist(res)  
	}
	
	get_dbvarTypes<-function(db,table){
		tmp<-sqlColumns(db,table)  
		varTypes<-tolower(as.character(tmp$TYPE_NAME))
		names(varTypes)<-as.character(tmp$COLUMN_NAME)
		varTypes
	}
	
	type_conv<-function(x,type){
		switch(toupper(type),
									VARCHAR=as.character(x),
									DOUBLE=as.numeric(x),
									DATETIME=as.character(x),
									INTEGER=as.numeric(x),
		)
	}
	
	
	sheets<-excel_sheets(input)
	tran <- as.data.frame(read_excel(input,sheet=sheets[agrep("transect",sheets,ignore.case=TRUE)][1]),stringsAsFactors=FALSE)
	obs <- as.data.frame(read_excel(input,sheet=sheets[agrep("observation",sheets,ignore.case=TRUE)][1]),stringsAsFactors=FALSE) 
	
	if(old){ #si les données proviennent de vieux fichiers excel
	  data(new_names)
	  names<-new_names
	  namesT<-names[names[,"table"]=="transects",]
	  namesO<-names[names[,"table"]=="observations",]
   	m<-match(names(tran),namesT[,"data_name"])
	  names(tran)<-namesT[,"new_name"][m]
  	m<-match(names(obs),namesO[,"data_name"])
	  names(obs)<-namesO[,"new_name"][m]
	}
	
	# si pas de id_transect, on supprime la ligne
	tran<-tran[!is.na(tran$id_transect),]
	obs<-obs[!is.na(obs$id_transect),]
	
	# si pas de longitude on scrap la ligne MAIS ? MODIFIER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	tran<-tran[!is.na(tran$longitude),]
	obs<-obs[!is.na(obs$longitude),]
	
	# si pas de donn?es de temps ou de lat lon on stop tout et on donne un message d'erreur
	col<-c("latitude","longitude","date","heure")
	manque1<-apply(tran[,col],2,function(i){any(is.na(i))})
	manque2<-apply(obs[,col],2,function(i){any(is.na(i))})
	if(any(c(manque1,manque2))){
		stop("Données de latitude/longitude/date/heure manquantes")
	}
	
	# Connect to Access db
	db<-odbcConnectAccess2007(dbpath)
	on.exit(odbcClose(db))
	Tran<-sqlFetch(db,"transects",stringsAsFactors=FALSE)
	Obs<-sqlFetch(db,"observations",stringsAsFactors=FALSE)
	Mis<-sqlFetch(db,"missions",stringsAsFactors=FALSE)
	#odbcClose(db)
	
	### Mission info
	nom_plateforme<-if(any(names(tran)=="nom_plateforme")){tran[,"nom_plateforme"][1]}else{NA}
	type_transect<-NA
	format_transect<-NA
	organisation<-if(any(names(tran)=="organisation")){tran[,"organisation"][1]}else{NA}
	
	
	### transects
	tran[,"mission"]<-paste0(prefix,codify_date(min(tran[,"date"],na.rm=TRUE)))
	tran[,"latitude"]<-decim(tran[,"latitude"])
	tran[,"longitude"]<-decim(tran[,"longitude"])
	tran[,"date"]<-fix_date(tran[,"date"])
	tran[,"date_heure"]<-paste(tran[,"date"],tran[,"heure"])
	tran[,"heure"]<-paste("1899-12-30",tran[,"heure"])
	tran[,"instantane"]<-as.numeric(gsub(" sec","",gsub("Non","999",tran[,"instantane"])))
	
	### observations
	obs[,"mission"]<-tran[,"mission"][1]
	obs[,"latitude"]<-decim(obs[,"latitude"])
	obs[,"longitude"]<-decim(obs[,"longitude"])
	obs[,"date"]<-fix_date(obs[,"date"])
	obs[,"date_heure"]<-paste(obs[,"date"],obs[,"heure"])
	obs[,"heure"]<-paste("1899-12-30",obs[,"heure"])
	obs[,"nb_individu"]<-as.numeric(obs[,"nb_individu"]) #parfois il y a des lettres dans les valeurs (e.g. ligne 1215 dans HudsonJuin2014)
	obs[,"id"]<-as.integer(seq(range(Obs[,"id"])[2]+1,length.out=nrow(obs)))
	
	if(any(Tran$mission==tran$mission[1])){
		 stop(paste("Mission name",tran$mission[1],"already in the database"))
	}
	
	difftran<-setdiff(names(Tran),names(tran)) #on regarde ce qU,on peut aller cherche comme information manquante dans l'autre table
	diffobs<-setdiff(names(Obs),names(obs))
	int<-intersect(diffobs,names(tran))
	
	
	if(length(int)>0){    #on rajoute ce qui manque d'important dans la table des observations (en 2016 le code_obs et cote_obs s'il y a plus d'une valeur on ne sait pas trop comment attribuer et plus compliqué
	  for(i in seq_along(int)){ 
	    r<-unique(tran[,int[i]])
	    r<-r[!is.na(r)]
	    if(length(r)==1){
	      obs[,int[i]]<-r
	    }else{
	      obs[,int[i]]<-NA
	    }
	  }
	  diffobs<-diffobs[!diffobs%in%int]
	}
	

	if(length(difftran)){
	  tran[,difftran]<-NA	
	  warning(paste("Added empty columns (",paste(difftran,collapse=", "),") to transects table to fit with",input))
	}

	varTypes<-get_dbvarTypes(db,"transects")
	wd<-which(varTypes=="double")
	for(i in seq_along(wd)){
		tran[,names(wd)[i]]<-as.numeric(gsub(",",".",tran[,names(wd)[i]]))
	}
	
	tran<-tran[,names(Tran)]
	sqlSave(db,tran,tablename="transects",append=TRUE,rownames=FALSE,addPK=FALSE,fast=TRUE,colnames=FALSE,varTypes=varTypes)
	cat(paste(nrow(tran),"lines added to the transects table"),"\n")
	
	if(length(diffobs)){
		obs[,diffobs]<-NA	
		warning(paste("Added empty columns (",paste(diffobs,collapse=", "),") to observations table to fit with",input))
	}

	varTypes<-get_dbvarTypes(db,"observations")
	wd<-which(varTypes=="double")
	for(i in seq_along(wd)){
		obs[,names(wd)[i]]<-as.numeric(gsub(",",".",obs[,names(wd)[i]]))
	}
	obs<-obs[,names(Obs)]
	

	### Convert column type in obs to fit with access
	cl<-sapply(Obs,class)
	cl<-sapply(seq_along(cl),function(i){ifelse(length(cl[[i]])==1,cl[[i]],"character")})
	w<-which(sapply(obs,class)!=cl)
	for(i in w){
	  obs[,i]<-eval(parse(text=paste0("as.",cl[i],"(obs[,i])")))
	}
	
	sqlSave(db,obs,tablename="observations",append=TRUE,rownames=FALSE,addPK=FALSE,fast=TRUE,colnames=FALSE,varTypes=varTypes)
	cat(paste(nrow(obs),"lines added to the observations table"),"\n")
	

	### missions
	mission<-tran[,"mission"][1]
	debut<-range(obs[,"date"])[1]
	fin<-range(obs[,"date"])[2]
	tab<-rev(table(obs[,"code_obs"]))
	observateur1<-ifelse(length(tab)==0,NA,names(tab)[1])
	observateur2<-if(length(tab)>1){paste(names(tab)[-1],collapse=", ")}else{NA}
	
	mis<-data.frame(mission,debut,fin,nom_plateforme,observateur1,observateur2,type_transect,format_transect,organisation,stringsAsFactors=FALSE)
	names(mis)<-names(Mis)
	
	varTypes<-get_dbvarTypes(db,"missions")
	wd<-which(varTypes=="double")
	for(i in seq_along(wd)){
		mis[,names(wd)[i]]<-as.numeric(gsub(",",".",mis[,names(wd)[i]]))
	}
	
	sqlSave(db,mis,tablename="missions",append=TRUE,rownames=FALSE,addPK=FALSE,fast=TRUE,colnames=FALSE,varTypes=varTypes)
	cat(paste(nrow(mis),"lines added to the missions table"),"\n")
	
}



###############
################
################
#################
##################
##################
###################


