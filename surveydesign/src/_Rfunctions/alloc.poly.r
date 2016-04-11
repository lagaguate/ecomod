#	source("Y:/INSHORE SCALLOP/Survey/2010/r/fn/alloc.poly.r")

# ALLOCATION by STRATIFIED RANDOM DESIGN
# based on bottom type (or another polygon based non-continuous stratifying variable)

	
alloc.poly<-function(poly.lst,bounding.poly,ntows,mindist=1,pool.size=4,repeated.tows=NULL, map=NULL,lplace='bottomleft',show.pool=F,UTMzone){
		
	
	options(warn=-1)
	# create pool of random points
	if(missing(ntows))ntows<-sum(poly.lst[[2]]$allocation)
	npool=ntows*pool.size
	  
	if(!missing(bounding.poly))surveyed.polys<-joinPolys(poly.lst[[1]],bounding.poly,operation="INT")
	
	if(missing(bounding.poly)){
	surveyed.polys<-poly.lst[[1]]
	bounding.poly<-poly.lst[[1]][chull(poly.lst[[1]]$X,poly.lst[[1]]$Y),]
		bounding.poly$POS<-1:nrow(bounding.poly)
		bounding.poly$SID<-1
	}
	attr(bounding.poly,"projection")<-"LL"
	if(!missing(UTMzone))attr(bounding.poly,"zone")<-UTMzone
#	browser()

	# start with a pool of random stations
	pool.EventData<-genran(npool,bounding.poly,mindist=mindist)
	
	
	Poly.ID<-unique(poly.lst[[2]]$PID)
	strata<-as.character(unique(poly.lst[[2]]$PName))
	strataTows.lst<-list(NULL)
	
	# allocation provided
	if("allocation"%in%names(poly.lst[[2]])){
		towsi<-with(poly.lst[[2]],tapply(allocation,PName,unique))
		strataPolys.dat<-merge(surveyed.polys,subset(poly.lst[[2]],select=c("PID","PName")))
		attr(strataPolys.dat,"projection")<-"LL"
		if(!missing(UTMzone))attr(strataPolys.dat,"zone")<-UTMzone
		strataArea<-calcArea(strataPolys.dat,1)
	}
	else{
		# calculate area and proportional allocation
		strataPolys.lst<-list(NULL)
		strataArea<-c()
		towsi<-c()
	
		for(i in 1:length(strata)){
			strataPIDS<-poly.lst[[2]]$PID[poly.lst[[2]]$PName==strata[i]]
			tmp<-subset(surveyed.polys,PID%in%strataPIDS)
			if(nrow(tmp)>0){
				tmp$PName<-strata[i]
				strataPolys.lst[[i]]<-combinePolys(tmp)
				attr(strataPolys.lst[[i]],"projection")<-"LL"
				if(!missing(UTMzone))attr(strataPolys.lst[[i]],"zone")<-UTMzone
				strataArea[i]<-calcArea(strataPolys.lst[[i]],1)$area
				names(strataArea)[i]<-strata[i]
				print(strata[i])
			}
		}
		strataArea<-na.omit(strataArea)
		strataPolys.dat<-do.call("rbind",strataPolys.lst)
		towsi<-round(strataArea/sum(strataArea)*ntows)
		towsi<-towsi[towsi>0]
		towsi[1]<-ntows-sum(towsi[-1])
		strata<-names(towsi)
	}
	#browser()
	for(i in 1:length(strata)){
		LocSet<-findPolys(pool.EventData,subset(strataPolys.dat,PName==strata[i]))
		strataTows.lst[[i]]<-data.frame(subset(pool.EventData,EID%in%LocSet$EID),Poly.ID=Poly.ID[i],STRATA=strata[i])
	
	}
	
	Tows<-do.call("rbind",strataTows.lst)
	Tows$EID<-1:nrow(Tows)
	rownames(Tows)<-1:nrow(Tows)
	attr(Tows,"projection")<-"LL"
	if(!missing(UTMzone))attr(Tows,"zone")<-UTMzone
	
	# randomly selects stations from last years	survey (repeated.tows)
	if(!is.null(repeated.tows)){
			
		names(repeated.tows)<-c("EID","X","Y","Poly.ID")
		repeated.tows$EID<-repeated.tows$EID+1000
		repeat.str<-poly.lst[[2]][!is.na(poly.lst[[2]]$repeats),]
		repeated.lst<-list(NULL)
		#browser()
		for(i in 1:length(repeat.str$PID)){
			str.tows<-subset(repeated.tows,Poly.ID==repeat.str$PID[i])
			repeated.lst[[i]]<-str.tows[sample(1:nrow(str.tows),repeat.str$repeats[repeat.str$PID==repeat.str$PID[i]]),]
			repeated.lst[[i]]$STRATA<-repeat.str$PName[repeat.str$PID==repeat.str$PID[i]]
		}
		repeated.tows<-do.call("rbind",repeated.lst)
		tmp<-rbind(Tows[,-4],repeated.tows)
		attr(tmp,"projection")<-"LL"
		if(!missing(UTMzone))attr(tmp,"zone")<-UTMzone
		tmp$nndist<-nndist(as.ppp(subset(convUL(tmp),select=c('X','Y')),with(convUL(bounding.poly),owin(range(X),range(Y)))))
		Tows<-subset(tmp,nndist>mindist&EID<1000)
		
	}
	
	Tows.lst<-list(NULL)
	strata<-as.character(unique(poly.lst[[2]]$PName))
	#browser()
	for(i in 1:length(strata)){
		Tows.lst[[i]]<-subset(Tows,STRATA==strata[i])[1:towsi[strata[i]],]
	
	}
	Tows<-do.call("rbind",Tows.lst)
	Tows$EID<-1:nrow(Tows)
	rownames(Tows)<-1:nrow(Tows)
	attr(Tows,"projection")<-"LL"
	if(!missing(UTMzone))attr(Tows,"zone")<-UTMzone
			
	if(!is.null(repeated.tows))Tows<-list(new.tows=Tows, repeated.tows=repeated.tows)

	if(!is.null(map)){
		loadfunctions('lobster')
		LobsterMap(map,poly.lst=list(surveyed.polys,poly.lst[[2]]))
		bg.col<-tapply(poly.lst[[2]]$col,poly.lst[[2]]$PName,unique)
		if(is.null(repeated.tows))addPoints(Tows,pch=21, cex=1,bg=bg.col[as.character(Tows$STRATA)])
		if(!is.null(repeated.tows)){
			addPoints(Tows$new.tows,pch=21, cex=1,bg=bg.col[as.character(Tows$new.tows$STRATA)])
			addPoints(Tows$repeated.tows,pch=24, cex=1,bg=bg.col[as.character(Tows$repeated.tows$STRATA[order(Tows$repeated.tows$EID)])])
		}
		if(show.pool)addPoints(pool.EventData,pch=4,cex=0.4)
#		browser()
		if(!is.null(repeated.tows))legend(lplace,legend=names(bg.col[unique(as.character(Tows$new.tows$STRATA))]),pch=21,pt.bg=bg.col[unique(as.character(Tows$new.tows$STRATA))],bty='n',cex=1, inset = .02)
		if(is.null(repeated.tows))legend(lplace,legend=names(bg.col[unique(as.character(Tows$STRATA))]),pch=21,pt.bg=bg.col[unique(as.character(Tows$STRATA))],bty='n',cex=1, inset = .02)

	}
	
	options(warn=0)
	return(list(Tows=Tows,Areas=strataArea))
	
}
