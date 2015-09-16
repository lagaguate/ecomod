#	source("Y:/INSHORE SCALLOP/Survey/2010/r/fn/alloc.poly.r")

# ALLOCATION by STRATIFIED RANDOM DESIGN
# based on bottom type (or another polygon based non-continuous stratifying variable)

	
alloc.poly<-function(poly.lst,bounding.poly,ntows,mindist=1,pool.size=4,repeated.tows=NULL){
		
	
	options(warn=-1)
	# create pool of random points
	if(missing(ntows))ntows<-sum(poly.lst[[2]]$allocation)
	npool=ntows*pool.size
	  
	if(!missing(bounding.poly))surveyed.polys<-joinPolys(poly.lst[[1]],bounding.poly,operation="INT",maxVert=1e+06)
	
	if(missing(bounding.poly)){
	surveyed.polys<-poly.lst[[1]]
	bounding.poly<-poly.lst[[1]][chull(poly.lst[[1]]$X,poly.lst[[1]]$Y),]
		bounding.poly$POS<-1:nrow(bounding.poly)
		bounding.poly$SID<-1
	}
	attr(bounding.poly,"projection")<-"LL"
#	browser()
	pool.EventData<-genran(npool,bounding.poly,mindist=mindist)
	
	
	Poly.ID<-unique(poly.lst[[2]]$PID)
	strata<-as.character(unique(poly.lst[[2]]$PName))
	strataTows.lst<-list(NULL)
	
	# allocation provided
	if("allocation"%in%names(poly.lst[[2]])){
		towsi<-with(poly.lst[[2]],tapply(allocation,PName,unique))
		strataPolys.dat<-merge(surveyed.polys,subset(poly.lst[[2]],select=c("PID","PName")))
		attr(strataPolys.dat,"projection")<-"LL"
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
				strataArea[i]<-calcArea(strataPolys.lst[[i]],1)$area
				names(strataArea)[i]<-strata[i]
				print(strata[i])
			}
		}
		strataArea<-na.omit(strataArea)
		strataPolys.dat<-do.call("rbind",strataPolys.lst)
		towsi<-round(strataArea/sum(strataArea)*ntows)
		towsi<-towsi[towsi>0]
		towsi[2]<-ntows-sum(towsi[-2])
		strata<-names(towsi)
	}
	
	for(i in 1:length(strata)){
		LocSet<-findPolys(pool.EventData,subset(strataPolys.dat,PName==strata[i]))
		strataTows.lst[[i]]<-data.frame(subset(pool.EventData,EID%in%LocSet$EID)[1:towsi[strata[i]],c("EID","X","Y")],Poly.ID=Poly.ID[i],STRATA=strata[i])
	
	}
	
	Tows<-do.call("rbind",strataTows.lst)
	Tows$EID<-1:sum(towsi)
	rownames(Tows)<-1:sum(towsi)
	attr(Tows,"projection")<-"LL"
	
	# randomly selects stations from last years	survey (repeated.tows)
	if(!is.null(repeated.tows)){
			
		names(repeated.tows)<-c("EID","X","Y","Poly.ID")
		repeated.tows$EID<-repeated.tows$EID+1000
		repeat.str<-poly.lst[[2]][!is.na(poly.lst[[2]]$repeats),]
		repeated.lst<-list(NULL)
		tmp<-rbind(Tows[,-5],repeated.tows)
		attr(tmp,"projection")<-"LL"
		#browser()
		tmp$nndist<-nndist(as.ppp(subset(convUL(tmp),select=c('X','Y')),with(convUL(bounding.poly),owin(range(X),range(Y)))))
		repeated.tows<-subset(tmp,nndist>mindist&EID>1000,-5)
		for(i in 1:length(repeat.str$PID)){
			str.tows<-subset(repeated.tows,Poly.ID==repeat.str$PID[i])
			repeated.lst[[i]]<-str.tows[sample(1:nrow(str.tows),repeat.str$repeats[repeat.str$PID==repeat.str$PID[i]]),]
			repeated.lst[[i]]$STRATA<-repeat.str$PName[repeat.str$PID==repeat.str$PID[i]]
		}
		repeated.tows<-do.call("rbind",repeated.lst)
		
		Tows<-list(new.tows=Tows, repeated.tows=repeated.tows)
	}
	
	options(warn=0)
	return(list(Tows=Tows,Areas=strataArea))
	
}
