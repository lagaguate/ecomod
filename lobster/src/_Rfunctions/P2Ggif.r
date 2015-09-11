P2Ggif<-function(logs,lfa='34',tail=7,pie.scale=500,wd=600,ht=800,...){
	
	require(animation)# for creating gif
	require(TeachingDemos) # for adding pie charts to map
	
	# Log data
	logs$julian<-julian(as.Date(logs$DATE_FISHED),origin=min(as.Date(logs$DATE_FISHED))-1)
	
	ports<-subset(read.csv(file.path( project.datadirectory("lobster"), "data","Ports.csv")),LFA==lfa)
	names(ports)[c(1,7,8)]<-c("COMMUNITY_CODE","Y1","X1")
	grids<-subset(read.csv(file.path( project.datadirectory("lobster"), "data","maps","lfa27_38_centgrid.csv")),LFA==lfa)
	grids$X2<-grids$CENTLON*-1
	grids$Y2<-grids$CENTLAT

	lpg<-subset(logs,select=c("COMMUNITY_CODE","GRID_NUM","julian","WEIGHT_KG"))
	lpg$PG<-paste(lpg$COMMUNITY_CODE,lpg$GRID_NUM,sep='.')
	rpg<-with(lpg,tapply(PG,PG,length))
	#lineSegs<-subset(lpg,!duplicated(PG))
	#lineSegs<-merge(merge(merge(lineSegs,subset(ports,X1<0&Y1>0,c("COMMUNITY_CODE","X1","Y1"))),subset(grids,select=c("GRID_NUM","X2","Y2"))),data.frame(PG=names(rpg),REPS=rpg))
	lineSegs<-merge(merge(lpg,subset(ports,X1<0&Y1>0,c("COMMUNITY_CODE","X1","Y1"))),subset(grids,select=c("GRID_NUM","X2","Y2")))

	
	LandSum<-sum(lpg$WEIGHT_KG,na.rm=T)
	

	### GIF animations ###
	## set some options first
	#oopt = ani.options(interval = 0.4, nmax = length(unique(logs$DATE_FISHED)), outdir=getwd())
	## use a loop to create images one by one
	#saveGIF({
	#for (i in 1:ani.options("nmax")) {
	par(ask=T)
	for (i in 1:length(unique(logs$DATE_FISHED))) {
	LobsterMap(lfa,...,title=min(as.Date(logs$DATE_FISHED))+i-1)
	with(subset(lineSegs,julian<=i&julian>i-tail),segments(X1,Y1,jitter(X2),jitter(Y2),col=rgb(0,0,0,0.3)))
	 
	 # Catch pie charts
	#LSF<-sum(subset(lpg,julian<i)$WEIGHT_KG)
	#subplot(pie(c(LandSum-LSF,LSF),labels=NA),-67.3,42.75,size=rep(pie.scale,2))

	 
	#ani.pause() ## pause for a while (’interval’)
	}
	#}, interval = 0.05, movie.name = "P2G.gif", ani.width = wd, ani.height = ht)
}


